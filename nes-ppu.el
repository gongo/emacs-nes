;; -*- lexical-binding: t -*-

(require 'gamegrid)
(require 'nes-util)
(require 'nes-color)
(require 'nes-interrupt)

(defconst nes/ppu:SCREEN-WIDTH 256)
(defconst nes/ppu:SCREEN-HEIGHT 240)

(defconst nes/ppu:TILE-WIDTH 8)
(defconst nes/ppu:TILE-HEIGHT 8)
(defconst nes/ppu:TILES-COUNT-IN-HORIZONTAL-LINE (/ nes/ppu:SCREEN-WIDTH nes/ppu:TILE-WIDTH))
(defconst nes/ppu:TILES-COUNT-IN-VERTICAL-LINE   (/ nes/ppu:SCREEN-HEIGHT nes/ppu:TILE-HEIGHT))

(defconst nes/ppu:BLOCK-WIDTH 16)
(defconst nes/ppu:BLOCK-HEIGHT 16)

;;
;; Each byte of attribute table controls the palette of a 4x4 tile part of the name table.
;;
;; https://wiki.nesdev.com/w/index.php/PPU_attribute_tables
;;
(defconst nes/ppu:ENCODED-ATTRIBUTE-WIDTH (* nes/ppu:TILE-WIDTH 4))
(defconst nes/ppu:ENCODED-ATTRIBUTE-HEIGHT (* nes/ppu:TILE-HEIGHT 4))
(defconst nes/ppu:ENCODED-ATTRIBUTE-COUNT-IN-HORIZONTAL-LINE (/ nes/ppu:SCREEN-WIDTH nes/ppu:ENCODED-ATTRIBUTE-WIDTH))
(defconst nes/ppu:ENCODED-ATTRIBUTE-COUNT-IN-VERTICAL-LINE   (/ nes/ppu:SCREEN-WIDTH nes/ppu:ENCODED-ATTRIBUTE-HEIGHT))

(defconst nes/ppu:CYCLES-PER-LINE 341)
(defconst nes/ppu:VBLANK-HEIGHT 22)

(defconst nes/ppu:SPRITE-RAM-BYTESIZE #x0100)

;;
;; This library does not use gamegrid-set-face(10)
;;
;; Explain it using the following example.
;;
;;   (defvar sample-10-options
;;     '(((glyph colorize)
;;        (t ?.))
;;       ((color-x color-x)
;;        (mono-x grid-x)
;;        (color-tty color-tty))
;;       (((glyph color-x) [1 0 0])
;;        (color-tty "red"))))
;;
;;   (defun sample-display-options ()
;;     (let ((options (make-vector 256 nil)))
;;       (dotimes (c 256)
;;         (aset options c
;;               (cond ((= c 1)
;;                      sample-1-options)
;;                     ((= c 2)
;;                      sample-2-options)
;;                     ;;
;;                     ;; skip
;;                     ;;
;;                     ((= c 10)
;;                      sample-10-options)
;;                     (t '(nil nil nil)))))
;;       options))
;;
;;   (gamegrid-init (sample-display-options))
;;
;;   1. gamegrid creates a dedicated display-table ( `bufefer-display-table' )
;;
;;   2. In the case of the above example:
;;
;;        (aref buffer-display-table 10) ;; => [46] ( ?. )
;;
;;   3. This means, 10 (C-j) replaces 46 (.).
;;
;;   4. As a result, screen collapse!!!
;;
;;        before
;;
;;          **********
;;          **********
;;          **********
;;          **********
;;
;;        after
;;
;;          **********.**********.**********.**********
;;
;;
(defconst nes/ppu:COLOR-START-OFFSET 11)

(defstruct (nes/image
            (:conc-name nes/image->))
  (x 0)
  (y 0)
  (c 0))

(defstruct (nes/ppu-bus
            (:conc-name nes/ppu-bus->))
  (video-ram (make-vector #x2000 0))
  character-ram ;; vector
  )

(defstruct (nes/ppu
            (:conc-name nes/ppu->))
  (cycle 0)
  (line 0)
  (bus (make-nes/ppu-bus))
  (sprite-ram (make-vector nes/ppu:SPRITE-RAM-BYTESIZE 0))
  (sprite-ram-addr #x00)
  (scroll-x 0)
  (scroll-y 0)
  (interrupt nil)
  (images (make-vector (* nes/ppu:SCREEN-WIDTH nes/ppu:SCREEN-HEIGHT) -1))
  (draw-queue '())

  ;; PPU registers
  (ppuctrl 0)
  (ppumask 0)
  (ppustatus 0)
  (oamaddr 0)
  (oamdata 0)
  (ppuscroll 0)
  (ppuaddr 0)
  (ppudata 0)

  ;; PPU internal registers
  ;; https://wiki.nesdev.com/w/index.php/PPU_scrolling#PPU_internal_registers
  (v 0)
  (t 0)
  (x 0)
  (w 0)
  )

(defun nes/ppu--bus-read (b addr)
  (when (nes/ppu-p b)
    (setq b (nes/ppu->bus b)))
  (cond
   ((<= addr #x1FFF) (aref (nes/ppu-bus->character-ram b) addr))

   ((<= addr #x27FF) (aref (nes/ppu-bus->video-ram b) (- addr #x2000)))  ;; nametable 0 - 1
   ((<= addr #x2FFF) (nes/ppu--bus-read b (- addr #x0800)))              ;; nametable 2 - 3 (mirror to 0 - 1)
   ((<= addr #x3EFF) (nes/ppu--bus-read b (- addr #x1000)))              ;; mirror to #x2000 - #x2EFF
   ((or (eq addr #x3F04)
        (eq addr #x3F08)
        (eq addr #x3F0C)) (nes/ppu--bus-read b #x3F00))                  ;; Use #x3F00 because these are unique data and not used by PPU
   ((or (eq addr #x3F10)
        (eq addr #x3F14)
        (eq addr #x3F18)
        (eq addr #x3F1C)) (nes/ppu--bus-read b (- addr #x0010)))         ;; mirror to #x3F00, #x3F04, #x3F08 and #x3F0C
   ((<= addr #x3F1F) (aref (nes/ppu-bus->video-ram b) (- addr #x2000)))
   ((<= addr #x3FFF) (nes/ppu--bus-read b (- addr #x0020)))              ;; mirror to #x3F00 - #x3F1F
   ;; (t 0)
   ))

(defun nes/ppu--bus-write (b addr value)
  (when (nes/ppu-p b)
    (setq b (nes/ppu->bus b)))
  (cond
   ((<= addr #x1FFF) (aset (nes/ppu-bus->character-ram b) addr value))
   ((<= addr #x27FF) (aset (nes/ppu-bus->video-ram b) (- addr #x2000) value))
   ((<= addr #x2FFF) (nes/ppu--bus-write b (- addr #x0800) value))
   ((<= addr #x3EFF) (nes/ppu--bus-write b (- addr #x1000) value))               ;; mirror to #x2000 - #x2EFF
   ((or (eq addr #x3F10)
        (eq addr #x3F14)
        (eq addr #x3F18)
        (eq addr #x3F1C)) (nes/ppu--bus-write b (- addr #x0010) value))          ;; mirror to #x3F00, #x3F04, #x3F08 and #x3F0C
   ((<= addr #x3F1F) (aset (nes/ppu-bus->video-ram b) (- addr #x2000) value))
   ((<= addr #x3FFF) (nes/ppu--bus-write b (- addr #x0020) value))               ;; mirror to #x3F00 - #x3F1F
   ;; (t 0)
   ))

(defun nes/ppu-read (p addr)
  (case addr
    (#x2002 (nes/ppu--read-status p))
    (#x2004 (nes/ppu--read-oam-data p))
    (#x2007 (nes/ppu--read-data p))
    (t 0)
    ))

(defun nes/ppu-write (p addr value)
  (case addr
    (#x2000 (nes/ppu--write-control p value))
    (#x2001 (nes/ppu--write-mask p value))
    (#x2003 (nes/ppu--write-oam-address p value))
    (#x2004 (nes/ppu--write-oam-data p value))
    (#x2005 (nes/ppu--write-scroll p value))
    (#x2006 (nes/ppu--write-address p value))
    (#x2007 (nes/ppu--write-data p value))
    ;;(t 0)
    ))

;;
;; https://wiki.nesdev.com/w/index.php/PPU_scrolling#Register_controls
;;
;; $2000 write
;;
(defsubst nes/ppu--write-control (ppu value)
  (setf (nes/ppu->ppuctrl ppu) value)
  ;; t: ...BA.. ........ = d: ......BA
  (setf (nes/ppu->t ppu) (logior (logand (nes/ppu->t ppu) #xF3FF)
                                 (lsh (logand value #x03) 10))))

;;
;; https://wiki.nesdev.com/w/index.php/PPU_registers
;;
;; $2001 write
;;
(defsubst nes/ppu--write-mask (ppu value)
  (setf (nes/ppu->ppumask p) value))

;;
;; https://wiki.nesdev.com/w/index.php/PPU_registers
;; https://wiki.nesdev.com/w/index.php/PPU_scrolling#Register_controls
;;
;; $2002 read
;;
(defsubst nes/ppu--read-status (ppu)
  (let ((data (nes/ppu->ppustatus ppu)))
    (nes/ppu--clear-vblank p)
    (setf (nes/ppu->w ppu) nil)
    data))

;;
;; https://wiki.nesdev.com/w/index.php/PPU_registers
;;
;; $2003 write
;;
(defsubst nes/ppu--write-oam-address (ppu value)
  (setf (nes/ppu->sprite-ram-addr ppu) value))

;;
;; https://wiki.nesdev.com/w/index.php/PPU_registers
;;
;; $2004 read
;;
(defsubst nes/ppu--read-oam-data (ppu)
  (nes/ppu--read-from-sprite-ram ppu (nes/ppu->sprite-ram-addr ppu)))

;;
;; https://wiki.nesdev.com/w/index.php/PPU_registers
;;
;; $2004 write
;;
(defsubst nes/ppu--write-oam-data (ppu value)
  (aset (nes/ppu->sprite-ram ppu) (nes/ppu->sprite-ram-addr ppu) value)
  (incf (nes/ppu->sprite-ram-addr ppu)))

;;
;; https://wiki.nesdev.com/w/index.php/PPU_scrolling#Register_controls
;;
;; $2005 write
;;
(defsubst nes/ppu--write-scroll (ppu value)
  (if (nes/ppu->w ppu)
      (progn
        ;;
        ;; t: CBA..HG FED..... = d: HGFEDCBA
        ;; w:                  = 0
        ;;
        (setf (nes/ppu->t ppu) (logior (logand (nes/ppu->t ppu) #x8FFF)
                                       (lsh (logand value #x07) 12)))
        (setf (nes/ppu->t ppu) (logior (logand (nes/ppu->t ppu) #xFC1F)
                                       (lsh (logand value #xF8) 2)))
        (setf (nes/ppu->w ppu) nil))
    (progn
      ;;
      ;; t: ....... ...HGFED = d: HGFED...
      ;; x:              CBA = d: .....CBA
      ;; w:                  = 1
      ;;
      (setf (nes/ppu->t ppu) (logior (logand (nes/ppu->t ppu) #xFFE0)
                                     (lsh (logand value #xFF) -3)))
      (setf (nes/ppu->x ppu) (logand value #x07))
      (setf (nes/ppu->w ppu) t))))

;;
;; https://wiki.nesdev.com/w/index.php/PPU_scrolling#Register_controls
;;
;; $2006 write
;;
(defsubst nes/ppu--write-address (ppu value)
  (if (nes/ppu->w ppu)
      (progn
        ;;
        ;; t: ....... HGFEDCBA = d: HGFEDCBA
        ;; v                   = t
        ;; w:                  = 0
        ;;
        (setf (nes/ppu->t ppu) (logior (logand (nes/ppu->t ppu) #xFF00)
                                       (logand value #xFF)))
        (setf (nes/ppu->v ppu) (nes/ppu->t ppu))
        (setf (nes/ppu->w ppu) nil))
    (progn
      ;;
      ;; t: .FEDCBA ........ = d: ..FEDCBA
      ;; t: X...... ........ = 0
      ;; w:                  = 1
      ;;
      (setf (nes/ppu->t ppu) (logior (logand (nes/ppu->t ppu) #x80FF)
                                     (ash (logand value #x3F) 8)))
      (setf (nes/ppu->t ppu) (mod (nes/ppu->t ppu) #x6000))
      (setf (nes/ppu->w ppu) t))))

;;
;; https://wiki.nesdev.com/w/index.php/PPU_registers
;; https://wiki.nesdev.com/w/index.php/PPU_scrolling#Register_controls
;;
;; $2007 read
;;
(defsubst nes/ppu--read-data (ppu)
  (nes/ppu--bus-read (nes/ppu->bus p) (nes/ppu->v p))
  (incf (nes/ppu->v p) (nes/ppu--video-ram-addr-offset p))))

;;
;; https://wiki.nesdev.com/w/index.php/PPU_registers
;;
;; $2007 write
;;
(defsubst nes/ppu--write-data (ppu value)
  (nes/ppu--bus-write (nes/ppu->bus ppu) (nes/ppu->v ppu) value)
  (incf (nes/ppu->v ppu) (nes/ppu--video-ram-addr-offset ppu)))

(defun nes/ppu--read-from-character-data (p index)
  (nes/ppu--bus-read (nes/ppu->bus p) index))

(defun nes/ppu--read-from-sprite-ram (p index)
  (aref (nes/ppu->sprite-ram p) index))

(defun nes/ppu--read-from-sprite-pattern-table (p index)
  (let ((offset (nes/ppu--sprite-pattern-table-addr-offset p)))
    (nes/ppu--bus-read (nes/ppu->bus p) (+ offset index))))

(defun nes/ppu--read-from-background-palette-table (p index)
  (nes/ppu--bus-read (nes/ppu->bus p) (+ #x3F00 index)))

(defun nes/ppu--read-from-sprite-palette-table (p index)
  (nes/ppu--bus-read (nes/ppu->bus p) (+ #x3F10 index)))

(defun nes/ppu-step (ppu)
  (when (and (nes/ppu--on-visible-cycle-p ppu)
             (nes/ppu--on-build-background-position-p ppu))
    (if (nes/ppu--background-enabled-p ppu)
        (nes/ppu--build-background-tile ppu)
      (nes/ppu--build-0-background-tile ppu)))
  (if (nes/ppu--on-right-end-cycle-p ppu)
      (progn
        (setf (nes/ppu->cycle ppu) 0)
        (if (nes/ppu--on-bottom-end-line-p ppu)
            (progn
              (setf (nes/ppu->line ppu) 0)
              (nes/interrupt-deassert-nmi (nes/ppu->interrupt ppu))
              (nes/ppu--clear-vblank ppu)
              (nes/ppu--clear-sprite-hit ppu)
              (when (nes/ppu--sprite-enabled-p ppu)
                (nes/ppu--build-sprites ppu))
              (nes/ppu--render ppu)
              )
          (progn
            (incf (nes/ppu->line ppu))
            (when (nes/ppu--on-line-to-start-vblank-p ppu)
              (nes/ppu--set-vblank ppu)
              (when (nes/ppu--vblank-interrupt-enabled-p ppu)
                (nes/interrupt-assert-nmi (nes/ppu->interrupt ppu)))
              )
            )
          ))
    (incf (nes/ppu->cycle ppu))))

(defun nes/ppu--on-visible-cycle-p (ppu)
  (and (< -1 (nes/ppu--current-x ppu) nes/ppu:SCREEN-WIDTH)
       (< -1 (nes/ppu--current-y ppu) nes/ppu:SCREEN-HEIGHT)))

(defun nes/ppu--on-right-end-cycle-p (ppu)
  (eq (nes/ppu->cycle ppu) (1- nes/ppu:CYCLES-PER-LINE)))

(defun nes/ppu--on-bottom-end-line-p (ppu)
  (eq (nes/ppu->line ppu) (+ nes/ppu:SCREEN-HEIGHT nes/ppu:VBLANK-HEIGHT)))

(defun nes/ppu--on-build-background-position-p (ppu)
  (and (zerop (nes/ppu--current-x-in-tile ppu))
       (zerop (nes/ppu--current-y-in-tile ppu))))

(defun nes/ppu--on-line-to-start-vblank-p (ppu)
  (eq (nes/ppu->line ppu) nes/ppu:SCREEN-HEIGHT))

;;
;; PPU pattern tables
;; https://wiki.nesdev.com/w/index.php/PPU_pattern_tables
;;
(defun nes/ppu--build-background-tile (p)
  (let* ((start-x (nes/ppu--current-x p))
         (start-y (nes/ppu--current-y p))
         (character-id (nes/ppu--background-character-id-at p start-x start-y)))
    (dotimes (j nes/ppu:TILE-HEIGHT)
      (let* ((character-address (+ (* nes/ppu:TILE-HEIGHT 2 character-id)
                                   j
                                   (nes/ppu--background-table-addr-offset p)))
             (character-pattern-1-bits (nes/ppu--read-from-character-data p character-address))
             (character-pattern-2-bits (nes/ppu--read-from-character-data p (+ character-address 8))))
        (dotimes (i nes/ppu:TILE-WIDTH)
          (let* ((x (+ start-x i))
                 (y (+ start-y j))
                 (pattern-bit (- nes/ppu:TILE-WIDTH 1 i))
                 (palette-id (nes/ppu--background-palette-id-at p x y))
                 (palette-offset (+ (if (nes--logbitp pattern-bit character-pattern-1-bits)
                                        #b01 #b00)
                                    (if (nes--logbitp pattern-bit character-pattern-2-bits)
                                        #b10 #b00)
                                    (lsh palette-id 2))))
            (nes/ppu--image-write p
                                  x
                                  y
                                  (nes/ppu--read-from-background-palette-table p palette-offset))))
        ))))

(defun nes/ppu--build-0-background-tile (p)
  (let* ((start-x  (nes/ppu--current-x p))
         (start-y  (nes/ppu--current-y p))
         (color-id (nes/ppu--read-from-background-palette-table p 0)))
    (dotimes (j nes/ppu:TILE-HEIGHT)
      (dotimes (i nes/ppu:TILE-WIDTH)
        (nes/ppu--image-write p (+ start-x i) (+ start-y j) color-id)))))

(defun nes/ppu--build-sprites (ppu)
  (dotimes (i 64)
    (let* ((base-addr (* i 4))
           ;;
           ;; https://wiki.nesdev.com/w/index.php/PPU_OAM
           ;;
           (sprite-y         (nes/ppu--read-from-sprite-ram ppu base-addr))
           (sprite-tile      (nes/ppu--read-from-sprite-ram ppu (+ base-addr 1)))
           (sprite-attribute (nes/ppu--read-from-sprite-ram ppu (+ base-addr 2)))
           (sprite-x         (nes/ppu--read-from-sprite-ram ppu (+ base-addr 3)))

           (palette-id              (logand sprite-attribute #b00000011))
           (sprite-priority-p       (not (nes--logbitp 5 sprite-attribute)))
           (reversed-horizontally-p (nes--logbitp 6 sprite-attribute))
           (reversed-vertically-p   (nes--logbitp 7 sprite-attribute))
           )
      (when sprite-priority-p
        (dotimes (y nes/ppu:TILE-HEIGHT)
          (let* ((sprite-pattern-address (+ (* nes/ppu:TILE-HEIGHT 2 sprite-tile) y))
                 (sprite-pattern-1-bits (nes/ppu--read-from-sprite-pattern-table ppu sprite-pattern-address))
                 (sprite-pattern-2-bits (nes/ppu--read-from-sprite-pattern-table ppu (+ sprite-pattern-address nes/ppu:TILE-HEIGHT)))
                 )
            (dotimes (x nes/ppu:TILE-WIDTH)
              (let* ((index-in-pattern (- nes/ppu:TILE-WIDTH 1 x))
                     (palette-offset (logior (if (nes--logbitp index-in-pattern sprite-pattern-1-bits)
                                                 #b01 #b00)
                                             (if (nes--logbitp index-in-pattern sprite-pattern-2-bits)
                                                 #b10 #b00)
                                             (lsh palette-id 2))))
                (when (/= (mod palette-offset 4) 0)
                  (when reversed-vertically-p
                    (setq y (- nes/ppu:TILE-HEIGHT 1 y)))
                  (when reversed-horizontally-p
                    (setq x (- nes/ppu:TILE-WIDTH 1 x)))
                  (when (and (< 0 (+ sprite-x x) nes/ppu:SCREEN-WIDTH)
                             (< 0 (+ sprite-y y) nes/ppu:SCREEN-HEIGHT))
                    (nes/ppu--image-write ppu
                                          (+ sprite-x x)
                                          (+ sprite-y y)
                                          (nes/ppu--read-from-sprite-palette-table ppu palette-offset))))
                ))))))))

(defun nes/ppu--image-write (ppu x y color-id)
  (let ((index (+ x (* y nes/ppu:SCREEN-WIDTH))))
    (aset (nes/ppu->images ppu) index color-id)))

(defun nes/ppu--render (ppu)
  (let ((images (nes/ppu->images ppu)))
    (dotimes (y nes/ppu:SCREEN-HEIGHT)
      (dotimes (x nes/ppu:SCREEN-WIDTH)
        (let ((index (+ x (* y nes/ppu:SCREEN-WIDTH))))
          (nes/ppu--render-set-cell x y (aref images index)))))))

;;
;; How to obtain the palette id corresponding to the specified (x, y)
;;
;;   1. Fetch the attribute corresponding to the specified coordinates from attribute tables
;;
;;      see: https://wiki.nesdev.com/w/index.php/PPU_attribute_tables
;;
;;   2. Determine the block (that units constituting 16 x 16 pixels) ID of the specified (x, y)
;;
;;        0     16  32  48  64   208 224 240 256 (x)
;;          +---+---+---+---+-...-+---+---+---+
;;          | 0 | 1 | 0 | 1 | ... | 1 | 0 | 1 |
;;       16 +---+---+---+---+-...-+---+---+---+
;;          | 2 | 3 | 2 | 3 | ... | 3 | 2 | 3 |
;;       32 +---+---+---+---+-...-+---+---+---+
;;          | 0 | 1 | 0 | 1 | ... | 1 | 0 | 1 |
;;       48 +---+---+---+---+-...-+---+---+---+
;;          |   .   .   .   .  .  .   .   .   |
;;          .   .   .   .   .  .  .   .   .   .
;;          |   .   .   .   .  .  .   .   .   |
;;      192 +---+---+---+---+-...-+---+---+---+
;;          | 2 | 3 | 2 | 3 | ... | 3 | 2 | 3 |
;;      208 +---+---+---+---+-...-+---+---+---+
;;          | 0 | 1 | 0 | 1 | ... | 1 | 0 | 1 |
;;      224 +---+---+---+---+-...-+---+---+---+
;;          | 2 | 3 | 2 | 3 | ... | 3 | 2 | 3 |
;;      240 +---+---+---+---+-...-+---+---+---+
;;      (y)
;;
;;   3. Get palette id corresponding to block id from attribute
;;
;;     e.g.
;;
;;        block-id = 3
;;       attribute = #b11100100
;;
;;                  |
;;                  V
;;
;;             11/10/01/00
;;
;;                  |
;;                  V
;;
;;         block-id |  3 |  2 |  1 |  0
;;                  | -- | -- | -- | --
;;       palette-id | 11 | 10 | 01 | 00
;;
;;                  |
;;                  V
;;
;;       palette-id => #b11
;;
(defun nes/ppu--background-palette-id-at (ppu x y)
  (let* ((attribute (nes/ppu--attribute-at ppu x y))
         (block-id  (logior (if (eq (mod x nes/ppu:BLOCK-WIDTH) 1)  #b01 #b00)
                            (if (eq (mod y nes/ppu:BLOCK-HEIGHT) 1) #b10 #b00))))
    (logand (lsh attribute (- (* block-id 2))) #b11)))

(defun nes/ppu--video-ram-addr-offset (ppu)
  (if (nes--logbitp 2 (nes/ppu->ppuctrl ppu)) 32 1))

(defun nes/ppu--background-table-addr-offset (ppu)
  (if (nes--logbitp 4 (nes/ppu->ppuctrl ppu)) #x1000 #x0000))

(defun nes/ppu--sprite-pattern-table-addr-offset (ppu)
  (if (nes--logbitp 3 (nes/ppu->ppuctrl ppu)) #x1000 #x0000))

(defun nes/ppu--set-vblank (ppu)
  (setf (nes/ppu->ppustatus ppu) (logior (nes/ppu->ppustatus ppu) #x80)))

(defun nes/ppu--clear-vblank (ppu)
  (setf (nes/ppu->ppustatus ppu) (logand (nes/ppu->ppustatus ppu) #x7F)))

(defun nes/ppu--vblank-p (ppu)
  (nes--logbitp 7 (nes/ppu->ppustatus ppu)))

(defun nes/ppu--clear-sprite-hit (ppu)
  (setf (nes/ppu->ppustatus ppu) (logand (nes/ppu->ppustatus ppu) #xDF)))

(defun nes/ppu--background-enabled-p (ppu)
  (nes--logbitp 3 (nes/ppu->ppumask ppu)))

(defun nes/ppu--sprite-enabled-p (ppu)
  (nes--logbitp 4 (nes/ppu->ppumask ppu)))

(defun nes/ppu--vblank-interrupt-enabled-p (ppu)
  (nes--logbitp 7 (nes/ppu->ppuctrl ppu)))

(defun nes/ppu--background-character-id-at (ppu x y)
  (let ((address (nes/ppu--name-table-address-at ppu x y)))
    (nes/ppu--bus-read ppu address)))

(defun nes/ppu--attribute-at (ppu x y)
  (let ((address (nes/ppu--attribute-table-address-at ppu x y)))
    (nes/ppu--bus-read ppu address)))

;;
;; Name table id (address)
;;
;;                              (tile-x)
;;      0            32           64
;;    0 +------------+------------+
;;      |            |            |
;;      | 0 (0x2000) | 1 (0x2400) |
;;      |            |            |
;;   30 +------------+------------+
;;      |            |            |
;;      | 2 (0x2800) | 3 (0x2C00) |
;;      |            |            |
;;   60 +------------+------------+
;; (tile-y)
;;
(defun nes/ppu--name-table-address-at (p x y)
  (let* ((tile-x        (/ (+ x (nes/ppu->scroll-x p)) nes/ppu:TILE-WIDTH))
         (tile-y        (/ (+ y (nes/ppu->scroll-y p)) nes/ppu:TILE-HEIGHT))
         (tile-id       (+ (* (mod tile-y nes/ppu:TILES-COUNT-IN-VERTICAL-LINE)
                              nes/ppu:TILES-COUNT-IN-HORIZONTAL-LINE)
                           (mod tile-x nes/ppu:TILES-COUNT-IN-HORIZONTAL-LINE)))
         (name-table-id (+ (/ tile-x nes/ppu:TILES-COUNT-IN-HORIZONTAL-LINE)
                           (* (/ tile-y nes/ppu:TILES-COUNT-IN-VERTICAL-LINE) 2)))
         )
    (+ #x2000 (* name-table-id #x0400) tile-id)))

;;
;; Attribute table id (address)
;;
;;                              (tile-x)
;;      0            32           64
;;    0 +------------+------------+
;;      |            |            |
;;      | 0 (0x23C0) | 1 (0x27C0) |
;;      |            |            |
;;   30 +------------+------------+
;;      |            |            |
;;      | 2 (0x2BC0) | 3 (0x2FC0) |
;;      |            |            |
;;   60 +------------+------------+
;; (tile-y)
;;
(defun nes/ppu--attribute-table-address-at (p x y)
  (let* ((enc-attr-x    (/ (+ x (nes/ppu->scroll-x p)) nes/ppu:ENCODED-ATTRIBUTE-WIDTH))
         (enc-attr-y    (/ (+ y (nes/ppu->scroll-y p)) nes/ppu:ENCODED-ATTRIBUTE-HEIGHT))
         (enc-attr-id   (+ (* (mod enc-attr-y nes/ppu:ENCODED-ATTRIBUTE-COUNT-IN-VERTICAL-LINE)
                              nes/ppu:ENCODED-ATTRIBUTE-COUNT-IN-HORIZONTAL-LINE)
                           (mod enc-attr-x nes/ppu:ENCODED-ATTRIBUTE-COUNT-IN-HORIZONTAL-LINE)))
         (attr-table-id (+ (/ enc-attr-x nes/ppu:ENCODED-ATTRIBUTE-COUNT-IN-HORIZONTAL-LINE)
                           (* (/ enc-attr-y nes/ppu:ENCODED-ATTRIBUTE-COUNT-IN-VERTICAL-LINE) 2)))
         )
    (+ #x23C0 (* attr-table-id #x0400) enc-attr-id)))

(defsubst nes/ppu--current-x (p)
  (1- (nes/ppu->cycle p)))

(defun nes/ppu--current-x-in-tile (p)
  (mod (nes/ppu--current-x p) nes/ppu:TILE-WIDTH))

(defsubst nes/ppu--current-y (p)
  (nes/ppu->line p))

(defun nes/ppu--current-y-in-tile (p)
  (mod (nes/ppu--current-y p) nes/ppu:TILE-HEIGHT))

(defun nes/ppu--display-options ()
  (let ((options (make-vector 256 '(nil nil nil))))
    (dotimes (i (length nes/colors))
      (aset options (+ i nes/ppu:COLOR-START-OFFSET) (aref nes/colors i)))
    options))

(defun nes/ppu--render-set-cell (x y c)
  "see `gamegrid-set-cell'

In this library, `insert-char' and `delete-char' are not necessary in set-cell.
Because all grid uses only `? '
"
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (1+ (gamegrid-cell-offset x y)))
      (gamegrid-set-face (+ c nes/ppu:COLOR-START-OFFSET)))))

(defun nes/ppu-init (buffer-name)
  (select-window (or (get-buffer-window buffer-name)
                     (selected-window)))
  (switch-to-buffer buffer-name)
  (setq show-trailing-whitespace nil)
  (setq gamegrid-use-color t)
  (setq gamegrid-use-glyphs nil)
  (gamegrid-init-buffer nes/ppu:SCREEN-WIDTH
                        nes/ppu:SCREEN-HEIGHT
                        ? )
  (gamegrid-init (nes/ppu--display-options)))

(defun nes/ppu-set-character-ram (ppu ram)
  (setf (nes/ppu-bus->character-ram (nes/ppu->bus ppu)) ram))

(defun nes/ppu-transfer-sprite-data (ppu index value)
  (let ((address (mod (+ (nes/ppu->sprite-ram-addr ppu) index) nes/ppu:SPRITE-RAM-BYTESIZE)))
    (aset (nes/ppu->sprite-ram ppu) address value)))

(provide 'nes-ppu)
