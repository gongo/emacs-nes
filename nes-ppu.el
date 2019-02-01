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
(defconst nes/ppu:TILES-COUNT-IN-VERTICAL-LINE (/ nes/ppu:SCREEN-HEIGHT nes/ppu:TILE-HEIGHT))

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
(defconst nes/ppu:ENCODED-ATTRIBUTE-COUNT-IN-VERTICAL-LINE (/ nes/ppu:SCREEN-WIDTH nes/ppu:ENCODED-ATTRIBUTE-HEIGHT))

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

  ;; Background temporary variables
  (name-table-byte 0)
  (attribute-table-byte 0)
  (tile-low-byte 0)
  (tile-hi-byte 0)
  (tile-data1 0)
  (tile-data2 0)

  ;; Sprite temporary variables
  (sprite-count 0)
  (sprite-patterns (make-vector 8 0))
  (sprite-positions (make-vector 8 0))
  (sprite-priorities (make-vector 8 0))
  (sprite-indexes (make-vector 8 0))

  ;; PPU registers
  (ppuctrl 0)
  (ppumask 0)
  (ppustatus 0)

  ;; PPU internal registers
  ;; https://wiki.nesdev.com/w/index.php/PPU_scrolling#PPU_internal_registers
  (v 0)
  (temp 0) ;; (setting-constant t)
  (x 0)
  (w nil)
  (f 0)

  ;; use $2007 PPUDATA
  (buffered-data 0)

  ;; NMI flags
  (nmi-previous nil)
  (nmi-delay 0)
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
  (setq value (logand #xFF value))
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
  (nes/ppu--nmi-change ppu)
  ;; t: ...BA.. ........ = d: ......BA
  (setf (nes/ppu->temp ppu) (logior (logand (nes/ppu->temp ppu) #xF3FF)
                                    (lsh (logand value #x03) 10))))

;;
;; https://wiki.nesdev.com/w/index.php/PPU_registers
;;
;; $2001 write
;;
(defsubst nes/ppu--write-mask (ppu value)
  (setf (nes/ppu->ppumask ppu) value))

;;
;; https://wiki.nesdev.com/w/index.php/PPU_registers
;; https://wiki.nesdev.com/w/index.php/PPU_scrolling#Register_controls
;;
;; $2002 read
;;
(defsubst nes/ppu--read-status (ppu)
  (let ((data (nes/ppu->ppustatus ppu)))
    (nes/ppu--clear-vblank ppu)
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
  (let ((addr (nes/ppu->sprite-ram-addr ppu)))
    (aset (nes/ppu->sprite-ram ppu) addr value)
    (setf (nes/ppu->sprite-ram-addr ppu) (logand (incf addr) #xFF))))

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
        (setf (nes/ppu->temp ppu) (logior (logand (nes/ppu->temp ppu) #x8FFF)
                                          (lsh (logand value #x07) 12)))
        (setf (nes/ppu->temp ppu) (logior (logand (nes/ppu->temp ppu) #xFC1F)
                                          (lsh (logand value #xF8) 2)))
        (setf (nes/ppu->w ppu) nil))
    (progn
      ;;
      ;; t: ....... ...HGFED = d: HGFED...
      ;; x:              CBA = d: .....CBA
      ;; w:                  = 1
      ;;
      (setf (nes/ppu->temp ppu) (logior (logand (nes/ppu->temp ppu) #xFFE0)
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
        (setf (nes/ppu->temp ppu) (logior (logand (nes/ppu->temp ppu) #xFF00)
                                          (logand value #xFF)))
        (setf (nes/ppu->v ppu) (nes/ppu->temp ppu))
        (setf (nes/ppu->w ppu) nil))
    (progn
      ;;
      ;; t: .FEDCBA ........ = d: ..FEDCBA
      ;; t: X...... ........ = 0
      ;; w:                  = 1
      ;;
      (setf (nes/ppu->temp ppu) (logior (logand (nes/ppu->temp ppu) #x80FF)
                                        (lsh (logand value #x3F) 8)))
      (setf (nes/ppu->w ppu) t))))

;;
;; https://wiki.nesdev.com/w/index.php/PPU_registers
;; https://wiki.nesdev.com/w/index.php/PPU_scrolling#Register_controls
;;
;; $2007 read
;;
(defsubst nes/ppu--read-data (ppu)
  (let* ((vram-address (nes/ppu->v ppu))
         (bus (nes/ppu->bus ppu))
         (read-value (nes/ppu--bus-read bus vram-address))
         (return-value 0))
    (if (< (% vram-address #x4000) #x3F00)
        (progn
          (setq return-value (nes/ppu->buffered-data ppu))
          (setf (nes/ppu->buffered-data ppu) read-value))
      (progn
        (setq return-value read-value)
        (setf (nes/ppu->buffered-data ppu) (nes/ppu--bus-read bus (- vram-address #x1000)))))

    (incf (nes/ppu->v ppu) (nes/ppu--vram-address-increment-offset ppu))
    return-value))

;;
;; https://wiki.nesdev.com/w/index.php/PPU_registers
;;
;; $2007 write
;;
(defsubst nes/ppu--write-data (ppu value)
  (nes/ppu--bus-write ppu (nes/ppu->v ppu) value)
  (incf (nes/ppu->v ppu) (nes/ppu--vram-address-increment-offset ppu)))

(defun nes/ppu--read-from-sprite-ram (p index)
  (aref (nes/ppu->sprite-ram p) index))

(defun nes/ppu--read-from-palette-table (p index)
  (nes/ppu--bus-read (nes/ppu->bus p) (+ #x3F00 index)))

;;
;; https://wiki.nesdev.com/w/index.php/PPU_rendering#Line-by-line_timing
;;
(defun nes/ppu-step (ppu)
  (nes/ppu--tick ppu)

  (let* ((rendering-enabled-p (or (nes/ppu--background-enabled-p ppu)
                                  (nes/ppu--sprite-enabled-p ppu)))
         (scanline (nes/ppu->line ppu))
         (pre-render-line-p (eq scanline 261))
         (visible-line-p (< scanline 240))
         (render-line-p (or pre-render-line-p visible-line-p))

         (cycle (nes/ppu->cycle ppu))
         (pre-fetch-cycle-p (<= 321 cycle 336))
         (visible-cycle-p (<= 1 cycle 256))
         (fetch-cycle-p (or pre-fetch-cycle-p visible-cycle-p))
         )

    (when rendering-enabled-p
      ;;
      ;; background logic
      ;;
      (when (and visible-line-p visible-cycle-p)
        (nes/ppu--render-pixel ppu))

      (when (and render-line-p fetch-cycle-p)
        (setf (nes/ppu->tile-data2 ppu) (logior (logand #xFFFFFFF0 (lsh (nes/ppu->tile-data2 ppu) 4))
                                                (logand (lsh (nes/ppu->tile-data1 ppu) -28) #b1111)))
        (setf (nes/ppu->tile-data1 ppu) (logand #xFFFFFFFF (lsh (nes/ppu->tile-data1 ppu) 4)))
        (case (% cycle 8)
          (1 (nes/ppu--fetch-current-name-table-byte ppu))
          (3 (nes/ppu--fetch-current-attribute-table-byte ppu))
          (5 (nes/ppu--fetch-current-tile-low-byte ppu))
          (7 (nes/ppu--fetch-current-tile-hi-byte ppu))
          (0 (nes/ppu--store-current-tile-data ppu))))

      (when (and pre-render-line-p (<= 280 cycle 304))
        (nes/ppu--copy-y ppu))

      (when render-line-p
        (when (and fetch-cycle-p (zerop (% cycle 8)))
          (nes/ppu--increment-x ppu))
        (when (eq cycle 256)
          (nes/ppu--increment-y ppu))
        (when (eq cycle 257)
          (nes/ppu--copy-x ppu)))

      ;; sprite logic
      (when (eq cycle 257)
        (if visible-line-p
            (nes/ppu--evaluate-sprites ppu)
          (setf (nes/ppu->sprite-count ppu) 0)))
      )

    (when (and (eq scanline 241) (eq cycle 1))
      (nes/ppu--set-vblank ppu))

    (when (and pre-render-line-p (eq cycle 1))
      (nes/ppu--clear-vblank ppu)
      (nes/ppu--clear-sprite-zero-hit ppu)
      (nes/ppu--clear-sprite-overflow ppu)
      )
    ))

(defun* nes/ppu--tick (ppu)
  (when (> (nes/ppu->nmi-delay ppu) 0)
    (decf (nes/ppu->nmi-delay ppu))
    (when (and (zerop (nes/ppu->nmi-delay ppu))
               (nes/ppu--generate-nmi-p ppu)
               (nes/ppu--vblank-p ppu))
      (nes/interrupt-assert-nmi (nes/ppu->interrupt ppu))))

  (when (or (nes/ppu--background-enabled-p ppu)
            (nes/ppu--sprite-enabled-p ppu))
    (when (and (eq (nes/ppu->f ppu) 1)
               (eq (nes/ppu->line ppu) 261)
               (eq (nes/ppu->cycle ppu) 339))
      (setf (nes/ppu->cycle ppu) 0)
      (setf (nes/ppu->line ppu) 0)
      (setf (nes/ppu->f ppu) (logxor (nes/ppu->f ppu) 1))
      (return-from nes/ppu--tick)))

  (incf (nes/ppu->cycle ppu))

  (let ((right-end-cycle-p (>= (nes/ppu->cycle ppu) nes/ppu:CYCLES-PER-LINE))
        (bottom-end-line-p (>= (nes/ppu->line ppu) (+ nes/ppu:SCREEN-HEIGHT nes/ppu:VBLANK-HEIGHT)))
        )
  (when right-end-cycle-p
    (setf (nes/ppu->cycle ppu) 0)
    (incf (nes/ppu->line ppu))
    (when bottom-end-line-p
      (setf (nes/ppu->line ppu) 0)
      (setf (nes/ppu->f ppu) (logxor (nes/ppu->f ppu) 1)))
    ))
  )

;;
;; https://wiki.nesdev.com/w/index.php/PPU_scrolling#At_dot_257_of_each_scanline
;;
;; v: ....F.. ...EDCBA = t: ....F.. ...EDCBA
;;
(defun nes/ppu--copy-x (ppu)
  (setf (nes/ppu->v ppu) (logior (logand (nes/ppu->v ppu) #xFBE0)
                                 (logand (nes/ppu->temp ppu) #x041F))))

;;
;; https://wiki.nesdev.com/w/index.php/PPU_scrolling#During_dots_280_to_304_of_the_pre-render_scanline_.28end_of_vblank.29
;;
;; v: IHGF.ED CBA..... = t: IHGF.ED CBA.....
;;
(defun nes/ppu--copy-y (ppu)
  (setf (nes/ppu->v ppu) (logior (logand (nes/ppu->v ppu) #x841F)
                                 (logand (nes/ppu->temp ppu) #x7BE0))))

;;
;; https://wiki.nesdev.com/w/index.php/PPU_scrolling#Coarse_X_increment
;;
(defun nes/ppu--increment-x (ppu)
  (if (eq (nes/ppu--coarse-x-scroll ppu) 31)
      (progn
        (nes/ppu--clear-coarse-x-scroll ppu)
        (nes/ppu--toggle-horizontal-name-table-select ppu))
    (nes/ppu--increment-coarse-x-scroll ppu)))

;;
;; https://wiki.nesdev.com/w/index.php/PPU_scrolling#Y_increment
;;
(defun nes/ppu--increment-y (ppu)
  (if (< (nes/ppu--fine-y-scroll ppu) 7)
      (nes/ppu--increment-fine-y-scroll ppu)
    (let ((coarse-y (nes/ppu--coarse-y-scroll ppu))
          (y 0))
      (nes/ppu--clear-fine-y-scroll ppu)
      (case coarse-y
       (29
        (nes/ppu--toggle-vertical-name-table-select ppu))
       ;; (31
       ;;  (setq y 0))
       (t
        (setq y (1+ coarse-y))
        ))
      (nes/ppu--set-coarse-y-scroll ppu y))))

(defun nes/ppu--nmi-change (ppu)
  (let ((nmi (and (nes/ppu--generate-nmi-p ppu)
                  (nes/ppu--vblank-p ppu))))
    (when (and nmi (null (nes/ppu->nmi-previous ppu)))
      (setf (nes/ppu->nmi-delay ppu) 15))
    (setf (nes/ppu->nmi-previous ppu) nmi)))

;;
;; https://wiki.nesdev.com/w/index.php/PPU_scrolling#Tile_and_attribute_fetching
;;
;;   Name table (address)
;;
;;                         (x)
;;        0       256      512
;;      0 +--------+--------+
;;        |        |        |
;;        | 0x2000 | 0x2400 |
;;        |        |        |
;;    240 +--------+--------+
;;        |        |        |
;;        | 0x2800 | 0x2C00 |
;;        |        |        |
;;    480 +--------+--------+
;;    (y)
;;
(defun nes/ppu--fetch-current-name-table-byte (ppu)
  "Return nametable byte (character id)"
  (let ((address (logior #x2000 (logand (nes/ppu->v ppu) #x0FFF))))
    (setf (nes/ppu->name-table-byte ppu) (nes/ppu--bus-read ppu address))))

;;
;; https://wiki.nesdev.com/w/index.php/PPU_scrolling#Tile_and_attribute_fetching
;;
;;
;;   Attribute table id (address)
;;
;;        0       256      512
;;      0 +--------+--------+
;;        |        |        |
;;        | 0x23C0 | 0x27C0 |
;;        |        |        |
;;    240 +--------+--------+
;;        |        |        |
;;        | 0x2BC0 | 0x2FC0 |
;;        |        |        |
;;    480 +--------+--------+
;;    (y)
;;
(defun nes/ppu--fetch-current-attribute-table-byte (ppu)
  (let* ((v (nes/ppu->v ppu))
         (address (logior #x23C0
                          (logand #x0C00 v)
                          (logand #x0038 (lsh v -4))
                          (logand #x0007 (lsh v -2))))
         (shift (logior (logand #x04 (lsh v -4))
                        (logand #x02 v)))
         )
    (setf (nes/ppu->attribute-table-byte ppu)
          (lsh (logand (lsh (nes/ppu--bus-read ppu address) (- shift)) #x03) 2))))

;;
;; https://wiki.nesdev.com/w/index.php/PPU_scrolling
;;
;;   yyy NN YYYYY XXXXX
;;   ||| || ||||| +++++-- coarse X scroll
;;   ||| || +++++-------- coarse Y scroll
;;   ||| ++-------------- nametable select
;;   +++----------------- fine Y scroll
;;
;; memo: 16 bytes in one character (low + hi)
;;
(defun nes/ppu--current-tile-low-byte-address (ppu)
  (let ((offset (nes/ppu--background-pattern-table-address-offset ppu))
        (tile (nes/ppu->name-table-byte ppu)))
    (+ (* tile 16) (nes/ppu--fine-y-scroll ppu) offset)))

(defun nes/ppu--fetch-current-tile-low-byte (ppu)
  (let ((address (nes/ppu--current-tile-low-byte-address ppu)))
    (setf (nes/ppu->tile-low-byte ppu) (nes/ppu--bus-read ppu (+ address 0)))))

(defun nes/ppu--fetch-current-tile-hi-byte (ppu)
  (let ((address (nes/ppu--current-tile-low-byte-address ppu)))
    (setf (nes/ppu->tile-hi-byte ppu) (nes/ppu--bus-read ppu (+ address 8)))))

;;
;; PPU pattern tables
;; https://wiki.nesdev.com/w/index.php/PPU_pattern_tables
;;
(defun nes/ppu--store-current-tile-data (ppu)
  (let ((attribute (nes/ppu->attribute-table-byte ppu))
        (low (nes/ppu->tile-low-byte ppu))
        (hi (nes/ppu->tile-hi-byte ppu))
        (data 0))
    (dotimes (i nes/ppu:TILE-WIDTH)
      (let* ((bit (- nes/ppu:TILE-WIDTH i 1))
             (p1 (if (nes--logbitp bit hi) #b10 #b00))
             (p2 (if (nes--logbitp bit low) #b01 #b00)))
        (setq data (logior (lsh data 4)
                           (logand #xFF (logior attribute p1 p2))))))
    (setf (nes/ppu->tile-data1 ppu) (logand #xFFFFFFFF data))))

(defun nes/ppu--evaluate-sprites (ppu)
  (let ((h (nes/ppu--sprite-height ppu))
        (count 0))
    (dotimes (i 64)
      (let* ((base-addr (* i 4))
             ;;
             ;; https://wiki.nesdev.com/w/index.php/PPU_OAM
             ;;
             (y         (nes/ppu--read-from-sprite-ram ppu (+ base-addr 0)))
             (tile      (nes/ppu--read-from-sprite-ram ppu (+ base-addr 1)))
             (attribute (nes/ppu--read-from-sprite-ram ppu (+ base-addr 2)))
             (x         (nes/ppu--read-from-sprite-ram ppu (+ base-addr 3)))

             ;;
             ;; line number in currnet sprite
             ;;
             (row (- (nes/ppu--current-y ppu) y))

             ;;
             ;; Flags determined by attribute
             ;;
             (sprite-priority-p (not (nes--logbitp 5 attribute)))
             (reversed-vertically-p (nes--logbitp 7 attribute))
             )
        (when (<= 0 row (1- h))
          (when reversed-vertically-p
            (setq row (- h 1 row)))
          (when (< count 8)
            (aset (nes/ppu->sprite-patterns ppu) count (nes/ppu--fetch-sprite-pattern ppu row tile attribute (eq h 8)))
            (aset (nes/ppu->sprite-positions ppu) count x)
            (aset (nes/ppu->sprite-priorities ppu) count sprite-priority-p)
            (aset (nes/ppu->sprite-indexes ppu) count i))
          (incf count)
          )))

      (when (> count 8)
        (nes/ppu--set-sprite-overflow ppu)
        (setq count 8))
      (setf (nes/ppu->sprite-count ppu) count)
      ))

(defun nes/ppu--fetch-sprite-pattern (ppu row tile attribute sprite-height-8-p)
  (let ((palette-id (lsh (logand attribute #b11) 2))
        (reversed-horizontally-p (nes--logbitp 6 attribute))
        (base-addr 0)
        (addr 0)
        (low 0)
        (hi 0)
        (patterns 0))
    ;;
    ;; https://wiki.nesdev.com/w/index.php/PPU_OAM#Byte_1
    ;;
    (if sprite-height-8-p
        (setq base-addr (nes/ppu--sprite-pattern-table-address-offset ppu))
      (progn
        (setq base-addr (* #x1000 (logand tile #b00000001)))
        (setq tile (logand tile #b11111110))
        ;;
        ;; if bottom half
        ;;
        (when (> row 7)
          (incf tile)
          (decf row 8)))
      )

    ;;
    ;; ram[addr] has 16 bit sprite data (low + hi)
    ;;
    (setq addr (+ base-addr (* nes/ppu:TILE-HEIGHT 2 (logand #xFF tile)) row))
    (setq low (nes/ppu--bus-read ppu addr))
    (setq hi (nes/ppu--bus-read ppu (+ addr 8)))
    (dotimes (i nes/ppu:TILE-WIDTH)
      (let ((idx (if reversed-horizontally-p i (- nes/ppu:TILE-WIDTH 1 i))))
        (setq patterns (logior (lsh patterns 4)
                               palette-id
                               (if (nes--logbitp idx low) #b01 #b00)
                               (if (nes--logbitp idx hi) #b10 #b00)))))
    patterns))

(defun nes/ppu--render-pixel (ppu)
  (let* ((x (nes/ppu--current-x ppu))
         (y (nes/ppu--current-y ppu))

         ;;
         ;; bg-color-index == 0 means `Universal background color`
         ;;
         (bg-color-index (if (or (>= x 8) (nes/ppu--left-background-enabled-p ppu))
                             (nes/ppu--get-current-background-palette-index ppu)
                           0))

         ;;
         ;; sp-color-index == 0 means `transparent`
         ;;
         (sp (nes/ppu--get-current-sprite-pixel ppu))
         (sp-index (elt sp 0))
         (sp-color-index (if (or (>= x 8) (nes/ppu--left-sprite-enabled-p ppu))
                             (elt sp 1) 0))

         ;;
         ;; https://wiki.nesdev.com/w/index.php/PPU_palettes#Memory_Map
         ;;
         (bg-p (/= (% bg-color-index 4) 0))
         (sp-p (/= (% sp-color-index 4) 0))

         (color-index (cond
                       ((and (not bg-p) (not sp-p))
                        0)

                       ((and (not bg-p) sp-p)
                        (logior sp-color-index #x10))

                       ((and bg-p (not sp-p))
                        bg-color-index)

                       (t
                        (when (and (zerop (aref (nes/ppu->sprite-indexes ppu) sp-index))
                                   (< x 255))
                          (nes/ppu--set-sprite-zero-hit ppu))
                        (if (aref (nes/ppu->sprite-priorities ppu) sp-index)
                            (logior sp-color-index #x10)
                          bg-color-index)))
                      )

         )
    (nes/ppu--image-write x y (nes/ppu--read-from-palette-table ppu color-index))
  ))

(defun nes/ppu--image-write (x y color)
  (nes/ppu--render-set-cell x y color))

(defun nes/ppu--get-current-background-palette-index (ppu)
  (if (nes/ppu--background-enabled-p ppu)
      (let ((index (- nes/ppu:TILE-WIDTH 1 (nes/ppu->x ppu))))
        (logand (lsh (nes/ppu->tile-data2 ppu) (- (* index 4))) #b1111))
    0))

(defun* nes/ppu--get-current-sprite-pixel (ppu)
  (if (nes/ppu--sprite-enabled-p ppu)
      (let ((offset 0)
            (color 0))
        (dotimes (i (nes/ppu->sprite-count ppu))
          (setq offset (- (nes/ppu--current-x ppu) (aref (nes/ppu->sprite-positions ppu) i)))
          (when (<= 0 offset 7)
            (setq color (logand #xF (lsh (aref (nes/ppu->sprite-patterns ppu) i) (- (* (- 7 offset) 4)))))
            (when (/= (% color 4) 0)
              (return-from nes/ppu--get-current-sprite-pixel (vector i color)))))
        [0 0])
    [0 0]))

(defun nes/ppu--vram-address-increment-offset (ppu)
  (if (nes--logbitp 2 (nes/ppu->ppuctrl ppu)) 32 1))

(defun nes/ppu--sprite-pattern-table-address-offset (ppu)
  (if (nes--logbitp 3 (nes/ppu->ppuctrl ppu)) #x1000 #x0000))

(defun nes/ppu--background-pattern-table-address-offset (ppu)
  (if (nes--logbitp 4 (nes/ppu->ppuctrl ppu)) #x1000 #x0000))

(defun nes/ppu--sprite-height (ppu)
  (if (nes--logbitp 5 (nes/ppu->ppuctrl ppu)) 16 8))

(defun nes/ppu--generate-nmi-p (ppu)
  (nes--logbitp 7 (nes/ppu->ppuctrl ppu)))

(defun nes/ppu--set-vblank (ppu)
  (setf (nes/ppu->ppustatus ppu) (logior (nes/ppu->ppustatus ppu) #x80))
  (nes/ppu--nmi-change ppu)
  )

(defun nes/ppu--clear-vblank (ppu)
  (setf (nes/ppu->ppustatus ppu) (logand (nes/ppu->ppustatus ppu) #x7F))
  (nes/ppu--nmi-change ppu)
  )

(defun nes/ppu--vblank-p (ppu)
  (nes--logbitp 7 (nes/ppu->ppustatus ppu)))

(defun nes/ppu--set-sprite-zero-hit (ppu)
  (setf (nes/ppu->ppustatus ppu) (logior (nes/ppu->ppustatus ppu) #x40)))

(defun nes/ppu--clear-sprite-zero-hit (ppu)
  (setf (nes/ppu->ppustatus ppu) (logand (nes/ppu->ppustatus ppu) #xBF)))

(defun nes/ppu--set-sprite-overflow (ppu)
  (setf (nes/ppu->ppustatus ppu) (logior (nes/ppu->ppustatus ppu) #x20)))

(defun nes/ppu--clear-sprite-overflow (ppu)
  (setf (nes/ppu->ppustatus ppu) (logand (nes/ppu->ppustatus ppu) #xDF)))

(defun nes/ppu--left-background-enabled-p (ppu)
  (nes--logbitp 1 (nes/ppu->ppumask ppu)))

(defun nes/ppu--left-sprite-enabled-p (ppu)
  (nes--logbitp 2 (nes/ppu->ppumask ppu)))

(defun nes/ppu--background-enabled-p (ppu)
  (nes--logbitp 3 (nes/ppu->ppumask ppu)))

(defun nes/ppu--sprite-enabled-p (ppu)
  (nes--logbitp 4 (nes/ppu->ppumask ppu)))

(defsubst nes/ppu--current-x (p)
  (1- (nes/ppu->cycle p)))

(defsubst nes/ppu--current-y (p)
  (nes/ppu->line p))

(defsubst nes/ppu--coarse-x-scroll (ppu)
  (logand (nes/ppu->v ppu) #b0000000000011111))

(defsubst nes/ppu--increment-coarse-x-scroll (ppu)
  (incf (nes/ppu->v ppu)))

(defsubst nes/ppu--clear-coarse-x-scroll (ppu)
  (setf (nes/ppu->v ppu) (logand (nes/ppu->v ppu) #b1111111111100000)))

(defsubst nes/ppu--coarse-y-scroll (ppu)
  (lsh (logand (nes/ppu->v ppu) #b0000001111100000) -5))

(defsubst nes/ppu--set-coarse-y-scroll (ppu y)
  (setf (nes/ppu->v ppu) (logior (logand (nes/ppu->v ppu) #b01111110000011111)
                                 (lsh y 5))))

(defsubst nes/ppu--fine-y-scroll (ppu)
  (lsh (logand (nes/ppu->v ppu) #b0111000000000000) -12))

(defsubst nes/ppu--clear-fine-y-scroll (ppu)
  (setf (nes/ppu->v ppu) (logand (nes/ppu->v ppu) #b1000111111111111)))

(defsubst nes/ppu--increment-fine-y-scroll (ppu)
  (incf (nes/ppu->v ppu) #b0001000000000000))

(defsubst nes/ppu--toggle-horizontal-name-table-select (ppu)
  (setf (nes/ppu->v ppu) (logxor (nes/ppu->v ppu) #b0000010000000000)))

(defsubst nes/ppu--toggle-vertical-name-table-select (ppu)
  (setf (nes/ppu->v ppu) (logxor (nes/ppu->v ppu) #b0000100000000000)))

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
  (let ((address (% (+ (nes/ppu->sprite-ram-addr ppu) index) nes/ppu:SPRITE-RAM-BYTESIZE)))
    (aset (nes/ppu->sprite-ram ppu) address value)))

(provide 'nes-ppu)
