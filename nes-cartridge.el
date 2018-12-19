;; -*- lexical-binding: t -*-

(eval-when-compile (require 'cl))

(require 'bindat)

(defconst nes/cartridge-ines-header-spec
  '((constant vec 4 u8)
    (size-of-prg-rom u8)
    (size-of-chr-rom u8)
    (flags6 u8)
    (flags7 u8)
    (size-of-prg-ram u8)
    (flags9 u8)
    (flags10 u8)
    (zero-pad vec 5 u8)))

;;
;; see https://wiki.nesdev.com/w/index.php/INES#iNES_file_format
;;
(defstruct (nes/cartridge
            (:conc-name nes/cartridge->))
  header
  trainer
  prg-rom
  chr-rom
  pc-inst-rom ;; PlayChoice 10
  pc-prom-data
  pc-prom-counter-out-data)

(defstruct (nes/cartridge-header
            (:conc-name nes/cartridge-header->))
  constant
  size-of-prg-rom
  size-of-chr-rom
  flags6
  flags7
  size-of-prg-ram
  flags9
  flags10
  zero-pad)


(defun nes/cartridge--load-header (filepath)
  (let* ((data
          (with-temp-buffer
            ;; The first 16 bytes are the iNES header
            (insert-file-contents-literally filepath nil 0 16)
            (buffer-substring-no-properties (point-min) (point-max))))
         (decoded
          (bindat-unpack nes/cartridge-ines-header-spec (encode-coding-string data 'raw-text))))
    (make-nes/cartridge-header
     :constant (bindat-get-field decoded 'constant)
     :size-of-prg-rom (bindat-get-field decoded 'size-of-prg-rom)
     :size-of-chr-rom (bindat-get-field decoded 'size-of-chr-rom)
     :flags6 (bindat-get-field decoded 'flags6)
     :flags7 (bindat-get-field decoded 'flags7)
     :size-of-prg-ram (bindat-get-field decoded 'size-of-prg-ram)
     :flags9 (bindat-get-field decoded 'flags9)
     :flags10 (bindat-get-field decoded 'flags10)
     :zero-pad (bindat-get-field decoded 'zero-pad))))

(defun nes/cartridge-load (filepath)
  (let* ((header
          (nes/cartridge--load-header filepath))
         (trainer-exists
          (eq (logand (lsh (nes/cartridge-header->flags6 header) -2)) 1))
         (playchoice-exists
          (eq (logand (lsh (nes/cartridge-header->flags7 header) -1)) 1))
         (body-spec
          `((trainer vec
                     ,(if trainer-exists 512 0)
                     u8)
            (prg-rom vec
                     ,(* 16384 (nes/cartridge-header->size-of-prg-rom header))
                     u8)
            (chr-rom vec
                     ,(* 8192 (nes/cartridge-header->size-of-chr-rom header))
                     u8)
            (pc-inst-rom vec
                         ,(if playchoice-exists 8192 0)
                         u8)
            (pc-prom-data vec
                          ,(if playchoice-exists 16 0)
                          u8)
            (pc-prom-counter-out-data vec
                                      ,(if playchoice-exists 16 0)
                                      u8)
            ))
         (body
          (with-temp-buffer
            (insert-file-contents-literally filepath nil 16)
            (buffer-substring-no-properties (point-min) (point-max))))
         (body-decoded
          (bindat-unpack body-spec (encode-coding-string body 'raw-text))))

    (make-nes/cartridge
     :header header
     :trainer (bindat-get-field body-decoded 'trainer)
     :prg-rom (bindat-get-field body-decoded 'prg-rom)
     :chr-rom (bindat-get-field body-decoded 'chr-rom)
     :pc-inst-rom (bindat-get-field body-decoded 'pc-inst-rom)
     :pc-prom-data (bindat-get-field body-decoded 'pc-prom-data)
     :pc-prom-counter-out-data (bindat-get-field body-decoded 'pc-prom-counter-out-data))))

(defun nes/cartridge-read-from-prg-rom (cart addr)
  (let ((rom (nes/cartridge->prg-rom cart)))
    (aref rom (mod addr (length rom)))))

(provide 'nes-cartridge)
