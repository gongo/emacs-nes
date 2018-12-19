;; -*- lexical-binding: t -*-

(eval-when-compile (require 'cl))

(require 'nes-ppu)

(defconst nes/dma:TRANSFER-BYTESIZE #x0100)

(defstruct (nes/dma
            (:conc-name nes/dma->))
  (ppu nil)
  (ram nil)
  (ram-addr #x0000)
  (processing nil))

(defun nes/dma-processing-p (dma)
  (nes/dma->processing dma))

(defun nes/dma-request-transfer (dma address)
  (setf (nes/dma->processing dma) t)
  (setf (nes/dma->ram-addr dma) (lsh address 8)))

(defun nes/dma-transfer (dma)
  (when (nes/dma-processing-p dma)
    (dotimes (i nes/dma:TRANSFER-BYTESIZE)
      (nes/ppu-transfer-sprite-data (nes/dma->ppu dma)
                                    i
                                    (aref (nes/dma->ram dma) (+ (nes/dma->ram-addr dma) i))))
    (setf (nes/dma->processing dma) nil)))

(provide 'nes-dma)
