;; -*- lexical-binding: t -*-

(eval-when-compile (require 'cl))

(defstruct (nes/interrupt
            (:conc-name nes/interrupt->))
  (irq nil)
  (nmi nil))

(defun nes/interrupt-assert-nmi (i)
  (setf (nes/interrupt->nmi i) t))

(defun nes/interrupt-deassert-nmi (i)
  (setf (nes/interrupt->nmi i) nil))

(defun nes/interrupt-clear (i)
  (setf (nes/interrupt->irq i) nil)
  (setf (nes/interrupt->nmi i) nil))

(provide 'nes-interrupt)
