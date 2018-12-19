;; -*- lexical-binding: t -*-

(eval-when-compile (require 'cl))

(defvar nes/keypad:a (kbd ","))
(defvar nes/keypad:b (kbd "."))
(defvar nes/keypad:up (kbd "w"))
(defvar nes/keypad:down (kbd "s"))
(defvar nes/keypad:left (kbd "a"))
(defvar nes/keypad:right (kbd "d"))
(defvar nes/keypad:select (kbd "n"))
(defvar nes/keypad:start (kbd "m"))

(defstruct (nes/keypad
            (:conc-name nes/keypad->))
  (index 0)
  (set-flag nil)
  (buffers (make-vector #x10 0)) ;; 1P + 2P
  (copies (make-vector #x10 0))
  )

(defun nes/keypad-write (k value)
  (cond
   ((eq (logand #b01 value) 1)
    (setf (nes/keypad->set-flag k) t))

   ((nes/keypad->set-flag k)
    (progn
      (setf (nes/keypad->set-flag k) nil)
      (setf (nes/keypad->copies k) (copy-sequence (nes/keypad->buffers k)))
      (setf (nes/keypad->index k) 0)
      (fillarray (nes/keypad->buffers k) 0)))
   ))

(defun nes/keypad-read (k)
  (let ((value (aref (nes/keypad->copies k) (nes/keypad->index k))))
    (setf (nes/keypad->index k) (mod (1+ (nes/keypad->index k)) #x10))
    value))

(defun nes/keypad-check (k keycode)
  (aset (nes/keypad->buffers k) keycode 1))

(defun nes/keypad-init (k map)
  (lexical-let ((k k))
    (define-key map nes/keypad:a      (lambda () (interactive) (nes/keypad-check k 0)))
    (define-key map nes/keypad:b      (lambda () (interactive) (nes/keypad-check k 1)))
    (define-key map nes/keypad:select (lambda () (interactive) (nes/keypad-check k 2)))
    (define-key map nes/keypad:start  (lambda () (interactive) (nes/keypad-check k 3)))
    (define-key map nes/keypad:up     (lambda () (interactive) (nes/keypad-check k 4)))
    (define-key map nes/keypad:down   (lambda () (interactive) (nes/keypad-check k 5)))
    (define-key map nes/keypad:left   (lambda () (interactive) (nes/keypad-check k 6)))
    (define-key map nes/keypad:right  (lambda () (interactive) (nes/keypad-check k 7)))
    ))

(provide 'nes-keypad)
