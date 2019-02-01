;; -*- lexical-binding: t -*-

(eval-when-compile (require 'cl))

(require 'nes-dma)
(require 'nes-instruction)
(require 'nes-interrupt)
(require 'nes-keypad)
(require 'nes-ppu)
(require 'nes-util)

;; https://wiki.nesdev.com/w/index.php/CPU_registers
(defstruct (nes/cpu-register
            (:conc-name nes/cpu-register->))
  (acc 0)
  (idx-x 0)
  (idx-y 0)
  (pc 0)
  (sp #x01fd)

  ;; status register
  (sr-carry nil)
  (sr-zero nil)
  (sr-interrupt t)
  (sr-decimal nil)
  (sr-break t)
  (sr-reserved t)
  (sr-overflow nil)
  (sr-negative nil))

(defstruct (nes/cpu-bus
            (:conc-name nes/cpu-bus->))
  (ram nil)
  (prg-rom (lambda (ignored)))
  )

(defstruct (nes/cpu
            (:conc-name nes/cpu->))
  (cycles 0)
  (ppu nil)
  (dma nil)
  (keypad nil)
  (bus (make-nes/cpu-bus))
  (register (make-nes/cpu-register))
  (interrupt nil)
  )

(defun nes/cpu--bus-read (c addr)
  (let ((b (nes/cpu->bus c))
        (ppu (nes/cpu->ppu c)))
    (cond
     ((< addr #x0800) (aref (nes/cpu-bus->ram b) addr))
     ((< addr #x2000) (nes/cpu--bus-read c (- addr #x0800)))
     ((< addr #x4000) (nes/ppu-read ppu (+ (mod addr #x0008) #x2000)))
     ((eq addr #x4016) (nes/keypad-read (nes/cpu->keypad c)))
     ;; ((eq addr #x4017) (nes/keypad-read (nes/cpu->keypad c))) ;; 2P
     ((>= addr #x8000) (funcall (nes/cpu-bus->prg-rom b) addr))
     (t 0)
     )))

(defun nes/cpu--bus-write (c addr data)
  (let ((b (nes/cpu->bus c))
        (ppu (nes/cpu->ppu c)))
    (cond
     ((< addr #x2000) (aset (nes/cpu-bus->ram b) (% addr #x0800) data))
     ((< addr #x4000) (nes/ppu-write ppu (+ (% addr #x0008) #x2000) data))
     ;; ((< addr #x4014) ( ... )) ;; APU
     ((eq addr #x4014) (nes/dma-request-transfer (nes/cpu->dma c) data))
     ((eq addr #x4016) (nes/keypad-write (nes/cpu->keypad c) data))
     ;; ((eq addr #x4017) (nes/keypad-write (nes/cpu->keypad c) data))  ;; 2P
     )))

(defun nes/cpu-read (c addr &optional size)
  (setq size (or size :byte))
  (setq addr (logand addr #xffff))
  (if (eq size :word)
      (logior
       (logand (nes/cpu--bus-read c addr) #xFF)
       (logand (lsh (nes/cpu--bus-read c (1+ addr)) 8) #xFF00)
       )
    (logand (nes/cpu--bus-read c addr) #xFF)))

(defun nes/cpu-write (c addr data)
  (nes/cpu--bus-write c addr data))

(defun nes/cpu-push (c data)
  (let* ((register (nes/cpu->register c))
         (addr (nes/cpu-register->sp register)))
    (nes/cpu-write c (logior #x100 (logand addr #x0ff)) data)
    (setf (nes/cpu-register->sp register) (1- addr))))

(defun nes/cpu-pull (c)
  (let* ((register (nes/cpu->register c))
         (addr (1+ (nes/cpu-register->sp register))))
    (setf (nes/cpu-register->sp register) addr)
    (nes/cpu-read c (logior #x100 (logand addr #x0ff)))))

(defun nes/cpu-push-status-register (c)
  (let ((r (nes/cpu->register c)))
    (nes/cpu-push c
                  (logior (lsh (if (nes/cpu-register->sr-negative r)  1 0) 7)
                          (lsh (if (nes/cpu-register->sr-overflow r)  1 0) 6)
                          (lsh (if (nes/cpu-register->sr-reserved r)  1 0) 5)
                          (lsh (if (nes/cpu-register->sr-break r)     1 0) 4)
                          (lsh (if (nes/cpu-register->sr-decimal r)   1 0) 3)
                          (lsh (if (nes/cpu-register->sr-interrupt r) 1 0) 2)
                          (lsh (if (nes/cpu-register->sr-zero r)      1 0) 1)
                          (lsh (if (nes/cpu-register->sr-carry r)     1 0) 0)))))

(defun nes/cpu-pull-status-register (c)
  (let ((data (nes/cpu-pull c))
        (r (nes/cpu->register c)))
    (setf (nes/cpu-register->sr-negative r)  (nes--logbitp 7 data))
    (setf (nes/cpu-register->sr-overflow r)  (nes--logbitp 6 data))
    (setf (nes/cpu-register->sr-reserved r)  (nes--logbitp 5 data))
    (setf (nes/cpu-register->sr-break r)     (nes--logbitp 4 data))
    (setf (nes/cpu-register->sr-decimal r)   (nes--logbitp 3 data))
    (setf (nes/cpu-register->sr-interrupt r) (nes--logbitp 2 data))
    (setf (nes/cpu-register->sr-zero r)      (nes--logbitp 1 data))
    (setf (nes/cpu-register->sr-carry r)     (nes--logbitp 0 data))))


(defun nes/cpu-reset (c)
  (let ((r (nes/cpu->register c)))
    (setf (nes/cpu-register->sp r) #xfd)
    (setf (nes/cpu-register->pc r) (nes/cpu-read c #xfffc :word))
    (setf (nes/cpu-register->sr-carry r) nil)
    (setf (nes/cpu-register->sr-zero r) nil)
    (setf (nes/cpu-register->sr-interrupt r) t)
    (setf (nes/cpu-register->sr-decimal r) nil)
    (setf (nes/cpu-register->sr-break r) t)
    (setf (nes/cpu-register->sr-reserved r) t)
    (setf (nes/cpu-register->sr-overflow r) nil)
    (setf (nes/cpu-register->sr-negative r) nil)
    ))

(defun nes/cpu-nmi (c)
  (let ((register (nes/cpu->register c)))
    (nes/interrupt-deassert-nmi (nes/cpu->interrupt c))
    (setf (nes/cpu-register->sr-break register) nil)
    (nes/cpu-push c (logand #xFF (lsh (nes/cpu-register->pc register) -8)))
    (nes/cpu-push c (logand #xFF (nes/cpu-register->pc register)))
    (nes/cpu-push-status-register c)
    (setf (nes/cpu-register->sr-interrupt register) t)
    (setf (nes/cpu-register->pc register) (nes/cpu-read c #xFFFA :word))))

(defun nes/cpu-irq (c)
  (let ((register (nes/cpu->register c)))
    (nes/cpu-push c (logand #xff (lsh (nes/cpu-register->pc register) -8)))
    (nes/cpu-push c (logand #xff (nes/cpu-register->pc register)))
    (nes/cpu-push-status-register c)
    (setf (nes/cpu-register->sr-interrupt register) t)
    (setf (nes/cpu-register->pc register) (nes/cpu-read c #xfffe :word))))

(defun nes/cpu--get-instruction-operand-and-cycle (c mode)
  (let ((register (nes/cpu->register c)))
    (case mode
      (:accumulator
       '(nil . 0))

      (:implied
       '(nil . 0))

      (:immediate
       (cons (nes/cpu--fetch c) 0))

      (:relative
       (let* ((base-addr (nes/cpu--fetch c))
              (addr (if (< base-addr #x80)
                        (+ base-addr (nes/cpu-register->pc register))
                      (- (+ base-addr (nes/cpu-register->pc register)) 256)))
              (cycle (if (not (eq (logand addr #xFF00) (logand (nes/cpu-register->pc register) #xFF00))) 1 0)))
         (cons addr cycle)))

      (:zero-page
       (cons (nes/cpu--fetch c) 0))

      (:zero-page-x
       (cons (logand (+ (nes/cpu--fetch c)
                        (nes/cpu-register->idx-x register))
                     #xFF)
             0))

      (:zero-page-y
       (cons (logand (+ (nes/cpu--fetch c)
                        (nes/cpu-register->idx-y register))
                     #xFF)
             0))

      (:absolute
       (cons (nes/cpu--fetch c :word) 0))

      (:absolute-x
       (let* ((addr (nes/cpu--fetch c :word))
              (idx-x (nes/cpu-register->idx-x register))
              (cycle (if (eq (logand addr #xFF00) (logand (+ addr idx-x) #xFF00))
                         0 1)))
         (cons (logand (+ addr idx-x) #xFFFF) cycle)))

      (:absolute-y
       (let* ((addr (nes/cpu--fetch c :word))
              (idx-y (nes/cpu-register->idx-y register))
              (cycle (if (eq (logand addr #xFF00) (logand (+ addr idx-y) #xFF00))
                         0 1)))
         (cons (logand (+ addr idx-y) #xFFFF) cycle)))

      (:pre-indexed-indirect
       (let* ((base-addr (logand (+ (nes/cpu--fetch c) (nes/cpu-register->idx-x register))
                                 #xFF))
              (addr (logand (+ (nes/cpu-read c base-addr)
                               (lsh (nes/cpu-read c (logand (1+ base-addr) #xFF)) 8))
                            #xFFFF))
              (cycle (if (/= (logand addr #xFF00) (logand base-addr #xFF00)) 1 0)))
         (cons addr cycle)))

      (:post-indexed-indirect
       (let* ((data-addr (nes/cpu--fetch c))
              (base-addr (+ (nes/cpu-read c data-addr)
                            (lsh (nes/cpu-read c (logand (1+ data-addr) #xFF)) 8)))
              (addr (logand (+ base-addr (nes/cpu-register->idx-y register)) #xFFFF))
              (cycle (if (/= (logand addr #xFF00) (logand base-addr #xFF00)) 1 0)))
         (cons addr cycle)))

      (:indirect-absolute
       (let ((data-addr (nes/cpu--fetch c :word)))
         (cons (logand (+ (nes/cpu-read c data-addr)
                          (lsh (nes/cpu-read c (logior (logand data-addr #xFF00)
                                                       (logand (1+ (logand data-addr #xFF)) #xFF)))
                               8))
                       #xFFFF)
               0))))))

(defun nes/cpu--fetch (c &optional size)
  (let ((size (or size :byte))
        (addr (nes/cpu-register->pc (nes/cpu->register c))))
    (incf (nes/cpu-register->pc (nes/cpu->register c)) (if (eq size :word) 2 1))
    (nes/cpu-read c addr size)))

(defun nes/cpu-set-working-ram (cpu ram)
  (setf (nes/cpu-bus->ram (nes/cpu->bus cpu)) ram))

(defun nes/cpu-set-program-rom (cpu rom-func)
  (setf (nes/cpu-bus->prg-rom (nes/cpu->bus cpu)) rom-func))

(defun nes/cpu-step (c)
  (when (nes/interrupt->nmi (nes/cpu->interrupt c))
    (nes/cpu-nmi c))
  (nes/interrupt-clear (nes/cpu->interrupt c))
  (let* ((opcode (nes/cpu--fetch c))
         (inst (aref nes/instruction:MAP opcode))
         (operand-and-cycle (nes/cpu--get-instruction-operand-and-cycle c (nes/instruction->mode inst)))
         (operand (car operand-and-cycle))
         (inst-cycle (aref nes/instruction:CYCLES opcode))
         (add-cycle (cdr operand-and-cycle)))
    (setf (nes/cpu->cycles c) 0)
    (funcall (nes/instruction->func inst) c operand (nes/instruction->mode inst))
    (+ (nes/cpu->cycles c) inst-cycle add-cycle)
    ))

(defun nes/cpu-init (c)
  (nes/cpu-reset c))

(provide 'nes-cpu)
