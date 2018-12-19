;; -*- lexical-binding: t -*-

(eval-when-compile (require 'cl))

(require 'nes-cartridge)
(require 'nes-cpu)
(require 'nes-ppu)
(require 'nes-dma)
(require 'nes-keypad)
(require 'nes-instruction)
(require 'nes-interrupt)
(require 'nes-color)
(require 'nes-util)

(defconst nes-buffer-name "*NES-EMULATOR*")

(defvar nes--current-cartridge-filename nil)
(defvar nes--current-game nil)

(setq nes-mode-map
      (let ((map (make-sparse-keymap 'nes-mode-map)))
        (define-key map (kbd "q") #'nes-quit)
        map))

(defstruct nes
  (cpu nil)
  (ppu nil)
  (dma nil)
  (keypad nil)
  (cart nil)
  (interrupt nil))

(defun nes-setup (filename)
  (let ((cart (nes/cartridge-load filename))
        (keypad (make-nes/keypad))
        (interrupt (make-nes/interrupt))
        (ram (make-vector #x0800 0))
        (cpu)
        (ppu)
        (dma)
        )
    ;; ppu
    (setq ppu (make-nes/ppu :interrupt interrupt))
    (nes/ppu-set-character-ram ppu (copy-sequence (nes/cartridge->chr-rom cart)))
    (nes/ppu-init nes-buffer-name)

    ;; dma
    (setq dma (make-nes/dma :ppu ppu :ram ram))

    ;; keypad
    (nes/keypad-init keypad nes-mode-map)

    ;; cpu
    (setq cpu (make-nes/cpu :ppu ppu :keypad keypad :interrupt interrupt :dma dma))
    (nes/cpu-set-working-ram cpu ram)
    (nes/cpu-set-program-rom cpu (lexical-let ((cart cart))
                                   (lambda (addr)
                                     (nes/cartridge-read-from-prg-rom cart addr))))
    (nes/cpu-init cpu)

    (make-nes :cpu cpu :ppu ppu :dma dma :keypad keypad :cart cart :interrupt interrupt)))

(defun nes-update ()
  (let ((buffer (get-buffer nes-buffer-name)))
    (when (eq (current-buffer) buffer)
      (let ((c (nes-cpu nes--current-game))
            (p (nes-ppu nes--current-game))
            (d (nes-dma nes--current-game)))
        (nes/dma-transfer d)
        (dotimes (_ 1000)
          (dotimes (_ (* (nes/cpu-step c) 3))
            (nes/ppu-step p))
          )
        ))
    (when buffer
      (run-at-time 0.001 nil 'nes-update))))

(defun nes-quit ()
  (interactive)
  (setq nes--current-game nil)
  (gamegrid-kill-timer)
  (kill-buffer nes-buffer-name))

(define-derived-mode nes-mode nil "NES Emulator"
  (use-local-map nes-mode-map)
  (add-hook 'kill-buffer-hook 'gamegrid-kill-timer nil t)
  (gamegrid-kill-timer)
  (setq nes--current-game (nes-setup nes--current-cartridge-filename))
  (run-at-time 0.001 nil 'nes-update)
  )

(defun nes (filename)
  "nes-mode keybindings:
\\{nes-mode-map}"
  (interactive "ffilename: ")
  (select-window (or (get-buffer-window nes-buffer-name)
                     (selected-window)))
  (switch-to-buffer nes-buffer-name)
  (setq nes--current-cartridge-filename filename)
  (nes-mode))
