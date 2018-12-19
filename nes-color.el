;; -*- lexical-binding: t -*-

(eval-when-compile (require 'cl))
(require 'cl-lib)

(defconst nes/color:COLORS
  [
   (#x80 #x80 #x80)
   (#x00 #x3D #xA6)
   (#x00 #x12 #xB0)
   (#x44 #x00 #x96)

   (#xA1 #x00 #x5E)
   (#xC7 #x00 #x28)
   (#xBA #x06 #x00)
   (#x8C #x17 #x00)

   (#x5C #x2F #x00)
   (#x10 #x45 #x00)
   (#x05 #x4A #x00)
   (#x00 #x47 #x2E)

   (#x00 #x41 #x66)
   (#x00 #x00 #x00)
   (#x05 #x05 #x05)
   (#x05 #x05 #x05)

   (#xC7 #xC7 #xC7)
   (#x00 #x77 #xFF)
   (#x21 #x55 #xFF)
   (#x82 #x37 #xFA)

   (#xEB #x2F #xB5)
   (#xFF #x29 #x50)
   (#xFF #x22 #x00)
   (#xD6 #x32 #x00)

   (#xC4 #x62 #x00)
   (#x35 #x80 #x00)
   (#x05 #x8F #x00)
   (#x00 #x8A #x55)

   (#x00 #x99 #xCC)
   (#x21 #x21 #x21)
   (#x09 #x09 #x09)
   (#x09 #x09 #x09)

   (#xFF #xFF #xFF)
   (#x0F #xD7 #xFF)
   (#x69 #xA2 #xFF)
   (#xD4 #x80 #xFF)

   (#xFF #x45 #xF3)
   (#xFF #x61 #x8B)
   (#xFF #x88 #x33)
   (#xFF #x9C #x12)

   (#xFA #xBC #x20)
   (#x9F #xE3 #x0E)
   (#x2B #xF0 #x35)
   (#x0C #xF0 #xA4)

   (#x05 #xFB #xFF)
   (#x5E #x5E #x5E)
   (#x0D #x0D #x0D)
   (#x0D #x0D #x0D)

   (#xFF #xFF #xFF)
   (#xA6 #xFC #xFF)
   (#xB3 #xEC #xFF)
   (#xDA #xAB #xEB)

   (#xFF #xA8 #xF9)
   (#xFF #xAB #xB3)
   (#xFF #xD2 #xB0)
   (#xFF #xEF #xA6)

   (#xFF #xF7 #x9C)
   (#xD7 #xE8 #x95)
   (#xA6 #xED #xAF)
   (#xA2 #xF2 #xDA)

   (#x99 #xFF #xFC)
   (#xDD #xDD #xDD)
   (#x11 #x11 #x11)
   (#x11 #x11 #x11)
   ])

(defconst nes/colors
  (map 'vector
       (lambda (color)
         (let ((normalize0 (/ (nth 0 color) 255.0))
               (normalize1 (/ (nth 1 color) 255.0))
               (normalize2 (/ (nth 2 color) 255.0)))
           `(
             ((glyph colorize)
              (t ? ))

             ((color-x color-x)
              (color-tty color-tty)
              (t mono-tty))

             (((glyph color-x) [,normalize0 ,normalize1 ,normalize2])
              (color-tty ,(format "#%X" (logior (lsh (nth 0 color) 16)
                                                (lsh (nth 1 color) 8)
                                                (nth 0 color))))
              )))
         )
       nes/color:COLORS
       ))

(provide 'nes-color)
