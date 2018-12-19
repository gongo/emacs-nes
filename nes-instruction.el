;; -*- lexical-binding: t -*-

(eval-when-compile (require 'cl))

(defconst nes/instruction:CYCLES
  [7 6 2 8 3 3 5 5 3 2 2 2 4 4 6 6   ;; 0x00
     2 5 2 8 4 4 6 6 2 4 2 7 4 4 6 7 ;; 0x10
     6 6 2 8 3 3 5 5 4 2 2 2 4 4 6 6 ;; 0x20
     2 5 2 8 4 4 6 6 2 4 2 7 4 4 6 7 ;; 0x30
     6 6 2 8 3 3 5 5 3 2 2 2 3 4 6 6 ;; 0x40
     2 5 2 8 4 4 6 6 2 4 2 7 4 4 6 7 ;; 0x50
     6 6 2 8 3 3 5 5 4 2 2 2 5 4 6 6 ;; 0x60
     2 5 2 8 4 4 6 6 2 4 2 7 4 4 6 7 ;; 0x70
     2 6 2 6 3 3 3 3 2 2 2 2 4 4 4 4 ;; 0x80
     2 6 2 6 4 4 4 4 2 4 2 5 5 4 5 5 ;; 0x90
     2 6 2 6 3 3 3 3 2 2 2 2 4 4 4 4 ;; 0xA0
     2 5 2 5 4 4 4 4 2 4 2 4 4 4 4 4 ;; 0xB0
     2 6 2 8 3 3 5 5 2 2 2 2 4 4 6 6 ;; 0xC0
     2 5 2 8 4 4 6 6 2 4 2 7 4 4 7 7 ;; 0xD0
     2 6 3 8 3 3 5 5 2 2 2 2 4 4 6 6 ;; 0xE0
     2 5 2 8 4 4 6 6 2 4 2 7 4 4 7 7 ;; 0xF0
     ]
  )

(defconst nes/instruction:MAP (make-vector 256 nil))

(defstruct (nes/instruction
            (:conc-name nes/instruction->))
  name
  func
  mode
  cycle)


(aset nes/instruction:MAP #xA9 (make-nes/instruction :func #'nes/instruction-lda :name "LDA" :mode :immediate))
(aset nes/instruction:MAP #xA5 (make-nes/instruction :func #'nes/instruction-lda :name "LDA" :mode :zero-page))
(aset nes/instruction:MAP #xAD (make-nes/instruction :func #'nes/instruction-lda :name "LDA" :mode :absolute))
(aset nes/instruction:MAP #xB5 (make-nes/instruction :func #'nes/instruction-lda :name "LDA" :mode :zero-page-x))
(aset nes/instruction:MAP #xBD (make-nes/instruction :func #'nes/instruction-lda :name "LDA" :mode :absolute-x))
(aset nes/instruction:MAP #xB9 (make-nes/instruction :func #'nes/instruction-lda :name "LDA" :mode :absolute-y))
(aset nes/instruction:MAP #xA1 (make-nes/instruction :func #'nes/instruction-lda :name "LDA" :mode :pre-indexed-indirect))
(aset nes/instruction:MAP #xB1 (make-nes/instruction :func #'nes/instruction-lda :name "LDA" :mode :post-indexed-indirect))
(aset nes/instruction:MAP #xA2 (make-nes/instruction :func #'nes/instruction-ldx :name "LDX" :mode :immediate))
(aset nes/instruction:MAP #xA6 (make-nes/instruction :func #'nes/instruction-ldx :name "LDX" :mode :zero-page))
(aset nes/instruction:MAP #xAE (make-nes/instruction :func #'nes/instruction-ldx :name "LDX" :mode :absolute))
(aset nes/instruction:MAP #xB6 (make-nes/instruction :func #'nes/instruction-ldx :name "LDX" :mode :zero-page-y))
(aset nes/instruction:MAP #xBE (make-nes/instruction :func #'nes/instruction-ldx :name "LDX" :mode :absolute-y))
(aset nes/instruction:MAP #xA0 (make-nes/instruction :func #'nes/instruction-ldy :name "LDY" :mode :immediate))
(aset nes/instruction:MAP #xA4 (make-nes/instruction :func #'nes/instruction-ldy :name "LDY" :mode :zero-page))
(aset nes/instruction:MAP #xAC (make-nes/instruction :func #'nes/instruction-ldy :name "LDY" :mode :absolute))
(aset nes/instruction:MAP #xB4 (make-nes/instruction :func #'nes/instruction-ldy :name "LDY" :mode :zero-page-x))
(aset nes/instruction:MAP #xBC (make-nes/instruction :func #'nes/instruction-ldy :name "LDY" :mode :absolute-x))

(aset nes/instruction:MAP #x85 (make-nes/instruction :func #'nes/instruction-sta :name "STA" :mode :zero-page))
(aset nes/instruction:MAP #x8D (make-nes/instruction :func #'nes/instruction-sta :name "STA" :mode :absolute))
(aset nes/instruction:MAP #x95 (make-nes/instruction :func #'nes/instruction-sta :name "STA" :mode :zero-page-x))
(aset nes/instruction:MAP #x9D (make-nes/instruction :func #'nes/instruction-sta :name "STA" :mode :absolute-x))
(aset nes/instruction:MAP #x99 (make-nes/instruction :func #'nes/instruction-sta :name "STA" :mode :absolute-y))
(aset nes/instruction:MAP #x81 (make-nes/instruction :func #'nes/instruction-sta :name "STA" :mode :pre-indexed-indirect))
(aset nes/instruction:MAP #x91 (make-nes/instruction :func #'nes/instruction-sta :name "STA" :mode :post-indexed-indirect))

(aset nes/instruction:MAP #x86 (make-nes/instruction :func #'nes/instruction-stx :name "STX" :mode :zero-page))
(aset nes/instruction:MAP #x8E (make-nes/instruction :func #'nes/instruction-stx :name "STX" :mode :absolute))
(aset nes/instruction:MAP #x96 (make-nes/instruction :func #'nes/instruction-stx :name "STX" :mode :zero-page-y))

(aset nes/instruction:MAP #x84 (make-nes/instruction :func #'nes/instruction-sty :name "STY" :mode :zero-page))
(aset nes/instruction:MAP #x8C (make-nes/instruction :func #'nes/instruction-sty :name "STY" :mode :absolute))
(aset nes/instruction:MAP #x94 (make-nes/instruction :func #'nes/instruction-sty :name "STY" :mode :zero-page-x))

(aset nes/instruction:MAP #x8A (make-nes/instruction :func #'nes/instruction-txa :name "TXA" :mode :implied))
(aset nes/instruction:MAP #x98 (make-nes/instruction :func #'nes/instruction-tya :name "TYA" :mode :implied))
(aset nes/instruction:MAP #x9A (make-nes/instruction :func #'nes/instruction-txs :name "TXS" :mode :implied))
(aset nes/instruction:MAP #xA8 (make-nes/instruction :func #'nes/instruction-tay :name "TAY" :mode :implied))
(aset nes/instruction:MAP #xAA (make-nes/instruction :func #'nes/instruction-tax :name "TAX" :mode :implied))
(aset nes/instruction:MAP #xBA (make-nes/instruction :func #'nes/instruction-tsx :name "TSX" :mode :implied))

(aset nes/instruction:MAP #x08 (make-nes/instruction :func #'nes/instruction-php :name "PHP" :mode :implied))
(aset nes/instruction:MAP #x28 (make-nes/instruction :func #'nes/instruction-plp :name "PLP" :mode :implied))
(aset nes/instruction:MAP #x48 (make-nes/instruction :func #'nes/instruction-pha :name "PHA" :mode :implied))
(aset nes/instruction:MAP #x68 (make-nes/instruction :func #'nes/instruction-pla :name "PLA" :mode :implied))

(aset nes/instruction:MAP #x69 (make-nes/instruction :func #'nes/instruction-adc :name "ADC" :mode :immediate))
(aset nes/instruction:MAP #x65 (make-nes/instruction :func #'nes/instruction-adc :name "ADC" :mode :zero-page))
(aset nes/instruction:MAP #x6D (make-nes/instruction :func #'nes/instruction-adc :name "ADC" :mode :absolute))
(aset nes/instruction:MAP #x75 (make-nes/instruction :func #'nes/instruction-adc :name "ADC" :mode :zero-page-x))
(aset nes/instruction:MAP #x7D (make-nes/instruction :func #'nes/instruction-adc :name "ADC" :mode :absolute-x))
(aset nes/instruction:MAP #x79 (make-nes/instruction :func #'nes/instruction-adc :name "ADC" :mode :absolute-y))
(aset nes/instruction:MAP #x61 (make-nes/instruction :func #'nes/instruction-adc :name "ADC" :mode :pre-indexed-indirect))
(aset nes/instruction:MAP #x71 (make-nes/instruction :func #'nes/instruction-adc :name "ADC" :mode :post-indexed-indirect))

(aset nes/instruction:MAP #xE9 (make-nes/instruction :func #'nes/instruction-sbc :name "SBC" :mode :immediate))
(aset nes/instruction:MAP #xE5 (make-nes/instruction :func #'nes/instruction-sbc :name "SBC" :mode :zero-page))
(aset nes/instruction:MAP #xED (make-nes/instruction :func #'nes/instruction-sbc :name "SBC" :mode :absolute))
(aset nes/instruction:MAP #xF5 (make-nes/instruction :func #'nes/instruction-sbc :name "SBC" :mode :zero-page-x))
(aset nes/instruction:MAP #xFD (make-nes/instruction :func #'nes/instruction-sbc :name "SBC" :mode :absolute-x))
(aset nes/instruction:MAP #xF9 (make-nes/instruction :func #'nes/instruction-sbc :name "SBC" :mode :absolute-y))
(aset nes/instruction:MAP #xE1 (make-nes/instruction :func #'nes/instruction-sbc :name "SBC" :mode :pre-indexed-indirect))
(aset nes/instruction:MAP #xF1 (make-nes/instruction :func #'nes/instruction-sbc :name "SBC" :mode :post-indexed-indirect))

(aset nes/instruction:MAP #xE0 (make-nes/instruction :func #'nes/instruction-cpx :name "CPX" :mode :immediate))
(aset nes/instruction:MAP #xE4 (make-nes/instruction :func #'nes/instruction-cpx :name "CPX" :mode :zero-page))
(aset nes/instruction:MAP #xEC (make-nes/instruction :func #'nes/instruction-cpx :name "CPX" :mode :absolute))
(aset nes/instruction:MAP #xC0 (make-nes/instruction :func #'nes/instruction-cpy :name "CPY" :mode :immediate))
(aset nes/instruction:MAP #xC4 (make-nes/instruction :func #'nes/instruction-cpy :name "CPY" :mode :zero-page))
(aset nes/instruction:MAP #xCC (make-nes/instruction :func #'nes/instruction-cpy :name "CPY" :mode :absolute))

(aset nes/instruction:MAP #xC9 (make-nes/instruction :func #'nes/instruction-cmp :name "CMP" :mode :immediate))
(aset nes/instruction:MAP #xC5 (make-nes/instruction :func #'nes/instruction-cmp :name "CMP" :mode :zero-page))
(aset nes/instruction:MAP #xCD (make-nes/instruction :func #'nes/instruction-cmp :name "CMP" :mode :absolute))
(aset nes/instruction:MAP #xD5 (make-nes/instruction :func #'nes/instruction-cmp :name "CMP" :mode :zero-page-x))
(aset nes/instruction:MAP #xDD (make-nes/instruction :func #'nes/instruction-cmp :name "CMP" :mode :absolute-x))
(aset nes/instruction:MAP #xD9 (make-nes/instruction :func #'nes/instruction-cmp :name "CMP" :mode :absolute-y))
(aset nes/instruction:MAP #xC1 (make-nes/instruction :func #'nes/instruction-cmp :name "CMP" :mode :pre-indexed-indirect))
(aset nes/instruction:MAP #xD1 (make-nes/instruction :func #'nes/instruction-cmp :name "CMP" :mode :post-indexed-indirect))

(aset nes/instruction:MAP #x29 (make-nes/instruction :func #'nes/instruction-and :name "AND" :mode :immediate))
(aset nes/instruction:MAP #x25 (make-nes/instruction :func #'nes/instruction-and :name "AND" :mode :zero-page))
(aset nes/instruction:MAP #x2D (make-nes/instruction :func #'nes/instruction-and :name "AND" :mode :absolute))
(aset nes/instruction:MAP #x35 (make-nes/instruction :func #'nes/instruction-and :name "AND" :mode :zero-page-x))
(aset nes/instruction:MAP #x3D (make-nes/instruction :func #'nes/instruction-and :name "AND" :mode :absolute-x))
(aset nes/instruction:MAP #x39 (make-nes/instruction :func #'nes/instruction-and :name "AND" :mode :absolute-y))
(aset nes/instruction:MAP #x21 (make-nes/instruction :func #'nes/instruction-and :name "AND" :mode :pre-indexed-indirect))
(aset nes/instruction:MAP #x31 (make-nes/instruction :func #'nes/instruction-and :name "AND" :mode :post-indexed-indirect))
(aset nes/instruction:MAP #x49 (make-nes/instruction :func #'nes/instruction-eor :name "EOR" :mode :immediate))
(aset nes/instruction:MAP #x45 (make-nes/instruction :func #'nes/instruction-eor :name "EOR" :mode :zero-page))
(aset nes/instruction:MAP #x4D (make-nes/instruction :func #'nes/instruction-eor :name "EOR" :mode :absolute))
(aset nes/instruction:MAP #x55 (make-nes/instruction :func #'nes/instruction-eor :name "EOR" :mode :zero-page-x))
(aset nes/instruction:MAP #x5D (make-nes/instruction :func #'nes/instruction-eor :name "EOR" :mode :absolute-x))
(aset nes/instruction:MAP #x59 (make-nes/instruction :func #'nes/instruction-eor :name "EOR" :mode :absolute-y))
(aset nes/instruction:MAP #x41 (make-nes/instruction :func #'nes/instruction-eor :name "EOR" :mode :pre-indexed-indirect))
(aset nes/instruction:MAP #x51 (make-nes/instruction :func #'nes/instruction-eor :name "EOR" :mode :post-indexed-indirect))
(aset nes/instruction:MAP #x09 (make-nes/instruction :func #'nes/instruction-ora :name "ORA" :mode :immediate))
(aset nes/instruction:MAP #x05 (make-nes/instruction :func #'nes/instruction-ora :name "ORA" :mode :zero-page))
(aset nes/instruction:MAP #x0D (make-nes/instruction :func #'nes/instruction-ora :name "ORA" :mode :absolute))
(aset nes/instruction:MAP #x15 (make-nes/instruction :func #'nes/instruction-ora :name "ORA" :mode :zero-page-x))
(aset nes/instruction:MAP #x1D (make-nes/instruction :func #'nes/instruction-ora :name "ORA" :mode :absolute-x))
(aset nes/instruction:MAP #x19 (make-nes/instruction :func #'nes/instruction-ora :name "ORA" :mode :absolute-y))
(aset nes/instruction:MAP #x01 (make-nes/instruction :func #'nes/instruction-ora :name "ORA" :mode :pre-indexed-indirect))
(aset nes/instruction:MAP #x11 (make-nes/instruction :func #'nes/instruction-ora :name "ORA" :mode :post-indexed-indirect))
(aset nes/instruction:MAP #x24 (make-nes/instruction :func #'nes/instruction-bit :name "BIT" :mode :zero-page))
(aset nes/instruction:MAP #x2C (make-nes/instruction :func #'nes/instruction-bit :name "BIT" :mode :absolute))
(aset nes/instruction:MAP #x0A (make-nes/instruction :func #'nes/instruction-asl :name "ASL" :mode :accumulator))
(aset nes/instruction:MAP #x06 (make-nes/instruction :func #'nes/instruction-asl :name "ASL" :mode :zero-page))
(aset nes/instruction:MAP #x0E (make-nes/instruction :func #'nes/instruction-asl :name "ASL" :mode :absolute))
(aset nes/instruction:MAP #x16 (make-nes/instruction :func #'nes/instruction-asl :name "ASL" :mode :zero-page-x))
(aset nes/instruction:MAP #x1E (make-nes/instruction :func #'nes/instruction-asl :name "ASL" :mode :absolute-x))
(aset nes/instruction:MAP #x4A (make-nes/instruction :func #'nes/instruction-lsr :name "LSR" :mode :accumulator))
(aset nes/instruction:MAP #x46 (make-nes/instruction :func #'nes/instruction-lsr :name "LSR" :mode :zero-page))
(aset nes/instruction:MAP #x4E (make-nes/instruction :func #'nes/instruction-lsr :name "LSR" :mode :absolute))
(aset nes/instruction:MAP #x56 (make-nes/instruction :func #'nes/instruction-lsr :name "LSR" :mode :zero-page-x))
(aset nes/instruction:MAP #x5E (make-nes/instruction :func #'nes/instruction-lsr :name "LSR" :mode :absolute-x))
(aset nes/instruction:MAP #x2A (make-nes/instruction :func #'nes/instruction-rol :name "ROL" :mode :accumulator))
(aset nes/instruction:MAP #x26 (make-nes/instruction :func #'nes/instruction-rol :name "ROL" :mode :zero-page))
(aset nes/instruction:MAP #x2E (make-nes/instruction :func #'nes/instruction-rol :name "ROL" :mode :absolute))
(aset nes/instruction:MAP #x36 (make-nes/instruction :func #'nes/instruction-rol :name "ROL" :mode :zero-page-x))
(aset nes/instruction:MAP #x3E (make-nes/instruction :func #'nes/instruction-rol :name "ROL" :mode :absolute-x))
(aset nes/instruction:MAP #x6A (make-nes/instruction :func #'nes/instruction-ror :name "ROR" :mode :accumulator))
(aset nes/instruction:MAP #x66 (make-nes/instruction :func #'nes/instruction-ror :name "ROR" :mode :zero-page))
(aset nes/instruction:MAP #x6E (make-nes/instruction :func #'nes/instruction-ror :name "ROR" :mode :absolute))
(aset nes/instruction:MAP #x76 (make-nes/instruction :func #'nes/instruction-ror :name "ROR" :mode :zero-page-x))
(aset nes/instruction:MAP #x7E (make-nes/instruction :func #'nes/instruction-ror :name "ROR" :mode :absolute-x))
(aset nes/instruction:MAP #xE8 (make-nes/instruction :func #'nes/instruction-inx :name "INX" :mode :implied))
(aset nes/instruction:MAP #xC8 (make-nes/instruction :func #'nes/instruction-iny :name "INY" :mode :implied))
(aset nes/instruction:MAP #xE6 (make-nes/instruction :func #'nes/instruction-inc :name "INC" :mode :zero-page))
(aset nes/instruction:MAP #xEE (make-nes/instruction :func #'nes/instruction-inc :name "INC" :mode :absolute))
(aset nes/instruction:MAP #xF6 (make-nes/instruction :func #'nes/instruction-inc :name "INC" :mode :zero-page-x))
(aset nes/instruction:MAP #xFE (make-nes/instruction :func #'nes/instruction-inc :name "INC" :mode :absolute-x))
(aset nes/instruction:MAP #xCA (make-nes/instruction :func #'nes/instruction-dex :name "DEX" :mode :implied))
(aset nes/instruction:MAP #x88 (make-nes/instruction :func #'nes/instruction-dey :name "DEY" :mode :implied))
(aset nes/instruction:MAP #xC6 (make-nes/instruction :func #'nes/instruction-dec :name "DEC" :mode :zero-page))
(aset nes/instruction:MAP #xCE (make-nes/instruction :func #'nes/instruction-dec :name "DEC" :mode :absolute))
(aset nes/instruction:MAP #xD6 (make-nes/instruction :func #'nes/instruction-dec :name "DEC" :mode :zero-page-x))
(aset nes/instruction:MAP #xDE (make-nes/instruction :func #'nes/instruction-dec :name "DEC" :mode :absolute-x))
(aset nes/instruction:MAP #x18 (make-nes/instruction :func #'nes/instruction-clc :name "CLC" :mode :implied))
(aset nes/instruction:MAP #x58 (make-nes/instruction :func #'nes/instruction-cli :name "CLI" :mode :implied))
(aset nes/instruction:MAP #xB8 (make-nes/instruction :func #'nes/instruction-clv :name "CLV" :mode :implied))
(aset nes/instruction:MAP #x38 (make-nes/instruction :func #'nes/instruction-sec :name "SEC" :mode :implied))
(aset nes/instruction:MAP #x78 (make-nes/instruction :func #'nes/instruction-sei :name "SEI" :mode :implied))
(aset nes/instruction:MAP #xEA (make-nes/instruction :func #'nes/instruction-nop :name "NOP" :mode :implied))

(aset nes/instruction:MAP #x00 (make-nes/instruction :func #'nes/instruction-brk :name "BRK" :mode :implied))

(aset nes/instruction:MAP #x20 (make-nes/instruction :func #'nes/instruction-jsr :name "JSR" :mode :absolute))
(aset nes/instruction:MAP #x4C (make-nes/instruction :func #'nes/instruction-jmp :name "JMP" :mode :absolute))
(aset nes/instruction:MAP #x6C (make-nes/instruction :func #'nes/instruction-jmp :name "JMP" :mode :indirect-absolute))
(aset nes/instruction:MAP #x40 (make-nes/instruction :func #'nes/instruction-rti :name "RTI" :mode :implied))
(aset nes/instruction:MAP #x60 (make-nes/instruction :func #'nes/instruction-rts :name "RTS" :mode :implied))
(aset nes/instruction:MAP #x10 (make-nes/instruction :func #'nes/instruction-bpl :name "BPL" :mode :relative))
(aset nes/instruction:MAP #x30 (make-nes/instruction :func #'nes/instruction-bmi :name "BMI" :mode :relative))
(aset nes/instruction:MAP #x50 (make-nes/instruction :func #'nes/instruction-bvc :name "BVC" :mode :relative))
(aset nes/instruction:MAP #x70 (make-nes/instruction :func #'nes/instruction-bvs :name "BVS" :mode :relative))
(aset nes/instruction:MAP #x90 (make-nes/instruction :func #'nes/instruction-bcc :name "BCC" :mode :relative))
(aset nes/instruction:MAP #xB0 (make-nes/instruction :func #'nes/instruction-bcs :name "BCS" :mode :relative))
(aset nes/instruction:MAP #xD0 (make-nes/instruction :func #'nes/instruction-bne :name "BNE" :mode :relative))
(aset nes/instruction:MAP #xF0 (make-nes/instruction :func #'nes/instruction-beq :name "BEQ" :mode :relative))
(aset nes/instruction:MAP #xF8 (make-nes/instruction :func #'nes/instruction-sed :name "SED" :mode :implied))
(aset nes/instruction:MAP #xD8 (make-nes/instruction :func #'nes/instruction-cld :name "CLD" :mode :implied))

;; Unofficial Opecodes

(aset nes/instruction:MAP #x02 (make-nes/instruction :func #'nes/instruction-nop :name "NOP" :mode :implied))
(aset nes/instruction:MAP #x12 (make-nes/instruction :func #'nes/instruction-nop :name "NOP" :mode :implied))
(aset nes/instruction:MAP #x1A (make-nes/instruction :func #'nes/instruction-nop :name "NOP" :mode :implied))
(aset nes/instruction:MAP #x22 (make-nes/instruction :func #'nes/instruction-nop :name "NOP" :mode :implied))
(aset nes/instruction:MAP #x32 (make-nes/instruction :func #'nes/instruction-nop :name "NOP" :mode :implied))
(aset nes/instruction:MAP #x3A (make-nes/instruction :func #'nes/instruction-nop :name "NOP" :mode :implied))
(aset nes/instruction:MAP #x42 (make-nes/instruction :func #'nes/instruction-nop :name "NOP" :mode :implied))
(aset nes/instruction:MAP #x52 (make-nes/instruction :func #'nes/instruction-nop :name "NOP" :mode :implied))
(aset nes/instruction:MAP #x5A (make-nes/instruction :func #'nes/instruction-nop :name "NOP" :mode :implied))
(aset nes/instruction:MAP #x62 (make-nes/instruction :func #'nes/instruction-nop :name "NOP" :mode :implied))
(aset nes/instruction:MAP #x72 (make-nes/instruction :func #'nes/instruction-nop :name "NOP" :mode :implied))
(aset nes/instruction:MAP #x7A (make-nes/instruction :func #'nes/instruction-nop :name "NOP" :mode :implied))
(aset nes/instruction:MAP #x92 (make-nes/instruction :func #'nes/instruction-nop :name "NOP" :mode :implied))
(aset nes/instruction:MAP #xB2 (make-nes/instruction :func #'nes/instruction-nop :name "NOP" :mode :implied))
(aset nes/instruction:MAP #xD2 (make-nes/instruction :func #'nes/instruction-nop :name "NOP" :mode :implied))
(aset nes/instruction:MAP #xDA (make-nes/instruction :func #'nes/instruction-nop :name "NOP" :mode :implied))
(aset nes/instruction:MAP #xF2 (make-nes/instruction :func #'nes/instruction-nop :name "NOP" :mode :implied))
(aset nes/instruction:MAP #xFA (make-nes/instruction :func #'nes/instruction-nop :name "NOP" :mode :implied))
(aset nes/instruction:MAP #x04 (make-nes/instruction :func #'nes/instruction-nopd :name "NOPD" :mode :implied))
(aset nes/instruction:MAP #x14 (make-nes/instruction :func #'nes/instruction-nopd :name "NOPD" :mode :implied))
(aset nes/instruction:MAP #x34 (make-nes/instruction :func #'nes/instruction-nopd :name "NOPD" :mode :implied))
(aset nes/instruction:MAP #x44 (make-nes/instruction :func #'nes/instruction-nopd :name "NOPD" :mode :implied))
(aset nes/instruction:MAP #x54 (make-nes/instruction :func #'nes/instruction-nopd :name "NOPD" :mode :implied))
(aset nes/instruction:MAP #x64 (make-nes/instruction :func #'nes/instruction-nopd :name "NOPD" :mode :implied))
(aset nes/instruction:MAP #x74 (make-nes/instruction :func #'nes/instruction-nopd :name "NOPD" :mode :implied))
(aset nes/instruction:MAP #x80 (make-nes/instruction :func #'nes/instruction-nopd :name "NOPD" :mode :implied))
(aset nes/instruction:MAP #x82 (make-nes/instruction :func #'nes/instruction-nopd :name "NOPD" :mode :implied))
(aset nes/instruction:MAP #x89 (make-nes/instruction :func #'nes/instruction-nopd :name "NOPD" :mode :implied))
(aset nes/instruction:MAP #xC2 (make-nes/instruction :func #'nes/instruction-nopd :name "NOPD" :mode :implied))
(aset nes/instruction:MAP #xD4 (make-nes/instruction :func #'nes/instruction-nopd :name "NOPD" :mode :implied))
(aset nes/instruction:MAP #xE2 (make-nes/instruction :func #'nes/instruction-nopd :name "NOPD" :mode :implied))
(aset nes/instruction:MAP #xF4 (make-nes/instruction :func #'nes/instruction-nopd :name "NOPD" :mode :implied))
(aset nes/instruction:MAP #x0C (make-nes/instruction :func #'nes/instruction-nopi :name "NOPI" :mode :implied))
(aset nes/instruction:MAP #x1C (make-nes/instruction :func #'nes/instruction-nopi :name "NOPI" :mode :implied))
(aset nes/instruction:MAP #x3C (make-nes/instruction :func #'nes/instruction-nopi :name "NOPI" :mode :implied))
(aset nes/instruction:MAP #x5C (make-nes/instruction :func #'nes/instruction-nopi :name "NOPI" :mode :implied))
(aset nes/instruction:MAP #x7C (make-nes/instruction :func #'nes/instruction-nopi :name "NOPI" :mode :implied))
(aset nes/instruction:MAP #xDC (make-nes/instruction :func #'nes/instruction-nopi :name "NOPI" :mode :implied))
(aset nes/instruction:MAP #xFC (make-nes/instruction :func #'nes/instruction-nopi :name "NOPI" :mode :implied))
(aset nes/instruction:MAP #xA7 (make-nes/instruction :func #'nes/instruction-lax :name "LAX" :mode :zero-page))
(aset nes/instruction:MAP #xB7 (make-nes/instruction :func #'nes/instruction-lax :name "LAX" :mode :zero-page-y))
(aset nes/instruction:MAP #xAF (make-nes/instruction :func #'nes/instruction-lax :name "LAX" :mode :absolute))
(aset nes/instruction:MAP #xBF (make-nes/instruction :func #'nes/instruction-lax :name "LAX" :mode :absolute-y))
(aset nes/instruction:MAP #xA3 (make-nes/instruction :func #'nes/instruction-lax :name "LAX" :mode :pre-indexed-indirect))
(aset nes/instruction:MAP #xB3 (make-nes/instruction :func #'nes/instruction-lax :name "LAX" :mode :post-indexed-indirect))
(aset nes/instruction:MAP #x87 (make-nes/instruction :func #'nes/instruction-sax :name "SAX" :mode :zero-page))
(aset nes/instruction:MAP #x97 (make-nes/instruction :func #'nes/instruction-sax :name "SAX" :mode :zero-page-y))
(aset nes/instruction:MAP #x8F (make-nes/instruction :func #'nes/instruction-sax :name "SAX" :mode :absolute))
(aset nes/instruction:MAP #x83 (make-nes/instruction :func #'nes/instruction-sax :name "SAX" :mode :pre-indexed-indirect))
(aset nes/instruction:MAP #xEB (make-nes/instruction :func #'nes/instruction-sbc :name "SBC" :mode :immediate))
(aset nes/instruction:MAP #xC7 (make-nes/instruction :func #'nes/instruction-dcp :name "DCP" :mode :zero-page))
(aset nes/instruction:MAP #xD7 (make-nes/instruction :func #'nes/instruction-dcp :name "DCP" :mode :zero-page-x))
(aset nes/instruction:MAP #xCF (make-nes/instruction :func #'nes/instruction-dcp :name "DCP" :mode :absolute))
(aset nes/instruction:MAP #xDF (make-nes/instruction :func #'nes/instruction-dcp :name "DCP" :mode :absolute-x))
(aset nes/instruction:MAP #xDB (make-nes/instruction :func #'nes/instruction-dcp :name "DCP" :mode :absolute-y))
(aset nes/instruction:MAP #xC3 (make-nes/instruction :func #'nes/instruction-dcp :name "DCP" :mode :pre-indexed-indirect))
(aset nes/instruction:MAP #xD3 (make-nes/instruction :func #'nes/instruction-dcp :name "DCP" :mode :post-indexed-indirect))
(aset nes/instruction:MAP #xE7 (make-nes/instruction :func #'nes/instruction-isb :name "ISB" :mode :zero-page))
(aset nes/instruction:MAP #xF7 (make-nes/instruction :func #'nes/instruction-isb :name "ISB" :mode :zero-page-x))
(aset nes/instruction:MAP #xEF (make-nes/instruction :func #'nes/instruction-isb :name "ISB" :mode :absolute))
(aset nes/instruction:MAP #xFF (make-nes/instruction :func #'nes/instruction-isb :name "ISB" :mode :absolute-x))
(aset nes/instruction:MAP #xFB (make-nes/instruction :func #'nes/instruction-isb :name "ISB" :mode :absolute-y))
(aset nes/instruction:MAP #xE3 (make-nes/instruction :func #'nes/instruction-isb :name "ISB" :mode :pre-indexed-indirect))
(aset nes/instruction:MAP #xF3 (make-nes/instruction :func #'nes/instruction-isb :name "ISB" :mode :post-indexed-indirect))
(aset nes/instruction:MAP #x07 (make-nes/instruction :func #'nes/instruction-slo :name "SLO" :mode :zero-page))
(aset nes/instruction:MAP #x17 (make-nes/instruction :func #'nes/instruction-slo :name "SLO" :mode :zero-page-x))
(aset nes/instruction:MAP #x0F (make-nes/instruction :func #'nes/instruction-slo :name "SLO" :mode :absolute))
(aset nes/instruction:MAP #x1F (make-nes/instruction :func #'nes/instruction-slo :name "SLO" :mode :absolute-x))
(aset nes/instruction:MAP #x1B (make-nes/instruction :func #'nes/instruction-slo :name "SLO" :mode :absolute-y))
(aset nes/instruction:MAP #x03 (make-nes/instruction :func #'nes/instruction-slo :name "SLO" :mode :pre-indexed-indirect))
(aset nes/instruction:MAP #x13 (make-nes/instruction :func #'nes/instruction-slo :name "SLO" :mode :post-indexed-indirect))
(aset nes/instruction:MAP #x27 (make-nes/instruction :func #'nes/instruction-rla :name "RLA" :mode :zero-page))
(aset nes/instruction:MAP #x37 (make-nes/instruction :func #'nes/instruction-rla :name "RLA" :mode :zero-page-x))
(aset nes/instruction:MAP #x2F (make-nes/instruction :func #'nes/instruction-rla :name "RLA" :mode :absolute))
(aset nes/instruction:MAP #x3F (make-nes/instruction :func #'nes/instruction-rla :name "RLA" :mode :absolute-x))
(aset nes/instruction:MAP #x3B (make-nes/instruction :func #'nes/instruction-rla :name "RLA" :mode :absolute-y))
(aset nes/instruction:MAP #x23 (make-nes/instruction :func #'nes/instruction-rla :name "RLA" :mode :pre-indexed-indirect))
(aset nes/instruction:MAP #x33 (make-nes/instruction :func #'nes/instruction-rla :name "RLA" :mode :post-indexed-indirect))
(aset nes/instruction:MAP #x47 (make-nes/instruction :func #'nes/instruction-sre :name "SRE" :mode :zero-page))
(aset nes/instruction:MAP #x57 (make-nes/instruction :func #'nes/instruction-sre :name "SRE" :mode :zero-page-x))
(aset nes/instruction:MAP #x4F (make-nes/instruction :func #'nes/instruction-sre :name "SRE" :mode :absolute))
(aset nes/instruction:MAP #x5F (make-nes/instruction :func #'nes/instruction-sre :name "SRE" :mode :absolute-x))
(aset nes/instruction:MAP #x5B (make-nes/instruction :func #'nes/instruction-sre :name "SRE" :mode :absolute-y))
(aset nes/instruction:MAP #x43 (make-nes/instruction :func #'nes/instruction-sre :name "SRE" :mode :pre-indexed-indirect))
(aset nes/instruction:MAP #x53 (make-nes/instruction :func #'nes/instruction-sre :name "SRE" :mode :post-indexed-indirect))
(aset nes/instruction:MAP #x67 (make-nes/instruction :func #'nes/instruction-rra :name "RRA" :mode :zero-page))
(aset nes/instruction:MAP #x77 (make-nes/instruction :func #'nes/instruction-rra :name "RRA" :mode :zero-page-x))
(aset nes/instruction:MAP #x6F (make-nes/instruction :func #'nes/instruction-rra :name "RRA" :mode :absolute))
(aset nes/instruction:MAP #x7F (make-nes/instruction :func #'nes/instruction-rra :name "RRA" :mode :absolute-x))
(aset nes/instruction:MAP #x7B (make-nes/instruction :func #'nes/instruction-rra :name "RRA" :mode :absolute-y))
(aset nes/instruction:MAP #x63 (make-nes/instruction :func #'nes/instruction-rra :name "RRA" :mode :pre-indexed-indirect))
(aset nes/instruction:MAP #x73 (make-nes/instruction :func #'nes/instruction-rra :name "RRA" :mode :post-indexed-indirect))


(provide 'nes-instruction)

;; Responding to recursive calls from nes-cpu
(require 'nes-cpu)

(defsubst nes/instruction--set-zero-and-negative-flags (r data)
  (setf (nes/cpu-register->sr-zero r) (zerop (logand data #xFF)))
  (setf (nes/cpu-register->sr-negative r) (nes--logbitp 7 data)))

(defun nes/instruction-lda (c addr-or-data mode)
  "Load Accumulator"
  (let ((data (if (eq mode :immediate)
                  addr-or-data
                (nes/cpu-read c addr-or-data)))
        (register (nes/cpu->register c)))
    (setf (nes/cpu-register->acc register) data)
    (nes/instruction--set-zero-and-negative-flags register data)))

(defun nes/instruction-ldx (c addr-or-data mode)
  "Load X Register"
  (let ((data (if (eq mode :immediate)
                  addr-or-data
                (nes/cpu-read c addr-or-data)))
        (register (nes/cpu->register c)))
    (setf (nes/cpu-register->idx-x register) data)
    (nes/instruction--set-zero-and-negative-flags register data)))

(defun nes/instruction-ldy (c addr-or-data mode)
  "Load Y Register"
  (let ((data (if (eq mode :immediate)
                  addr-or-data
                (nes/cpu-read c addr-or-data)))
        (register (nes/cpu->register c)))
    (setf (nes/cpu-register->idx-y register) data)
    (nes/instruction--set-zero-and-negative-flags register data)))

(defun nes/instruction-sta (c address _mode)
  "Store Accumulator"
  (nes/cpu-write c address (nes/cpu-register->acc (nes/cpu->register c))))

(defun nes/instruction-stx (c address _mode)
  "Store X Register"
  (nes/cpu-write c address (nes/cpu-register->idx-x (nes/cpu->register c))))

(defun nes/instruction-sty (c address _mode)
  "Store Y Register"
  (nes/cpu-write c address (nes/cpu-register->idx-y (nes/cpu->register c))))

(defun nes/instruction-tax (c _data _mode)
  "Transfer Accumulator to X"
  (let* ((register (nes/cpu->register c))
         (data (nes/cpu-register->acc register)))
    (setf (nes/cpu-register->idx-x register) data)
    (nes/instruction--set-zero-and-negative-flags register data)))

(defun nes/instruction-tay (c _data _mode)
  "Transfer Accumulator to Y"
  (let* ((register (nes/cpu->register c))
         (data (nes/cpu-register->acc register)))
    (setf (nes/cpu-register->idx-y register) data)
    (nes/instruction--set-zero-and-negative-flags register data)))

(defun nes/instruction-tsx (c _data _mode)
  "Transfer Stack Pointer to X"
  (let* ((register (nes/cpu->register c))
         (data (nes/cpu-register->sp register)))
    (setf (nes/cpu-register->idx-x register) data)
    (nes/instruction--set-zero-and-negative-flags register data)))

(defun nes/instruction-txa (c _data _mode)
  "Transfer X to Accumulator"
  (let* ((register (nes/cpu->register c))
         (data (nes/cpu-register->idx-x register)))
    (setf (nes/cpu-register->acc register) data)
    (nes/instruction--set-zero-and-negative-flags register data)))

(defun nes/instruction-txs (c _data _mode)
  "Transfer X to Stack Pointer"
  (let ((register (nes/cpu->register c)))
    (setf (nes/cpu-register->sp register)
          (nes/cpu-register->idx-x register))))

(defun nes/instruction-tya (c _data _mode)
  "Transfer Y to Accumulator"
  (let* ((register (nes/cpu->register c))
         (data (nes/cpu-register->idx-y register)))
    (setf (nes/cpu-register->acc register) data)
    (nes/instruction--set-zero-and-negative-flags register data)))

;;
;; In the byte pushed, bit 5 is always set to 1, and bit 4 is 1 if from an instruction (PHP or BRK)
;;
;; see https://wiki.nesdev.com/w/index.php/Status_flags#The_B_flag
;;
(defun nes/instruction-php (c _data _mode)
  "Push Processor Status"
  (let* ((r (nes/cpu->register c))
         (old-break (nes/cpu-register->sr-break r))
         (old-reserved (nes/cpu-register->sr-reserved r)))
    (setf (nes/cpu-register->sr-break r) t)
    (setf (nes/cpu-register->sr-reserved r) t)
    (nes/cpu-push-status-register c)
    (setf (nes/cpu-register->sr-break r) old-break)
    (setf (nes/cpu-register->sr-reserved r) old-reserved)))

(defun nes/instruction-pha (c _data _mode)
  "Push Accumulator"
  (nes/cpu-push c (nes/cpu-register->acc (nes/cpu->register c))))

(defun nes/instruction-pla (c _data _mode)
  "Pull Accumulator"
  (let ((register (nes/cpu->register c))
        (data (nes/cpu-pull c)))
    (setf (nes/cpu-register->acc register) data)
    (nes/instruction--set-zero-and-negative-flags register data)))

;;
;; Two instructions (PLP and RTI) pull a byte from the stack and set all the flags. They ignore bits 5 and 4.
;;
;; see https://wiki.nesdev.com/w/index.php/Status_flags#The_B_flag
;;
(defun nes/instruction-plp (c _data _mode)
  "Pull Processor Status"
  (let* ((r (nes/cpu->register c))
         (current-break (nes/cpu-register->sr-break r))
         (current-reserved (nes/cpu-register->sr-reserved r)))
    (nes/cpu-pull-status-register c)
    (setf (nes/cpu-register->sr-break r) current-break)
    (setf (nes/cpu-register->sr-reserved r) current-reserved)))

(defun nes/instruction-sec (c _data _mode)
  "Set Carry Flag"
  (setf (nes/cpu-register->sr-carry (nes/cpu->register c)) t))

(defun nes/instruction-sed (c _data _mode)
  "Set Decimal FLag"
  (setf (nes/cpu-register->sr-decimal (nes/cpu->register c)) t))

(defun nes/instruction-sei (c _data _mode)
  "Set Interrupt Disable"
  (setf (nes/cpu-register->sr-interrupt (nes/cpu->register c)) t))

(defun nes/instruction-clc (c _data _mode)
  "Clear Carry Flag"
  (setf (nes/cpu-register->sr-carry (nes/cpu->register c)) nil))

(defun nes/instruction-cld (c _data _mode)
  "Clear Decimal Mode"
  (setf (nes/cpu-register->sr-decimal (nes/cpu->register c)) nil))

(defun nes/instruction-cli (c _data _mode)
  "Clear Interrupt Disable"
  (setf (nes/cpu-register->sr-interrupt (nes/cpu->register c)) nil))

(defun nes/instruction-clv (c _data _mode)
  "Clear Overflow Flag"
  (setf (nes/cpu-register->sr-overflow (nes/cpu->register c)) nil))

(defun nes/instruction-inc (c address _mode)
  "Increment Memory"
  (let ((data (logand (1+ (nes/cpu-read c address)) #xff))
        (register (nes/cpu->register c)))
    (nes/cpu-write c address data)
    (nes/instruction--set-zero-and-negative-flags register data)))

(defun nes/instruction-inx (c _address _mode)
  "Increment X Register"
  (let* ((register (nes/cpu->register c))
         (data (logand (1+ (nes/cpu-register->idx-x register)) #xff)))
    (setf (nes/cpu-register->idx-x register) data)
    (nes/instruction--set-zero-and-negative-flags register data)))

(defun nes/instruction-iny (c _address _mode)
  "Increment Y Register"
  (let* ((register (nes/cpu->register c))
         (data (logand (1+ (nes/cpu-register->idx-y register)) #xff)))
    (setf (nes/cpu-register->idx-y register) data)
    (nes/instruction--set-zero-and-negative-flags register data)))

(defun nes/instruction-dec (c address _mode)
  "Decrement Memory"
  (let ((data (logand (1- (nes/cpu-read c address)) #xff))
        (register (nes/cpu->register c)))
    (nes/cpu-write c address data)
    (nes/instruction--set-zero-and-negative-flags register data)))

(defun nes/instruction-dex (c _address _mode)
  "Decrement X Register"
  (let* ((register (nes/cpu->register c))
         (data (logand (1- (nes/cpu-register->idx-x register)) #xff)))
    (setf (nes/cpu-register->idx-x register) data)
    (nes/instruction--set-zero-and-negative-flags register data)))

(defun nes/instruction-dey (c _address _mode)
  "Decrement Y Register"
  (let* ((register (nes/cpu->register c))
         (data (logand (1- (nes/cpu-register->idx-y register)) #xff)))
    (setf (nes/cpu-register->idx-y register) data)
    (nes/instruction--set-zero-and-negative-flags register data)))

(defun nes/instruction-adc (c addr-or-data mode)
  "Add with Carry"
  (let* ((register (nes/cpu->register c))
         (data (if (eq mode :immediate)
                   addr-or-data
                 (nes/cpu-read c addr-or-data)))
         (acc (nes/cpu-register->acc register))
         (result (+ acc
                    data
                    (if (nes/cpu-register->sr-carry register)
                        1 0))))
    (nes/instruction--set-zero-and-negative-flags register result)
    (setf (nes/cpu-register->sr-carry register) (> result #xff))
    (setf (nes/cpu-register->sr-overflow register)
          (and (not (nes--logbitp 7 (logxor acc data)))
               (nes--logbitp 7 (logxor acc result))))
    (setf (nes/cpu-register->acc register) (logand result #xFF))))

(defun nes/instruction-sbc (c addr-or-data mode)
  "Subtract with Carry"
  (let* ((register (nes/cpu->register c))
         (data (if (eq mode :immediate)
                   addr-or-data
                 (nes/cpu-read c addr-or-data)))
         (acc (nes/cpu-register->acc register))
         (result (- acc
                    data
                    (if (nes/cpu-register->sr-carry register)
                        0 1))))
    (nes/instruction--set-zero-and-negative-flags register result)
    (setf (nes/cpu-register->sr-carry register) (>= result 0))
    (setf (nes/cpu-register->sr-overflow register)
          (and (nes--logbitp 7 (logxor acc data))
               (nes--logbitp 7 (logxor acc result))))
    (setf (nes/cpu-register->acc register) (logand result #xFF))))

(defun nes/instruction-cmp (c addr-or-data mode)
  "Compare"
  (let* ((register (nes/cpu->register c))
         (data (if (eq mode :immediate)
                   addr-or-data
                 (nes/cpu-read c addr-or-data)))
         (acc (nes/cpu-register->acc register))
         (compared (- acc data)))
    (nes/instruction--set-zero-and-negative-flags register compared)
    (setf (nes/cpu-register->sr-carry register) (>= compared 0))))

(defun nes/instruction-cpx (c addr-or-data mode)
  "Compare X Register"
  (let* ((register (nes/cpu->register c))
         (data (if (eq mode :immediate)
                   addr-or-data
                 (nes/cpu-read c addr-or-data)))
         (x (nes/cpu-register->idx-x register))
         (compared (- x data)))
    (nes/instruction--set-zero-and-negative-flags register compared)
    (setf (nes/cpu-register->sr-carry register) (>= compared 0))))

(defun nes/instruction-cpy (c addr-or-data mode)
  "Compare Y Register"
  (let* ((register (nes/cpu->register c))
         (data (if (eq mode :immediate)
                   addr-or-data
                 (nes/cpu-read c addr-or-data)))
         (y (nes/cpu-register->idx-y register))
         (compared (- y data)))
    (nes/instruction--set-zero-and-negative-flags register compared)
    (setf (nes/cpu-register->sr-carry register) (>= compared 0))))

(defun nes/instruction-and (c addr-or-data mode)
  "Logical AND"
  (let* ((register (nes/cpu->register c))
         (data (if (eq mode :immediate)
                   addr-or-data
                 (nes/cpu-read c addr-or-data)))
         (acc (nes/cpu-register->acc register))
         (result (logand acc data)))
    (nes/instruction--set-zero-and-negative-flags register result)
    (setf (nes/cpu-register->acc register) (logand result #xff))))

(defun nes/instruction-ora (c addr-or-data mode)
  "Logical Inclusive OR"
  (let* ((register (nes/cpu->register c))
         (data (if (eq mode :immediate)
                   addr-or-data
                 (nes/cpu-read c addr-or-data)))
         (acc (nes/cpu-register->acc register))
         (result (logior acc data)))
    (nes/instruction--set-zero-and-negative-flags register result)
    (setf (nes/cpu-register->acc register) (logand result #xff))))

(defun nes/instruction-eor (c addr-or-data mode)
  "Exclusive OR"
  (let* ((register (nes/cpu->register c))
         (data (if (eq mode :immediate)
                   addr-or-data
                 (nes/cpu-read c addr-or-data)))
         (acc (nes/cpu-register->acc register))
         (result (logxor acc data)))
    (nes/instruction--set-zero-and-negative-flags register result)
    (setf (nes/cpu-register->acc register) (logand result #xff))))

(defun nes/instruction-asl (c address mode)
  "Arithmetic Shift Left"
  (let* ((register (nes/cpu->register c))
         (data (if (eq mode :accumulator)
                   (nes/cpu-register->acc register)
                 (nes/cpu-read c address)))
         (shifted (logand (lsh data 1) #xff)))
    (setf (nes/cpu-register->sr-carry register) (nes--logbitp 7 data))
    (nes/instruction--set-zero-and-negative-flags register shifted)
    (if (eq mode :accumulator)
        (setf (nes/cpu-register->acc register) shifted)
      (nes/cpu-write c address shifted))))

(defun nes/instruction-lsr (c address mode)
  "Logical Shift Right"
  (let* ((register (nes/cpu->register c))
         (data (logand (if (eq mode :accumulator)
                           (nes/cpu-register->acc register)
                         (nes/cpu-read c address))))
         (shifted (logand (lsh data -1) #xff)))
    (setf (nes/cpu-register->sr-carry register) (nes--logbitp 0 data))
    (nes/instruction--set-zero-and-negative-flags register shifted)
    (if (eq mode :accumulator)
        (setf (nes/cpu-register->acc register) shifted)
      (nes/cpu-write c address shifted))))

(defun nes/instruction-rol (c address mode)
  "Rotate Left"
  (let* ((register (nes/cpu->register c))
         (data (logand (if (eq mode :accumulator)
                           (nes/cpu-register->acc register)
                         (nes/cpu-read c address))))
         (carry (nes/cpu-register->sr-carry register))
         (rotated (logand #xff
                          (logior (lsh data 1) (if carry #x01 #x00)))))
    (setf (nes/cpu-register->sr-carry register) (nes--logbitp 7 data))
    (nes/instruction--set-zero-and-negative-flags register rotated)
    (if (eq mode :accumulator)
        (setf (nes/cpu-register->acc register) rotated)
      (nes/cpu-write c address rotated))))

(defun nes/instruction-ror (c address mode)
  "Rotate Right"
  (let* ((register (nes/cpu->register c))
         (data (logand (if (eq mode :accumulator)
                           (nes/cpu-register->acc register)
                         (nes/cpu-read c address))))
         (carry (nes/cpu-register->sr-carry register))
         (rotated (logand #xff
                          (logior (lsh data -1) (if carry #x80 #x00)))))
    (setf (nes/cpu-register->sr-carry register) (/= (logand data #x01) 0))
    (nes/instruction--set-zero-and-negative-flags register rotated)
    (if (eq mode :accumulator)
        (setf (nes/cpu-register->acc register) rotated)
      (nes/cpu-write c address rotated))))


(defun nes/instruction-jmp (c address _mode)
  "Jump"
  (setf (nes/cpu-register->pc (nes/cpu->register c)) address))

(defun nes/instruction-nop (_c _address _mode)
  "No Operation"
  )

(defun nes/instruction-bmi (c address _mode)
  (let ((register (nes/cpu->register c)))
    (when (nes/cpu-register->sr-negative register)
      (setf (nes/cpu->cycles c) (logand (1+ (nes/cpu->cycles c)) #xFFFF))
      (setf (nes/cpu-register->pc register) address))))

(defun nes/instruction-bpl (c address _mode)
  (let ((register (nes/cpu->register c)))
    (unless (nes/cpu-register->sr-negative register)
      (setf (nes/cpu->cycles c) (logand (1+ (nes/cpu->cycles c)) #xFFFF))
      (setf (nes/cpu-register->pc register) address))))

(defun nes/instruction-bvs (c address _mode)
  (let ((register (nes/cpu->register c)))
    (when (nes/cpu-register->sr-overflow register)
      (setf (nes/cpu->cycles c) (logand (1+ (nes/cpu->cycles c)) #xFFFF))
      (setf (nes/cpu-register->pc register) address))))

(defun nes/instruction-bvc (c address _mode)
  (let ((register (nes/cpu->register c)))
    (unless (nes/cpu-register->sr-overflow register)
      (setf (nes/cpu->cycles c) (logand (1+ (nes/cpu->cycles c)) #xFFFF))
      (setf (nes/cpu-register->pc register) address))))

(defun nes/instruction-beq (c address _mode)
  (let ((register (nes/cpu->register c)))
    (when (nes/cpu-register->sr-zero register)
      (setf (nes/cpu->cycles c) (logand (1+ (nes/cpu->cycles c)) #xFFFF))
      (setf (nes/cpu-register->pc register) address))))

(defun nes/instruction-bne (c address _mode)
  (let ((register (nes/cpu->register c)))
    (unless (nes/cpu-register->sr-zero register)
      (setf (nes/cpu->cycles c) (logand (1+ (nes/cpu->cycles c)) #xFFFF))
      (setf (nes/cpu-register->pc register) address))))

(defun nes/instruction-bcs (c address _mode)
  (let ((register (nes/cpu->register c)))
    (when (nes/cpu-register->sr-carry register)
      (setf (nes/cpu->cycles c) (logand (1+ (nes/cpu->cycles c)) #xFFFF))
      (setf (nes/cpu-register->pc register) address))))

(defun nes/instruction-bcc (c address _mode)
  (let ((register (nes/cpu->register c)))
    (unless (nes/cpu-register->sr-carry register)
      (setf (nes/cpu->cycles c) (logand (1+ (nes/cpu->cycles c)) #xFFFF))
      (setf (nes/cpu-register->pc register) address))))

(defun nes/instruction-brk (c _address _mode)
  (let* ((register (nes/cpu->register c))
         (interrupt (nes/cpu-register->sr-interrupt register)))
    (setf (nes/cpu-register->sr-break register) t)
    (incf (nes/cpu-register->pc register))
    (nes/cpu-push c (logand #xFF (lsh (nes/cpu-register->pc register) -8)))
    (nes/cpu-push c (logand #xFF (nes/cpu-register->pc register)))
    (nes/cpu-push-status-register c)
    (unless interrupt
      (setf (nes/cpu-register->sr-interrupt register) t)
      (setf (nes/cpu-register->pc register) (nes/cpu-read c #xFFFE :word)))
    (decf (nes/cpu-register->pc register))))

(defun nes/instruction-jsr (c address _mode)
  (let* ((register (nes/cpu->register c))
         (pc (1- (nes/cpu-register->pc register))))
    (nes/cpu-push c (logand (lsh pc -8) #xFF))
    (nes/cpu-push c (logand pc #xFF))
    (setf (nes/cpu-register->pc register) address)))

(defun nes/instruction-rts (c _address _mode)
  (let ((register (nes/cpu->register c))
        pc)
    (setq pc (logior (nes/cpu-pull c)
                     (lsh (nes/cpu-pull c) 8)))
    (setf (nes/cpu-register->pc register) (1+ pc))))

(defun nes/instruction-rti (c _address _mode)
  (nes/cpu-pull-status-register c)
  (let ((register (nes/cpu->register c)))
    (setf (nes/cpu-register->sr-reserved register) t)
    (setf (nes/cpu-register->pc register)
          (logior (nes/cpu-pull c)
                  (lsh (nes/cpu-pull c) 8)))))

(defun nes/instruction-bit (c address _mode)
  (let* ((data (nes/cpu-read c address))
         (register (nes/cpu->register c)))
    (setf (nes/cpu-register->sr-negative register) (nes--logbitp 7 data))
    (setf (nes/cpu-register->sr-overflow register) (nes--logbitp 6 data))
    (setf (nes/cpu-register->sr-zero register) (zerop (logand (nes/cpu-register->acc register) data)))
    ))

;; Unofficial Opecodes

(defun nes/instruction-nopd (c _address _mode)
  (let ((r (nes/cpu->register c)))
    (setf (nes/cpu-register->pc r) (1+ (nes/cpu-register->pc r)))))

(defun nes/instruction-nopi (c _address _mode)
  (let ((r (nes/cpu->register c)))
    (setf (nes/cpu-register->pc r) (+ (nes/cpu-register->pc r) 2))))

(defun nes/instruction-lax (c address _mode)
  (let ((data (nes/cpu-read c address))
        (register (nes/cpu->register c)))
    (setf (nes/cpu-register->idx-x register) data)
    (setf (nes/cpu-register->acc register) data)
    (nes/instruction--set-zero-and-negative-flags register data)))

(defun nes/instruction-sax (c address _mode)
  (let ((r (nes/cpu->register c)))
    (nes/cpu-write c address
                   (logand (nes/cpu-register->acc r)
                           (nes/cpu-register->idx-x r)))))

(defun nes/instruction-dcp (c address _mode)
  (let* ((reg (nes/cpu->register c))
         (operated (logand (1- (nes/cpu-read c address)) #xFF)))
    (setf (nes/cpu-register->sr-negative reg)
          (/= (logand (logand (- (nes/cpu-register->acc reg) operated) #x1FF) #x80) 0))
    (setf (nes/cpu-register->sr-zero reg)
          (= (logand (- (nes/cpu-register->acc reg) operated) #x1FF) 0))
    (nes/cpu-write c address operated)))

(defun nes/instruction-isb (c addr _mode)
  (let* ((data (logand (1+ (nes/cpu-read c addr)) #xFF))
         (reg (nes/cpu->register c))
         (operated (+ (logand (lognot data) #xFF)
                      (nes/cpu-register->acc reg)
                      (if (nes/cpu-register->sr-carry reg) 1 0))))
    (setf (nes/cpu-register->sr-overflow reg)
          (and (zerop (logand (logxor (nes/cpu-register->acc reg) data) #x80))
               (not (zerop (logand (logxor (nes/cpu-register->acc reg) operated) #x80)))))
    (setf (nes/cpu-register->sr-carry reg)
          (> operated #xFF))
    (nes/instruction--set-zero-and-negative-flags reg operated)
    (setf (nes/cpu-register->acc reg) (logand operated #xFF))
    (nes/cpu-write c addr data)))

(defun nes/instruction-slo (c addr _mode)
  (let* ((data (nes/cpu-read c addr))
         (reg (nes/cpu->register c)))
    (setf (nes/cpu-register->sr-carry reg) (nes--logbitp 7 data))
    (setq data (logand (lsh data 1) #xFF))
    (setf (nes/cpu-register->acc reg)
          (logior (nes/cpu-register->acc reg) data))
    (setf (nes/cpu-register->sr-negative reg)
          (nes--logbitp 7 (nes/cpu-register->acc reg)))
    (setf (nes/cpu-register->sr-zero reg)
          (= (logand (nes/cpu-register->acc reg) #xFF) 0))
    (nes/cpu-write c addr data)))


(defun nes/instruction-rla (c addr _mode)
  (let* ((data (nes/cpu-read c addr))
         (reg (nes/cpu->register c)))
    (setq data (+ (lsh data 1)
                  (if (nes/cpu-register->sr-carry reg) 1 0)))
    (setf (nes/cpu-register->sr-carry reg)
          (nes--logbitp 8 data))
    (setf (nes/cpu-register->acc reg)
          (logand (logand (nes/cpu-register->acc reg) data) #xFF))
    (setf (nes/cpu-register->sr-negative reg)
          (nes--logbitp 7 (nes/cpu-register->acc reg)))
    (setf (nes/cpu-register->sr-zero reg)
          (= (logand (nes/cpu-register->acc reg) #xFF) 0))
    (nes/cpu-write c addr data)))

(defun nes/instruction-sre (c addr _mode)
  (let* ((data (nes/cpu-read c addr))
         (reg (nes/cpu->register c)))
    (setf (nes/cpu-register->sr-carry reg)
          (nes--logbitp 0 data))
    (setq data (lsh data -1))
    (setf (nes/cpu-register->acc reg)
          (logxor (nes/cpu-register->acc reg) data))
    (setf (nes/cpu-register->sr-negative reg)
          (nes--logbitp 7 (nes/cpu-register->acc reg)))
    (setf (nes/cpu-register->sr-zero reg)
          (= (logand (nes/cpu-register->acc reg) #xFF) 0))
    (nes/cpu-write c addr data)))

(defun nes/instruction-rra (c addr _mode)
  (let* ((data (nes/cpu-read c addr))
         (reg (nes/cpu->register c))
         (carry (nes--logbitp 0 data))
         operated)
    (setq data (logior (lsh data -1)
                       (if (nes/cpu-register->sr-carry reg) #x80 #x00)))
    (setq operated (+ data (nes/cpu-register->acc reg) (if carry 1 0)))

    (setf (nes/cpu-register->sr-overflow reg)
          (and (not (nes--logbitp 7 (logxor (nes/cpu-register->acc reg) data)))
               (nes--logbitp 7 (logxor (nes/cpu-register->acc reg) operated))))

    (setf (nes/cpu-register->sr-carry reg)
          (> operated #xFF))
    (setf (nes/cpu-register->sr-negative reg)
          (nes--logbitp 7 operated))
    (setf (nes/cpu-register->sr-zero reg)
          (= (logand operated #xFF) 0))
    (setf (nes/cpu-register->acc reg) (logand operated #xFF))
    (nes/cpu-write c addr data)))
