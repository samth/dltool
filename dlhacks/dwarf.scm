;;; Guile DWARF reader and writer

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Parts of this file were derived from sysdeps/generic/dwarf2.h, from
;; the GNU C Library.  That file is available under the LGPL version 2
;; or later, and is copyright:
;;
;; Copyright (C) 1992, 1993, 1995, 1996, 1997, 2000, 2011
;; 	Free Software Foundation, Inc.
;; Contributed by Gary Funck (gary@intrepid.com).  Derived from the
;; DWARF 1 implementation written by Ron Guilmette (rfg@monkeys.com).

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (dlhacks dwarf)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (system base target)
  #:use-module (dlhacks elf)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:export (elf->dwarf-context
            read-die-roots

            abbrev? abbrev-tag abbrev-has-children? abbrev-attrs abbrev-forms

            die? die-ctx die-offset die-abbrev die-vals die-children
            die-tag die-attrs die-forms die-ref

            ctx-parent-die ctx-start ctx-end

            read-die fold-die-list

            fold-die-children die->tree))

;;;
;;; First, define a number of constants.  The figures numbers refer to
;;; the DWARF 2.0 draft specification available on http://dwarfstd.org/.
;;; Extra codes not defined in that document are taken from the dwarf2
;;; header in glibc.
;;;

(define-syntax-rule (define-enumeration code->name (tag value) ...)
  (define code->name
    (let ((table (make-hash-table)))
      (hashv-set! table value 'tag)
      ...
      (lambda (v)
        (hashv-ref table v v)))))

;; Figures 14 and 15: Tag names and codes.
;;
(define-enumeration tag-code->name
  (padding #x00)
  (array-type #x01)
  (class-type #x02)
  (entry-point #x03)
  (enumeration-type #x04)
  (formal-parameter #x05)
  (imported-declaration #x08)
  (label #x0a)
  (lexical-block #x0b)
  (member #x0d)
  (pointer-type #x0f)
  (reference-type #x10)
  (compile-unit #x11)
  (string-type #x12)
  (structure-type #x13)
  (subroutine-type #x15)
  (typedef #x16)
  (union-type #x17)
  (unspecified-parameters #x18)
  (variant #x19)
  (common-block #x1a)
  (common-inclusion #x1b)
  (inheritance #x1c)
  (inlined-subroutine #x1d)
  (module #x1e)
  (ptr-to-member-type #x1f)
  (set-type #x20)
  (subrange-type #x21)
  (with-stmt #x22)
  (access-declaration #x23)
  (base-type #x24)
  (catch-block #x25)
  (const-type #x26)
  (constant #x27)
  (enumerator #x28)
  (file-type #x29)
  (friend #x2a)
  (namelist #x2b)
  (namelist-item #x2c)
  (packed-type #x2d)
  (subprogram #x2e)
  (template-type-param #x2f)
  (template-value-param #x30)
  (thrown-type #x31)
  (try-block #x32)
  (variant-part #x33)
  (variable #x34)
  (volatile-type #x35)
  (format-label #x4101)
  (function-template #x4102)
  (class-template #x4103)
  (GNU-BINCL #x4104)
  (GNU-EINCL #x4105)
  (lo-user #x4080)
  (hi-user #xffff))

;; Figure 16: Flag that tells whether entry has a child or not.
;;
(define-enumeration children-code->name
  (no 0)
  (yes 1))

;; Figures 17 and 18: Attribute names and codes.
;;
(define-enumeration attribute-code->name
  (sibling #x01)
  (location #x02)
  (name #x03)
  (ordering #x09)
  (subscr-data #x0a)
  (byte-size #x0b)
  (bit-offset #x0c)
  (bit-size #x0d)
  (element-list #x0f)
  (stmt-list #x10)
  (low-pc #x11)
  (high-pc #x12)
  (language #x13)
  (member #x14)
  (discr #x15)
  (discr-value #x16)
  (visibility #x17)
  (import #x18)
  (string-length #x19)
  (common-reference #x1a)
  (comp-dir #x1b)
  (const-value #x1c)
  (containing-type #x1d)
  (default-value #x1e)
  (inline #x20)
  (is-optional #x21)
  (lower-bound #x22)
  (producer #x25)
  (prototyped #x27)
  (return-addr #x2a)
  (start-scope #x2c)
  (stride-size #x2e)
  (upper-bound #x2f)
  (abstract-origin #x31)
  (accessibility #x32)
  (address-class #x33)
  (artificial #x34)
  (base-types #x35)
  (calling-convention #x36)
  (count #x37)
  (data-member-location #x38)
  (decl-column #x39)
  (decl-file #x3a)
  (decl-line #x3b)
  (declaration #x3c)
  (discr-list #x3d)
  (encoding #x3e)
  (external #x3f)
  (frame-base #x40)
  (friend #x41)
  (identifier-case #x42)
  (macro-info #x43)
  (namelist-items #x44)
  (priority #x45)
  (segment #x46)
  (specification #x47)
  (static-link #x48)
  (type #x49)
  (use-location #x4a)
  (variable-parameter #x4b)
  (virtuality #x4c)
  (vtable-elem-location #x4d)
  (sf-names #x2101)
  (src-info #x2102)
  (mac-info #x2103)
  (src-coords #x2104)
  (body-begin #x2105)
  (body-end #x2106)
  (lo-user #x2000)
  (hi-user #x3fff))

;; Figure 19: Form names and codes.
;;
(define-enumeration form-code->name
  (addr #x01)
  (block2 #x03)
  (block4 #x04)
  (data2 #x05)
  (data4 #x06)
  (data8 #x07)
  (string #x08)
  (block #x09)
  (block1 #x0a)
  (data1 #x0b)
  (flag #x0c)
  (sdata #x0d)
  (strp #x0e)
  (udata #x0f)
  (ref-addr #x10)
  (ref1 #x11)
  (ref2 #x12)
  (ref4 #x13)
  (ref8 #x14)
  (ref-udata #x15)
  (indirect #x16))

;; Figures 22 and 23: Location atom names and codes.
;;
(define-enumeration location-op->name
  (addr #x03)
  (deref #x06)
  (const1u #x08)
  (const1s #x09)
  (const2u #x0a)
  (const2s #x0b)
  (const4u #x0c)
  (const4s #x0d)
  (const8u #x0e)
  (const8s #x0f)
  (constu #x10)
  (consts #x11)
  (dup #x12)
  (drop #x13)
  (over #x14)
  (pick #x15)
  (swap #x16)
  (rot #x17)
  (xderef #x18)
  (abs #x19)
  (and #x1a)
  (div #x1b)
  (minus #x1c)
  (mod #x1d)
  (mul #x1e)
  (neg #x1f)
  (not #x20)
  (or #x21)
  (plus #x22)
  (plus-uconst #x23)
  (shl #x24)
  (shr #x25)
  (shra #x26)
  (xor #x27)
  (bra #x28)
  (eq #x29)
  (ge #x2a)
  (gt #x2b)
  (le #x2c)
  (lt #x2d)
  (ne #x2e)
  (skip #x2f)
  (lit0 #x30)
  (lit1 #x31)
  (lit2 #x32)
  (lit3 #x33)
  (lit4 #x34)
  (lit5 #x35)
  (lit6 #x36)
  (lit7 #x37)
  (lit8 #x38)
  (lit9 #x39)
  (lit10 #x3a)
  (lit11 #x3b)
  (lit12 #x3c)
  (lit13 #x3d)
  (lit14 #x3e)
  (lit15 #x3f)
  (lit16 #x40)
  (lit17 #x41)
  (lit18 #x42)
  (lit19 #x43)
  (lit20 #x44)
  (lit21 #x45)
  (lit22 #x46)
  (lit23 #x47)
  (lit24 #x48)
  (lit25 #x49)
  (lit26 #x4a)
  (lit27 #x4b)
  (lit28 #x4c)
  (lit29 #x4d)
  (lit30 #x4e)
  (lit31 #x4f)
  (reg0 #x50)
  (reg1 #x51)
  (reg2 #x52)
  (reg3 #x53)
  (reg4 #x54)
  (reg5 #x55)
  (reg6 #x56)
  (reg7 #x57)
  (reg8 #x58)
  (reg9 #x59)
  (reg10 #x5a)
  (reg11 #x5b)
  (reg12 #x5c)
  (reg13 #x5d)
  (reg14 #x5e)
  (reg15 #x5f)
  (reg16 #x60)
  (reg17 #x61)
  (reg18 #x62)
  (reg19 #x63)
  (reg20 #x64)
  (reg21 #x65)
  (reg22 #x66)
  (reg23 #x67)
  (reg24 #x68)
  (reg25 #x69)
  (reg26 #x6a)
  (reg27 #x6b)
  (reg28 #x6c)
  (reg29 #x6d)
  (reg30 #x6e)
  (reg31 #x6f)
  (breg0 #x70)
  (breg1 #x71)
  (breg2 #x72)
  (breg3 #x73)
  (breg4 #x74)
  (breg5 #x75)
  (breg6 #x76)
  (breg7 #x77)
  (breg8 #x78)
  (breg9 #x79)
  (breg10 #x7a)
  (breg11 #x7b)
  (breg12 #x7c)
  (breg13 #x7d)
  (breg14 #x7e)
  (breg15 #x7f)
  (breg16 #x80)
  (breg17 #x81)
  (breg18 #x82)
  (breg19 #x83)
  (breg20 #x84)
  (breg21 #x85)
  (breg22 #x86)
  (breg23 #x87)
  (breg24 #x88)
  (breg25 #x89)
  (breg26 #x8a)
  (breg27 #x8b)
  (breg28 #x8c)
  (breg29 #x8d)
  (breg30 #x8e)
  (breg31 #x8f)
  (regx #x90)
  (fbreg #x91)
  (bregx #x92)
  (piece #x93)
  (deref-size #x94)
  (xderef-size #x95)
  (nop #x96)
  (lo-user #x80)
  (hi-user #xff))

;; Figure 24: Type encodings.
;;
(define-enumeration type-encoding->name
  (void #x0)
  (address #x1)
  (boolean #x2)
  (complex-float #x3)
  (float #x4)
  (signed #x5)
  (signed-char #x6)
  (unsigned #x7)
  (unsigned-char #x8)
  (lo-user #x80)
  (hi-user #xff))

;; Figure 25: Access attribute.
;;
(define-enumeration access-code->name
  (public 1)
  (protected 2)
  (private 3))

;; Figure 26: Visibility.
;;
(define-enumeration visibility-code->name
  (local 1)
  (exported 2)
  (qualified 3))

;; Figure 27: Virtuality.
;;
(define-enumeration virtuality-code->name
  (none 0)
  (virtual 1)
  (pure-virtual 2))

;; Figure 28: Source language names and codes.
;;
(define-enumeration language-code->name
  (C89 #x0001)
  (C #x0002)
  (Ada83 #x0003)
  (C-plus-plus #x0004)
  (Cobol74 #x0005)
  (Cobol85 #x0006)
  (Fortran77 #x0007)
  (Fortran90 #x0008)
  (Pascal83 #x0009)
  (Modula2 #x000a)
  (Java #x000b)
  (Mips-Assembler #x8001)

  (lo-user #x8000)
  (hi-user #xffff))

;; Figure 29: Case sensitivity.
;;
(define-enumeration case-sensitivity-code->name
  (case-sensitive 0)
  (up-case 1)
  (down-case 2)
  (case-insensitive 3))

;; Figure 30: Calling convention.
;;
(define-enumeration calling-convention-code->name
  (normal #x1)
  (program #x2)
  (nocall #x3)
  (lo-user #x40)
  (hi-user #xff))

;; Figure 31: Inline attribute.
;;
(define-enumeration inline-code->name
  (not-inlined 0)
  (inlined 1)
  (declared-not-inlined 2)
  (declared-inlined 3))

;; Figure 32: Array ordering names and codes.
(define-enumeration ordering-code->name
  (row-major 0)
  (col-major 1))

;; Figure 33: Discriminant lists.
;;
(define-enumeration discriminant-code->name
  (label 0)
  (range 1))

;; Figure 34: "Standard" line number opcodes.
;;
(define-enumeration standard-line-opcode->name
  (extended-op 0)
  (copy 1)
  (advance-pc 2)
  (advance-line 3)
  (set-file 4)
  (set-column 5)
  (negate-stmt 6)
  (set-basic-block 7)
  (const-add-pc 8)
  (fixed-advance-pc 9))

;; Figure 35: "Extended" line number opcodes.
;;
(define-enumeration extended-line-opcode->name
  (end-sequence 1)
  (set-address 2)
  (define-file 3))

;; Figure 36: Names and codes for macro information.
;;
(define-enumeration macro-code->name
  (define 1)
  (undef 2)
  (start-file 3)
  (end-file 4)
  (vendor-ext 255))

;; Figure 37: Call frame information.
;;
(define-enumeration call-frame-address-code->name
  (advance-loc #x40)
  (offset #x80)
  (restore #xc0)
  (nop #x00)
  (set-loc #x01)
  (advance-loc1 #x02)
  (advance-loc2 #x03)
  (advance-loc4 #x04)
  (offset-extended #x05)
  (restore-extended #x06)
  (undefined #x07)
  (same-value #x08)
  (register #x09)
  (remember-state #x0a)
  (restore-state #x0b)
  (def-cfa #x0c)
  (def-cfa-register #x0d)
  (def-cfa-offset #x0e)
  (def-cfa-expression #x0f)
  (expression #x10)
  (offset-extended-sf #x11)
  (def-cfa-sf #x12)
  (def-cfa-offset-sf #x13)
  (GNU-window-save #x2d)
  (GNU-args-size #x2e)
  (GNU-negative-offset-extended #x2f)

  (extended 0)
  (low-user #x1c)
  (high-user #x3f))

;(define CIE-ID #xffffffff)
;(define CIE-VERSION 1)
;(define ADDR-none 0)


;;;
;;; A general configuration object.
;;;

(define-record-type <dwarf-meta>
  (make-dwarf-meta vaddr memsz
                   path lib-path
                   info-start info-end
                   abbrevs-start abbrevs-end
                   strtab-start strtab-end
                   loc-start loc-end)
  dwarf-meta?
  (vaddr meta-vaddr)
  (memsz meta-memsz)
  (path meta-path)
  (lib-path meta-lib-path)
  (info-start meta-info-start)
  (info-end meta-info-end)
  (abbrevs-start meta-abbrevs-start)
  (abbrevs-end meta-abbrevs-end)
  (strtab-start meta-strtab-start)
  (strtab-end meta-strtab-end)
  (loc-start meta-loc-start)
  (loc-end meta-loc-end))

(define-record-type <dwarf-context>
  (make-dwarf-context bv word-size endianness meta
                      abbrevs
                      parent start end)
  dwarf-context?
  (bv ctx-bv)
  (word-size ctx-word-size)
  (endianness ctx-endianness)
  (meta ctx-meta)
  (abbrevs ctx-abbrevs)
  (parent-die ctx-parent-die)
  (start ctx-start)
  (end ctx-end))


(set-record-type-printer! <dwarf-context>
                          (lambda (x port)
                            (format port "<dwarf-context ~a>"
                                    (number->string (object-address x) 16))))

;;;
;;; Procedures for reading DWARF data.
;;;

(define (read-u8 ctx pos)
  (values (bytevector-u8-ref (ctx-bv ctx) pos)
          (1+ pos)))
(define (skip-8 ctx pos)
  (+ pos 1))

(define (read-u16 ctx pos)
  (values (bytevector-u16-ref (ctx-bv ctx) pos (ctx-endianness ctx))
          (+ pos 2)))
(define (skip-16 ctx pos)
  (+ pos 2))

(define (read-u32 ctx pos)
  (values (bytevector-u32-ref (ctx-bv ctx) pos (ctx-endianness ctx))
          (+ pos 4)))
(define (skip-32 ctx pos)
  (+ pos 4))

(define (read-u64 ctx pos)
  (values (bytevector-u64-ref (ctx-bv ctx) pos (ctx-endianness ctx))
          (+ pos 8)))
(define (skip-64 ctx pos)
  (+ pos 8))

(define (read-addr ctx pos)
  (case (ctx-word-size ctx)
    ((4) (read-u32 ctx pos))
    ((8) (read-u64 ctx pos))
    (else (error "unsupported word size" ctx))))
(define (skip-addr ctx pos)
  (+ pos (ctx-word-size ctx)))

(define (%read-uleb128 bv pos)
  ;; Unrolled by one.
  (let ((b (bytevector-u8-ref bv pos)))
    (if (zero? (logand b #x80))
        (values b
                (1+ pos))
        (let lp ((n (logxor #x80 b)) (pos (1+ pos)) (shift 7))
          (let ((b (bytevector-u8-ref bv pos)))
            (if (zero? (logand b #x80))
                (values (logior (ash b shift) n)
                        (1+ pos))
                (lp (logior (ash (logxor #x80 b) shift) n)
                    (1+ pos)
                    (+ shift 7))))))))

(define (%read-sleb128 bv pos)
  (let lp ((n 0) (pos pos) (shift 0))
    (let ((b (bytevector-u8-ref bv pos)))
      (if (zero? (logand b #x80))
          (values (logior (ash b shift) n
                          (if (zero? (logand #x40 b))
                              0
                              (- (ash 1 (+ shift 7)))))
                  (1+ pos))
          (lp (logior (ash (logxor #x80 b) shift) n)
              (1+ pos)
              (+ shift 7))))))

(define (read-uleb128 ctx pos)
  (%read-uleb128 (ctx-bv ctx) pos))

(define (read-sleb128 ctx pos)
  (%read-sleb128 (ctx-bv ctx) pos))

(define (skip-leb128 ctx pos)
  (let ((bv (ctx-bv ctx)))
    (let lp ((pos pos))
      (let ((b (bytevector-u8-ref bv pos)))
        (if (zero? (logand b #x80))
            (1+ pos)
            (lp (1+ pos)))))))

(define (read-block ctx pos len)
  (let ((bv (make-bytevector len)))
    (bytevector-copy! (ctx-bv ctx) pos bv 0 len)
    (values bv
            (+ pos len))))

(define (read-string ctx pos)
  (let ((bv (ctx-bv ctx)))
    (let lp ((end pos))
      (if (zero? (bytevector-u8-ref bv end))
          (let ((out (make-bytevector (- end pos))))
            (bytevector-copy! bv pos out 0 (- end pos))
            (values (utf8->string out)
                    (1+ end)))
          (lp (1+ end))))))

(define (skip-string ctx pos)
  (let ((bv (ctx-bv ctx)))
    (let lp ((end pos))
      (if (zero? (bytevector-u8-ref bv end))
          (1+ end)
          (lp (1+ end))))))

(define-record-type <abbrev>
  (make-abbrev code tag has-children? attrs forms)
  abbrev?
  (code abbrev-code)
  (tag abbrev-tag)
  (has-children? abbrev-has-children?)
  (attrs abbrev-attrs)
  (forms abbrev-forms))

(define (read-abbrev ctx pos)
  (let*-values (((code pos) (read-uleb128 ctx pos))
                ((tag pos) (read-uleb128 ctx pos))
                ((children pos) (read-u8 ctx pos)))
    (let lp ((attrs '()) (forms '()) (pos pos))
      (let*-values (((attr pos) (read-uleb128 ctx pos))
                    ((form pos) (read-uleb128 ctx pos)))
        (if (and (zero? attr) (zero? form))
            (values (make-abbrev code
                                 (tag-code->name tag)
                                 (eq? (children-code->name children) 'yes)
                                 (reverse attrs)
                                 (reverse forms))
                    pos)
            (lp (cons (attribute-code->name attr) attrs)
                (cons (form-code->name form) forms)
                pos))))))

(define* (read-abbrevs ctx pos
                       #:optional (start (meta-abbrevs-start
                                          (ctx-meta ctx)))
                       (end (meta-abbrevs-end
                             (ctx-meta ctx))))
  (let lp ((abbrevs '()) (pos (+ start pos)) (max-code -1))
    (if (zero? (read-u8 ctx pos))
        (if (< pos end)
            (let ((av (make-vector (1+ max-code) #f)))
              (for-each (lambda (a)
                          (vector-set! av (abbrev-code a) a))
                        abbrevs)
              av)
            (error "Unexpected length" abbrevs pos start end))
        (let-values (((abbrev pos) (read-abbrev ctx pos)))
          (lp (cons abbrev abbrevs)
              pos
              (max (abbrev-code abbrev) max-code))))))

;; Values.
;;
(define *readers* (make-hash-table))
(define *scanners* (make-hash-table))
(define-syntax define-value-reader
  (syntax-rules ()
    ((_ form reader scanner)
     (begin
       (hashq-set! *readers* 'form reader)
       (hashq-set! *scanners* 'form scanner)))))

(define-value-reader addr read-addr skip-addr)

(define-value-reader block
  (lambda (ctx pos)
    (let-values (((len pos) (read-uleb128 ctx pos)))
      (read-block ctx pos len)))
  (lambda (ctx pos)
    (let-values (((len pos) (read-uleb128 ctx pos)))
      (+ pos len))))

(define-value-reader block1
  (lambda (ctx pos)
    (let-values (((len pos) (read-u8 ctx pos)))
      (read-block ctx pos len)))
  (lambda (ctx pos)
    (+ pos 1 (bytevector-u8-ref (ctx-bv ctx) pos))))

(define-value-reader block2
  (lambda (ctx pos)
    (let-values (((len pos) (read-u16 ctx pos)))
      (read-block ctx pos len)))
  (lambda (ctx pos)
    (+ pos 2 (bytevector-u16-ref (ctx-bv ctx) pos (ctx-endianness ctx)))))

(define-value-reader block4
  (lambda (ctx pos)
    (let-values (((len pos) (read-u32 ctx pos)))
      (read-block ctx pos len)))
  (lambda (ctx pos)
    (+ pos 4 (bytevector-u32-ref (ctx-bv ctx) pos (ctx-endianness ctx)))))

(define-value-reader data1 read-u8 skip-8)
(define-value-reader data2 read-u16 skip-16)
(define-value-reader data4 read-u32 skip-32)
(define-value-reader data8 read-u64 skip-64)
(define-value-reader udata read-uleb128 skip-leb128)
(define-value-reader sdata read-sleb128 skip-leb128)

(define-value-reader flag
  (lambda (ctx pos)
    (values (not (zero? (bytevector-u8-ref (ctx-bv ctx) pos)))
            (1+ pos)))
  skip-8)

(define-value-reader string
  read-string
  skip-string)

(define-value-reader strp
  (lambda (ctx pos)
    (let ((strtab (meta-strtab-start (ctx-meta ctx))))
      (unless strtab
        (error "expected a string table" ctx))
      (let-values (((offset pos) (read-u32 ctx pos)))
        (values (read-string ctx (+ strtab offset))
                pos))))
  skip-32)

(define-value-reader ref-addr
  (lambda (ctx pos)
    (let-values (((addr pos) (read-addr ctx pos)))
      (values (+ addr (meta-info-start (ctx-meta ctx)))
              pos)))
  skip-addr)

(define-value-reader ref1
  (lambda (ctx pos)
    (let-values (((addr pos) (read-u8 ctx pos)))
      (values (+ addr (ctx-start ctx))
              pos)))
  skip-8)

(define-value-reader ref2
  (lambda (ctx pos)
    (let-values (((addr pos) (read-u16 ctx pos)))
      (values (+ addr (ctx-start ctx))
              pos)))
  skip-16)

(define-value-reader ref4
  (lambda (ctx pos)
    (let-values (((addr pos) (read-u32 ctx pos)))
      (values (+ addr (ctx-start ctx))
              pos)))
  skip-32)

(define-value-reader ref8
  (lambda (ctx pos)
    (let-values (((addr pos) (read-u64 ctx pos)))
      (values (+ addr (ctx-start ctx))
              pos)))
  skip-64)

(define-value-reader ref
  (lambda (udata ctx pos)
    (let-values (((addr pos) (read-uleb128 ctx pos)))
      (values (+ addr (ctx-start ctx))
              pos)))
  skip-leb128)

(define-value-reader indirect
  (lambda (ctx pos)
    (let*-values (((form pos) (read-uleb128 ctx pos))
                  ((val pos) (read-value ctx pos (form-code->name form))))
      (values (cons form val)
              pos)))
  (lambda (ctx pos)
    (let*-values (((form pos) (read-uleb128 ctx pos)))
      (skip-value ctx pos (form-code->name form)))))

(define (read-value ctx pos form)
  ((or (hashq-ref *readers* form)
       (error "unrecognized form" form))
   ctx pos))

(define (skip-value ctx pos form)
  ((or (hashq-ref *scanners* form)
       (error "unrecognized form" form))
   ctx pos))

;; Parsers for particular attributes.
;;
(define (parse-location-list ctx offset)
  (let lp ((pos (+ (meta-loc-start (ctx-meta ctx)) offset))
           (out '()))
    (let*-values (((start pos) (read-addr ctx pos))
                  ((end pos) (read-addr ctx pos)))
      (if (and (zero? start) (zero? end))
          (reverse out)
          (let*-values (((len pos) (read-u16 ctx pos))
                        ((block pos) (read-block ctx pos len)))
            (lp pos
                (cons (list start end (parse-location ctx block)) out)))))))

(define (parse-location ctx loc)
  (cond
   ((bytevector? loc)
    (let ((len (bytevector-length loc))
          (word-size (ctx-word-size ctx))
          (endianness (ctx-endianness ctx)))
      (define (u8-ref pos) (bytevector-u8-ref loc pos))
      (define (s8-ref pos) (bytevector-s8-ref loc pos))
      (define (u16-ref pos) (bytevector-u16-ref loc pos endianness))
      (define (s16-ref pos) (bytevector-s16-ref loc pos endianness))
      (define (u32-ref pos) (bytevector-u32-ref loc pos endianness))
      (define (s32-ref pos) (bytevector-s32-ref loc pos endianness))
      (define (u64-ref pos) (bytevector-u64-ref loc pos endianness))
      (define (s64-ref pos) (bytevector-s64-ref loc pos endianness))
      (let lp ((pos 0) (out '()))
        (if (= pos len)
            (reverse out)
            (let ((op (location-op->name (u8-ref pos))))
              (case op
                ((addr)
                 (case word-size
                   ((4) (lp (+ pos 5) (cons (list op (u32-ref (1+ pos))) out)))
                   ((8) (lp (+ pos 9) (cons (list op (u64-ref (1+ pos))) out)))
                   (else (error "what!"))))
                ((const1u pick deref-size xderef-size)
                 (lp (+ pos 2) (cons (list op (u8-ref (1+ pos))) out)))
                ((const1s)
                 (lp (+ pos 2) (cons (list op (s8-ref (1+ pos))) out)))
                ((const2u)
                 (lp (+ pos 3) (cons (list op (u16-ref (1+ pos))) out)))
                ((const2s skip bra)
                 (lp (+ pos 3) (cons (list op (s16-ref (1+ pos))) out)))
                ((const4u)
                 (lp (+ pos 5) (cons (list op (u32-ref (1+ pos))) out)))
                ((const4s)
                 (lp (+ pos 5) (cons (list op (s32-ref (1+ pos))) out)))
                ((const8u)
                 (lp (+ pos 9) (cons (list op (u64-ref (1+ pos))) out)))
                ((const8s)
                 (lp (+ pos 9) (cons (list op (s64-ref (1+ pos))) out)))
                ((plus-uconst regx piece)
                 (let-values (((val pos) (%read-uleb128 loc (1+ pos))))
                   (lp pos (cons (list op val) out))))
                ((breg0 breg1 breg2 breg3 breg4 breg5 breg6 breg7 breg8 breg9
                        breg10 breg11 breg12 breg13 breg14 breg15 breg16 breg17
                        breg18 breg19 breg20 breg21 breg22 breg23 breg24 breg25
                        breg26 breg27 breg28 breg29 breg30 breg31 fbreg)
                 (let-values (((val pos) (%read-sleb128 loc (1+ pos))))
                   (lp pos (cons (list op val) out))))
                (else
                 (if (number? op)
                     ;; We failed to parse this opcode; we have to give
                     ;; up
                     loc
                     (lp (1+ pos) (cons (list op) out))))))))))
   (else
    (parse-location-list ctx loc))))

(define-syntax-rule (define-attribute-parsers parse (name parser) ...)
  (define parse
    (let ((parsers (make-hash-table)))
      (hashq-set! parsers 'name parser)
      ...
      (lambda (ctx attr val)
        (cond
         ((hashq-ref parsers attr) => (lambda (p) (p ctx val)))
         (else val))))))

(define-attribute-parsers parse-attribute
  (encoding (lambda (ctx val) (type-encoding->name val)))
  (accessibility (lambda (ctx val) (access-code->name val)))
  (visibility (lambda (ctx val) (visibility-code->name val)))
  (virtuality (lambda (ctx val) (virtuality-code->name val)))
  (language (lambda (ctx val) (language-code->name val)))
  (location parse-location)
  (data-member-location parse-location)
  (case-sensitive (lambda (ctx val) (case-sensitivity-code->name val)))
  (calling-convention (lambda (ctx val) (calling-convention-code->name val)))
  (inline (lambda (ctx val) (inline-code->name val)))
  (ordering (lambda (ctx val) (ordering-code->name val)))
  (discr-value (lambda (ctx val) (discriminant-code->name val))))

;; "Debugging Information Entries": DIEs.
;;
(define-record-type <die>
  (make-die ctx offset abbrev vals)
  die?
  (ctx die-ctx)
  (offset die-offset)
  (abbrev die-abbrev)
  (vals %die-vals %set-die-vals!))

(define (die-tag die)
  (abbrev-tag (die-abbrev die)))

(define (die-attrs die)
  (abbrev-attrs (die-abbrev die)))

(define (die-forms die)
  (abbrev-forms (die-abbrev die)))

(define (die-vals die)
  (let ((vals (%die-vals die)))
    (or vals
        (begin
          (%set-die-vals! die (read-values (die-ctx die) (skip-leb128 (die-ctx die) (die-offset die)) (die-abbrev die)))
          (die-vals die)))))

(define* (die-next-offset die #:optional offset-vals)
  (let ((ctx (die-ctx die)))
    (skip-values ctx (or offset-vals (skip-leb128 ctx (die-offset die)))
                 (die-abbrev die))))

(define* (die-ref die attr #:optional default)
  (cond
   ((list-index (die-attrs die) attr)
    => (lambda (n) (list-ref (die-vals die) n)))
   (else default)))

(define (read-values ctx offset abbrev)
  (let lp ((attrs (abbrev-attrs abbrev))
           (forms (abbrev-forms abbrev))
           (vals '())
           (pos offset))
    (if (null? forms)
        (values (reverse vals) pos)
        (let-values (((val pos) (read-value ctx pos (car forms))))
          (lp (cdr attrs) (cdr forms)
              (cons (parse-attribute ctx (car attrs) val) vals)
              pos)))))

(define (skip-values ctx offset abbrev)
  (let lp ((forms (abbrev-forms abbrev))
           (pos offset))
    (if (null? forms)
        pos
        (lp (cdr forms) (skip-value ctx pos (car forms))))))

(define (read-die-abbrev ctx offset)
  (let*-values (((code pos) (read-uleb128 ctx offset)))
    (values (cond ((zero? code) #f)
                  ((vector-ref (ctx-abbrevs ctx) code))
                  (else (error "unknown abbrev" ctx code)))
            pos
            #f pos)))

(define (read-die ctx offset)
  (let*-values (((abbrev pos) (read-die-abbrev ctx offset)))
    (if abbrev
        (values (make-die ctx offset abbrev #f)
                (skip-values ctx pos abbrev))
        (values #f pos))))

(define* (die-sibling ctx abbrev offset #:optional offset-vals offset-end)
  (cond
   ((not (abbrev-has-children? abbrev))
    (or offset-end
        (skip-values ctx
                     (or offset-vals (skip-leb128 ctx offset))
                     abbrev)))
   ((memq 'sibling (abbrev-attrs abbrev))
    (let lp ((offset (or offset-vals (skip-leb128 ctx offset)))
             (attrs (abbrev-attrs abbrev))
             (forms (abbrev-forms abbrev)))
      (if (eq? (car attrs) 'sibling)
          (read-value ctx offset (car forms))
          (lp (skip-value ctx offset (car forms))
              (cdr attrs) (cdr forms)))))
   (else
    (call-with-values
        (lambda ()
          (fold-die-list ctx
                         (or offset-end
                             (skip-values ctx
                                          (or offset-vals
                                              (skip-leb128 ctx offset))
                                          abbrev))
                         (lambda (ctx offset abbrev) #t)
                         error
                         #f))
      (lambda (seed pos)
        pos)))))

(define (fold-die-list ctx offset skip? proc seed)
  (let lp ((offset offset) (seed seed))
    (let-values (((abbrev pos) (read-die-abbrev ctx offset)))
      (cond
       ((not abbrev) (values seed pos))
       ((skip? ctx offset abbrev)
        (lp (die-sibling ctx abbrev offset pos) seed))
       (else
        (let-values (((vals pos) (read-values ctx pos abbrev)))
          (let* ((die (make-die ctx offset abbrev vals))
                 (seed (proc die seed)))
            (lp (die-sibling ctx abbrev offset #f pos) seed))))))))

(define (fold-die-children die skip? proc seed)
  (if (abbrev-has-children? (die-abbrev die))
      (values (fold-die-list (die-ctx die) (die-next-offset die)
                             skip? proc seed))
      seed))

(define (die-children die)
  (define (skip? ctx offset abbrev) #f)
  (reverse (fold-die-children die skip? cons '())))

(define (make-compilation-unit-context ctx abbrevs start len)
  (make-dwarf-context (ctx-bv ctx)
                      (ctx-word-size ctx) (ctx-endianness ctx)
                      (ctx-meta ctx)
                      abbrevs #f start (+ start 4 len)))

(define (read-compilation-unit ctx pos)
  (let*-values (((start) pos)
                ((len pos) (read-u32 ctx pos))
                ((version pos) (read-u16 ctx pos))
                ((abbrevs-offset pos) (read-u32 ctx pos))
                ((av) (read-abbrevs ctx abbrevs-offset))
                ((addrsize pos) (read-u8 ctx pos))
                ((ctx) (make-compilation-unit-context ctx av start len)))
    (values (read-die ctx pos)
            (ctx-end ctx))))

(define (read-die-roots ctx)
  (let lp ((dies '()) (pos (meta-info-start (ctx-meta ctx))))
    (if (< pos (meta-info-end (ctx-meta ctx)))
        (let-values (((die pos) (read-compilation-unit ctx pos)))
          (if die
              (lp (cons die dies) pos)
              (reverse dies)))
        (reverse dies))))

(define* (elf->dwarf-context elf #:key (vaddr 0) (memsz 0)
                             (path #f) (lib-path path))
  (let* ((sections (elf-sections-by-name elf))
         (info (assoc-ref sections ".debug_info"))
         (abbrevs (assoc-ref sections ".debug_abbrev"))
         (strtab (assoc-ref sections ".debug_str"))
         (loc (assoc-ref sections ".debug_loc")))
    (make-dwarf-context (elf-bytes elf)
                        (elf-word-size elf)
                        (elf-byte-order elf)
                        (make-dwarf-meta
                         vaddr memsz
                         path lib-path
                         (elf-section-offset info)
                         (+ (elf-section-offset info)
                            (elf-section-size info))
                         (elf-section-offset abbrevs)
                         (+ (elf-section-offset abbrevs)
                            (elf-section-size abbrevs))
                         (elf-section-offset strtab)
                         (+ (elf-section-offset strtab)
                            (elf-section-size strtab))
                         (elf-section-offset loc)
                         (+ (elf-section-offset loc)
                            (elf-section-size loc)))
                        #() #f 0 0)))

(define (die->tree die)
  (define (skip? ctx offset abbrev) #f)
  (cons* (die-tag die)
         (cons 'offset (die-offset die))
         (reverse! (fold-die-children
                    die
                    skip?
                    (lambda (die seed)
                      (cons (die->tree die) seed))
                    (fold acons '() (die-attrs die) (die-vals die))))))
