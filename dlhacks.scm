;; guile-dlhacks
;; Copyright (C) 2012 Andy Wingo <wingo at pobox dot com>

;; This library is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;                                                                  
;; This library is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;                                                                  
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;; Various utilities for groveling shared libraries, currently on GNU
;; systems only, for the purpose of extracting and operating on their
;; debugging information.
;;
;;; Code:

(define-module (dlhacks)
  #:use-module (dlhacks elf)
  #:use-module (dlhacks dwarf)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 vlist)
  #:use-module ((ice-9 i18n) #:select (string-locale<?))
  #:use-module (rnrs bytevectors)
  #:use-module ((srfi srfi-1) #:hide (list-index))
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (global-debug-path
            system-library-search-path
            find-library
            find-debug-object))

(define global-debug-path
  (make-parameter "/usr/lib/debug"))

(define (scandir path selector)
  (let ((dir (opendir path)))
    (let lp ((out '()))
      (let ((ent (readdir dir)))
        (if (eof-object? ent)
            (sort out string-locale<?)
            (lp (if (selector ent)
                    (cons ent out)
                    out)))))))

(define (join-path head)
  (string-join (reverse head) "/"))

(define (glob? s)
  (or (string-index s #\[)
      (string-index s #\*)
      (string-index s #\?)))

(define (glob->regexp s)
  (call-with-output-string
   (lambda (p)
     (write-char #\^ p)
     (with-input-from-string s
       (lambda ()
         (let lp ()
           (let ((c (read-char)))
             (unless (eof-object? c)
               (case c
                 ((#\[ #\?)
                  (write-char c p))
                 ((#\*)
                  (write-char #\. p)
                  (write-char c p))
                 ((#\\)
                  (write-char c p)
                  (let ((c (read-char)))
                    (unless (eof-object? c)
                      (write-char c p))))
                 ((#\. #\^ #\$ #\( #\) #\+ #\{ #\} #\|)
                  (write-char #\[ p)
                  (write-char c p)
                  (write-char #\] p))
                 (else
                  (write-char c p)))
               (lp))))))
     (write-char #\$ p))))

(define (compute-matches head s)
  (if (glob? s)
      (let ((pat (make-regexp (glob->regexp s)))
            (dir (join-path head)))
        (catch #t
          (lambda ()
            (scandir dir
                     (lambda (s)
                       (and (not (equal? s "."))
                            (not (equal? s ".."))
                            (regexp-exec pat s)
                            s))))
          (lambda _ '())))
      (list s)))

(define (expand-glob s)
  (let lp ((head '()) (tail (string-split s #\/)))
    (match tail
      (()
       (let ((path (join-path head)))
         (if (file-exists? path)
             (list path)
             '())))
      ((pat . tail)
       (append-map (lambda (elt)
                     (lp (cons elt head) tail))
                   (compute-matches head pat))))))

(define (load-conf f)
  (call-with-input-file f
    (lambda (p)
      (let lp ((paths '()))
        (let ((s (read-line p)))
          (if (eof-object? s)
              (reverse paths)
              (let ((s (string-trim-both s)))
                (cond
                 ((string-null? s) (lp paths))
                 ((string-prefix? "#" s) (lp paths))
                 ((string-prefix? "/" s) (lp (cons s paths)))
                 ((string-prefix? "include " s)
                  (let ((path (string-trim
                               (substring s (string-length "include ")))))
                    (unless (string-prefix? "/" path)
                      (error "expected an absolute path for includes"))
                    (lp (fold append-reverse
                              paths
                              (map load-conf
                                   (if (glob? path)
                                       (expand-glob path)
                                       (list path)))))))
                 (else
                  (error "invalid syntax" s))))))))))

(define system-library-search-path
  (let ((p (delay (load-conf "/etc/ld.so.conf"))))
    (lambda () (force p))))

(define (find-library-candidates stem search-path extension)
  (append-map
   (lambda (path)
     (filter-map (lambda (base)
                   (let ((f (string-append path "/" base)))
                     (and (not (file-is-directory? f))
                          f)))
                 (scandir path
                          (lambda (f)
                            (match (string-split f #\.)
                              (((? (cut equal? <> stem))
                                (? (cut equal? <> extension))
                                _ ...) #t)
                              (_ #f))))))
   search-path))

(define (has-elf-magic? file)
  (has-elf-header?
   (call-with-input-file file
     (lambda (f) (get-bytevector-n f 64)))))

(define* (find-library stem #:key
                       (search-path (system-library-search-path))
                       (extension "so")
                       version)
  (define (so-version f)
    (match (cddr (string-split (basename f) #\.))
      (() #t) ; Unversioned.
      ((v . _) (string->number v))))
  
  (let ((candidates
         (filter has-elf-magic?
                 (find-library-candidates stem search-path extension))))
    (if version
        ;; Find a library with a particular version.
        (find (lambda (elt) (equal? (so-version elt) version))
              candidates)
        ;; Otherwise, find the first unversioned library.  If none is
        ;; found, return the path with the highest version.
        (let lp ((in candidates) (candidate #f))
          (if (null? in)
              candidate
              (let ((v (so-version (car in))))
                (if (equal? v #t)
                    ;; We have our unversioned match.
                    (car in)
                    (lp (cdr in)
                        (if (or (not candidate)
                                (and v (> v (so-version candidate))))
                            (car in)
                            candidate)))))))))

(define (extract-exported-symbols elf)
  (let ((strs (assoc-ref (elf-sections-by-name elf) ".dynstr"))
        (syms (assoc-ref (elf-sections-by-name elf) ".dynsym")))
    (unless (and strs (= (elf-section-type strs) SHT_STRTAB))
      (error "ELF object has no dynamic string table"))
    (unless (and syms (= (elf-section-type syms) SHT_DYNSYM))
      (error "ELF object has no dynamic symbol table"))
    (let ((len (floor/ (elf-section-size syms) (elf-section-entsize syms))))
      (let lp ((n 0) (out '()))
        (if (< n len)
            (let ((sym (elf-symbol-table-ref elf syms n strs)))
              (lp (1+ n)
                  (if (and (equal? (elf-symbol-visibility sym) STV_DEFAULT)
                           (not (zero? (elf-symbol-value sym)))
                           ;; No debugging information on these.
                           (not (member (elf-symbol-name sym) '("_init" "_fini")))
                           (or (equal? (elf-symbol-type sym) STT_OBJECT)
                               (equal? (elf-symbol-type sym) STT_FUNC))
                           (or (equal? (elf-symbol-binding sym) STB_GLOBAL)
                               (equal? (elf-symbol-binding sym) STB_WEAK)))
                      (cons sym out)
                      out)))
            (reverse out))))))

(define (read-debuglink bv offset size byte-order)
  (define (align4 address)
    (+ address (modulo (- 4 (modulo address 4)) 4)))
  (let lp ((end offset))
    (if (zero? (bytevector-u8-ref bv end))
        (let* ((len (- end offset))
               (out (make-bytevector len)))
          (bytevector-copy! bv offset out 0 (- end offset))
          (let ((pos (align4 (1+ end))))
            (unless (<= (+ len 5) size)
              (error "bad debuglink" out))
            (values (utf8->string out)
                    (bytevector-u32-ref bv pos byte-order))))
        (lp (1+ end)))))

(define (search-debug-dirs basename dirname)
  (let ((path (string-append (global-debug-path) dirname "/" basename)))
    (and (file-exists? path)
         (has-elf-magic? path)
         path)))

(define (find-debug-object library)
  (let* ((bv (call-with-input-file library get-bytevector-all))
         (elf (parse-elf bv))
         (sections (elf-sections-by-name elf)))
    (cond
     ((assoc-ref sections ".debug_info")
      library)
     ((assoc-ref sections ".gnu_debuglink")
      => (lambda (section)
           (let-values (((basename crc)
                         (read-debuglink bv (elf-section-offset section)
                                         (elf-section-size section)
                                         (elf-byte-order elf))))
             (search-debug-dirs basename (dirname library)))))
     (else #f))))

(define* (die-ref die attr #:optional default)
  (cond
   ((list-index (die-attrs die) attr)
    => (cut list-ref (die-vals die) <>))
   (else default)))

(define (extract-declaration die resolve-die intern-type)
  (define (recur* die)
    (extract-declaration die resolve-die intern-type))
  (define (recur die)
    (case (die-tag die)
      ((typedef)
       (intern-type die))
      ((structure-type union-type enumeration-type class-type)
       (let ((name (die-ref die 'name)))
         (if (and name (not (die-ref die 'declaration)))
             (intern-type die)
             (recur* die))))
      (else
       (recur* die))))
  (define (visit-type offset)
    (recur (resolve-die offset)))
  (define (visit-attr attr val tail)
    (case attr
      ((decl-file decl-line sibling low-pc high-pc frame-base external
        location)
       tail)
      (else
       (cons (list attr (case attr
                          ((type) (visit-type val))
                          (else val)))
             tail))))
  (define (has-tag? tag)
    (lambda (x) (eq? (die-tag x) tag)))

  (let ((tag (die-tag die))
        (kids (die-children die)))
    (cons tag
          (fold-right
           visit-attr
           (case tag
             ((subprogram subroutine-type)
              (let ((formals (map recur
                                  (filter (has-tag? 'formal-parameter)
                                          kids)))
                    (varargs (find (has-tag? 'unspecified-parameters)
                                   kids)))
                (list
                 (cons 'args
                       (if varargs
                           (append formals (list recur varargs))
                           formals)))))
             ((structure-type union-type class-type)
              (if (die-ref die 'declaration)
                  '()
                  (list (cons 'members (map recur kids)))))
             ((enumeration-type)
              (list (cons 'literals (map recur kids))))
             (else
              (unless (null? kids)
                (error "unexpected children" die))
              '()))
           (die-attrs die) (die-vals die)))))

(define (type-name die)
  (list (case (die-tag die)
          ((structure-type) 'struct)
          ((union-type) 'union)
          ((class-type) 'class)
          ((typedef) 'typedef)
          ((enumeration-type) 'enum)
          (else (error "Don't know how to name" die)))
        (or (die-ref die 'name)
            (error "anonymous type should not get here"))))

(define (find-definitions symbols debuginfo)
  (let ((by-offset (make-hash-table))
        (externs (make-hash-table))
        (types-by-die (make-hash-table))
        (types-by-name vlist-null))
    (define (resolve-die offset)
      (or (hashv-ref by-offset offset)
          (error "unknown offset")))
    (define (intern-type die)
      (or (hashq-ref types-by-die die)
          (let* ((name (type-name die))
                 (decl (extract-declaration die resolve-die intern-type)))
            (cond
             ((vhash-assoc name types-by-name)
              => (lambda (pair)
                   (let ((name* (car pair))
                         (decl* (cdr pair)))
                     (unless (equal? decl decl*)
                       (error "two types with the same name but different decls"
                              decl decl*))
                     name*)))
             (else
              (set! types-by-name (vhash-cons name decl types-by-name))
              (hashq-set! types-by-die die name)
              name)))))
    (define (intern-die die)
      (hashv-set! by-offset (die-offset die) die)
      (when (die-ref die 'external)
        (let ((name (die-ref die 'name)))
          (when name
            (hash-set! externs name die))))
      (for-each intern-die (die-children die)))
    (for-each intern-die debuginfo)
    (let ((tail (map (lambda (s)
                       (let* ((name (elf-symbol-name s))
                              (die (hash-ref externs name)))
                         (unless die
                           (error "No debugging information for symbol" name))
                         (extract-declaration die resolve-die intern-type)))
                     symbols)))
      (append (vhash-fold (lambda (name decl tail)
                            (cons decl tail))
                          '()
                          types-by-name)
              tail))))
