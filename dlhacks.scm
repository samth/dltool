;; guile-dlhacks
;; Copyright (C) 2012 Andy Wingo <wingo@igalia.com>

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
  #:autoload (ice-9 pretty-print) (pretty-print)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (global-debug-path
            ld-so-conf
            system-library-search-path
            find-library
            find-debug-object
            load-dwarf-context
            extract-exported-symbols
            extract-definitions
            extract-one-definition))

(define global-debug-path
  (make-parameter "/usr/lib/debug"))

(define ld-so-conf
  (make-parameter "/etc/ld.so.conf"))

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
  (lambda ()
    (append
     (cond ((getenv "LD_LIBRARY_PATH")
            => (lambda (path)
                 (filter (lambda (x)
                           (false-if-exception (file-is-directory? x)))
                         (string-split path #\:))))
           (else '()))
     (load-conf (ld-so-conf)))))

(define (library-matcher stem extension)
  (let* ((head (string-append stem "." extension))
         (len (string-length head)))
    ;; Returns #f if the library does not have the stem and extension.
    ;; Otherwise returns the version as a number, or #t if there is no
    ;; version.
    (lambda (f)
      (if (string= head f)
          #t
          (and (string-prefix? head f)
               (let ((tail (substring f len)))
                 (and (string-prefix? "." tail)
                      (string->number (cadr (string-split tail #\.))))))))))

(define (find-library-candidates stem search-path extension)
  (let* ((matcher (library-matcher stem extension)))
    (append-map
     (lambda (path)
       (filter-map
        (lambda (base)
          (let ((f (string-append path "/" base)))
            (and (not (file-is-directory? f))
                 f)))
        (scandir path matcher)))
     search-path)))

(define (has-elf-magic? file)
  (and (file-exists? file)
       (has-elf-header?
        (call-with-input-file file
          (lambda (f) (get-bytevector-n f 64))))))

(define* (find-library stem #:key
                       (search-path (system-library-search-path))
                       (extension "so")
                       version)
  (define so-version (library-matcher stem extension))
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
    (and (has-elf-magic? path)
         path)))

(define (search-debug-by-build-id build-id)
  (define (byte->hex-string byte)
    (let ((s (number->string byte 16)))
      (if (= (string-length s) 1)
          (string-append "0" s)
          s)))
  (let* ((bytes (bytevector->u8-list build-id))
         (path (string-concatenate
                `(,(global-debug-path)
                  "/.build-id/"
                  ,(byte->hex-string (car bytes))
                  "/"
                  ,@(map byte->hex-string (cdr bytes))
                  ".debug"))))
    (and (has-elf-magic? path)
         path)))

(define (find-debug-object library elf)
  (let ((sections (elf-sections-by-name elf)))
    (or (and (assoc-ref sections ".debug_info")
             library)
        (and=> (assoc-ref sections ".note.gnu.build-id")
               (lambda (section)
                 (let ((note (parse-elf-note elf section)))
                   (and (equal? (elf-note-name note) "GNU")
                        (= (elf-note-type note) NT_GNU_BUILD_ID)
                        (search-debug-by-build-id (elf-note-desc note))))))
        (and=> (assoc-ref sections ".gnu_debuglink")
               (lambda (section)
                 (let-values (((basename crc)
                               (read-debuglink (elf-bytes elf)
                                               (elf-section-offset section)
                                               (elf-section-size section)
                                               (elf-byte-order elf))))
                   (search-debug-dirs basename (dirname library)))))
        (error "No debugging symbols for library" library))))

(define (load-elf file)
  (parse-elf (call-with-input-file file get-bytevector-all)))

(define (load-dwarf-context lib)
  (let* ((lib-path (if (string-index lib #\/)
                       lib
                       (or (find-library lib)
                           (error "Failed to find library" lib))))
         (lib-elf (load-elf lib-path))
         (dbg-path (find-debug-object lib-path lib-elf)))
    (values (elf->dwarf-context (if (equal? lib-path dbg-path)
                                    lib-elf
                                    (load-elf dbg-path))
                                #:path dbg-path
                                #:lib-path lib-path)
            lib-elf)))

(define (extract-declaration die intern-type)
  (define (recur* die)
    (extract-declaration die intern-type))
  (define (recur die)
    (case (die-tag die)
      ((typedef)
       ;; Typedefs without types are declarations.
       (if (die-ref die 'type)
           (intern-type die)
           (type-name die)))
      ((structure-type union-type enumeration-type class-type)
       (cond
        ((die-ref die 'name)
         (intern-type die))
        ((die-ref die 'specification)
         => (lambda (offset)
              (let ((spec (find-die-by-offset die offset)))
                (if (die-ref spec 'name)
                    (intern-type die spec)
                    (recur* die)))))
        (else
         (recur* die))))
      (else
       (recur* die))))
  (define (visit-type offset)
    (recur (find-die-by-offset die offset)))
  (define (visit-attr attr val tail)
    (case attr
      ((decl-file decl-line sibling low-pc high-pc frame-base external
        location)
       tail)
      ((type containing-type specification object-pointer import)
       (cons (list attr (visit-type val)) tail))
      (else
       (cons (list attr val) tail))))
  (define (has-tag? tag)
    (lambda (x) (eq? (die-tag x) tag)))

  (let ((tag (die-tag die))
        (kids (die-children die)))
    (cons tag
          (fold
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
                           (append formals (list (recur varargs)))
                           formals)))))
             ((structure-type union-type class-type)
              (list (cons 'members
                          (map recur
                               (filter (has-tag? 'member) kids)))))
             ((enumeration-type)
              (list (cons 'literals (map recur kids))))
             ((array-type)
              (map recur kids))
             (else
              (unless (null? kids)
                (error "unexpected children" die))
              '()))
           (reverse (die-attrs die))
           (reverse (die-vals die))))))

(define (ctx-namespace-name ctx)
  (cond
   ((ctx-die ctx)
    => (lambda (die)
         (case (die-tag die)
           ((compile-unit) '())
           (else
            (cons (type-name die)
                  (ctx-namespace-name (die-ctx die)))))))
   (else '())))

(define* (type-name die #:optional spec)
  (cond
   ((die-ref die 'name)
    => (lambda (name)
         (list 'named-type-reference
               (cons* (die-tag die)
                      name
                      (ctx-namespace-name (die-ctx die))))))
   ((and spec (die-ref spec 'name))
    (type-name spec))
   ((die-ref die 'specification)
    => (lambda (offset)
         (type-name (find-die-by-offset die offset))))
   (else
    (error "anonymous type should not get here" die))))

;; A new declaration is compatible with an previous one if it has the
;; same size as the previous one, and only differs in declarations of
;; members that occupy no space, neither in the vtable nor in the
;; instance.
(define (compatible-declarations? die decl previous-decl)
  (define (elidable? elt)
    (match elt
      (('subprogram . attrs)
       (not (assoc 'virtuality attrs)))
      (_ #f)))
  ;; A "subset" has fewer decls than a "superset".
  (define (compatible-subset? sub super)
    (match sub
      ((sub-head . sub-tail)
       (match super
         ((super-head . super-tail)
          (if (or (equal? sub-head super-head)
                  (compatible-subset? sub-head super-head))
              (compatible-subset? sub-tail super-tail)
              (and (elidable? super-head)
                   (compatible-subset? sub super-tail))))
         (_ #f)))
      (()
       (match super
         ((super-head . super-tail)
          (and (elidable? super-head)
               (compatible-subset? sub super-tail)))
         (() #t)))
      (_ (or (equal? sub super)
             ;; Classes and structures are compatible.  In fact in some
             ;; cases, you will see DWARF declarations for "struct foo;"
             ;; with `structure-type' followed by a `class-type'
             ;; corresponding to a "struct foo : public bar" definition.
             (and (memq sub '(class-type structure-type))
                  (memq super '(class-type structure-type)))))))
  (or (equal? decl previous-decl)
      (compatible-subset? previous-decl decl)
      (compatible-subset? decl previous-decl)))

(define (extract-definitions ctx names)
  (let ((externs (make-hash-table))
        (types-by-offset (make-hash-table))
        (types-by-name vlist-null)
        (roots (read-die-roots ctx)))
    (define (prepare-extern name)
      (hash-set! externs name #f))
    (define* (intern-type die #:optional spec)
      (or (hashv-ref types-by-offset (die-offset die))
          (let* ((name (type-name die spec)))
            (hashv-set! types-by-offset (die-offset die) name)
            (unless (die-ref die 'declaration)
              (let ((decl (extract-declaration die intern-type)))
                (match (vhash-assoc name types-by-name)
                  ((name* . decl*)
                   (unless (compatible-declarations? die decl decl*)
                     (pretty-print decl (current-error-port))
                     (pretty-print decl* (current-error-port))
                     (error "two types with the same name but incompatible decls" name)))
                  (#f
                   (set! types-by-name (vhash-cons name decl types-by-name))))))
            name)))
    (define (find-externs die)
      (define (skip? ctx offset abbrev)
        (case (abbrev-tag abbrev)
          ((subprogram variable) #f)
          ;; For C++, descend into classes and structures so that we
          ;; populate the context tree.
          ((class-type structure-type)
           (not (eq? (ctx-language ctx) 'C-plus-plus)))
          (else #t)))
      (case (die-tag die)
        ((subprogram variable)
         (when (and (die-ref die 'external)
                    (not (die-ref die 'declaration))
                    (not (die-ref die 'inline)))
           (let ((name (die-ref die 'name)))
             (when name
               (let ((handle (hash-get-handle externs name)))
                 (cond
                  ((not handle))
                  ((cdr handle)
                   (error "Duplicate definition" name (cdr handle) die))
                  (else
                   (set-cdr! handle die))))))))
        ((compile-unit class-type structure-type)
         ;; We only see class-type and structure-type if we are
         ;; processing C++.
         (fold-die-children die
                            (lambda (die seed) (find-externs die))
                            #f
                            #:skip? skip?))))
    (for-each prepare-extern names)
    (for-each find-externs roots)
    (let lp ((names names) (out '()))
      (match names
        ((name . names)
         (cond
          ((hash-ref externs name)
           => (lambda (die)
                (lp names
                    (cons (extract-declaration die intern-type)
                          out))))
          (else
           (format (current-error-port)
                   "warning: no debug information for symbol: ~a\n"
                   name)
           (lp names out))))
        (()
         (vhash-fold (lambda (name decl tail)
                       (cons decl tail))
                     (reverse out)
                     types-by-name))))))

(define-syntax-rule (let/ec k e e* ...)
  (let ((tag (make-prompt-tag)))
    (call-with-prompt
     tag
     (lambda ()
       (let ((k (lambda args (apply abort-to-prompt tag args))))
         e e* ...))
     (lambda (_ res) res))))

(define* (find-die roots pred #:key
                   (skip? (lambda (ctx offset abbrev) #f))
                   (recurse? (lambda (die) #t)))
  (let/ec k
    (define (visit-die die)
      (cond
       ((pred die)
        (k die))
       ((recurse? die)
        (fold-die-children die (lambda (die seed) (visit-die die)) #f
                           #:skip? skip?))
       (else #f)))
    (for-each visit-die roots)
    #f))

(define* (extract-one-definition ctx pred #:optional (depth 1))
  (let ((roots (read-die-roots ctx)))
    (define* (visit-die x seen)
      (define (recur y)
        (visit-die y (cons x seen)))
      (define (visit-type offset)
        (recur (or (find-die-by-offset x offset)
                   (error "what!"))))
      (define (visit-attr attr val tail)
        (case attr
          ((decl-file decl-line sibling low-pc high-pc frame-base
                      external location)
           tail)
          ((type containing-type specification object-pointer import)
           (cons (list attr (visit-type val)) tail))
          (else
           (cons (list attr val) tail))))
      (if (and (or (< depth (length seen))
                   (find (lambda (y) (equal? (die-offset y) (die-offset x)))
                         seen))
               (memq (die-tag x)
                     '(structure-type union-type class-type typedef
                                      enumeration-type))
               (die-ref x 'name))
          (type-name x)
          (cons (die-tag x)
                (fold visit-attr
                      (map recur (die-children x))
                      (reverse (die-attrs x))
                      (reverse (die-vals x))))))
    (and=> (find-die roots pred
                     #:recurse? (lambda (die)
                                  (case (die-tag die)
                                    ((compile-unit) #t)
                                    ((structure-type class-type)
                                     (eq? (ctx-language (die-ctx die))
                                          'C-plus-plus))
                                    (else #f))))
           (cut visit-die <> '()))))
