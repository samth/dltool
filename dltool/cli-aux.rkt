#lang racket

(require "main.rkt" "elf.rkt" "dwarf.rkt"
	 srfi/13 (only-in srfi/1 find))
(define option-ref dict-ref)

(provide (all-defined-out))

(define (-list-exports options lib)
  (for-each
   (lambda (sym)
     (printf "~a: ~a at ~a\n"
             (elf-symbol-name sym)
             (cond
              ((= (elf-symbol-type sym) STT_OBJECT)
               "object")
              ((= (elf-symbol-type sym) STT_FUNC)
               "function")
              (else "<unknown>"))
             (elf-symbol-value sym)))
   (all-exports
    (parse-elf (call-with-input-file
                   (if (string-index lib #\/)
                       lib
                       (or (find-library lib)
                           (error "Failed to find library" lib)))
                 port->bytes)))))

(define (-print-one options lib name [tag #f])
    (call-with-values (lambda () (load-dwarf-context lib))
    (lambda (ctx lib-elf)
      (let ((tag (and=> tag string->symbol))
            (d (option-ref options 'depth "0")))
        (unless (string->number d)
          (error "Bad depth (expected a number)" d))
        (let* ((die (if (option-ref options 'grovel #f)
                        (find-one-definition
                         ctx
                         (lambda (die)
                           (and (or (not tag)
                                    (eq? (die-tag die) tag))
                                (die-name die)
                                (string-suffix? (die-name die) name)
                                (equal? (die-qname die) name)
                                (not (empty-declaration? die)))))
                        (let ((symbol
                               (find (lambda (symbol)
                                       (equal? (elf-symbol-name symbol) name))
                                     (all-exports lib-elf))))
                          (unless symbol
                            (error "Failed to find symbol in exports list:"
                                   name))
                          (cdar (resolve-symbols
                                 ctx
                                 (list symbol)
                                 (lambda (name)
                                   (error "No debugging info found for symbol:"
                                          name))))))))
          (unless die
            (format (current-error-port)
                    "ERROR: Failed to find ~a in library ~a: ~a\n"
                    (or tag "definition") lib name)
            (exit 1))
          (pretty-print (extract-one-definition die (string->number d))))))))

(define (-dump options lib)
  (for-each (lambda (x)	
              (pretty-print (die->tree x) #:width 120))
            (load-die-roots lib)))

(define (all-exports lib-elf)
  (sort (extract-exported-symbols lib-elf)
        (lambda (a b)
          (string<? (elf-symbol-name a) (elf-symbol-name b)))))

(define (load-die-roots lib)
  (read-die-roots (load-dwarf-context lib)))

(define (-print-decls options lib [syms '()])
  (for-each
   pretty-print
   (call-with-values (lambda () (load-dwarf-context lib))
     (lambda (ctx lib-elf)
       (let ((symbols (all-exports lib-elf)))
         (for-each (lambda (name)
                     (unless (find (lambda (symbol)
                                     (equal? (elf-symbol-name symbol) name))
                                   symbols)
                       (error "Failed to find symbol in exports list:" name)))
                   syms)
         (extract-definitions
          (resolve-symbols
           ctx
           (if (null? syms)
               symbols
               (filter (lambda (symbol)
                         (member (elf-symbol-name symbol) syms))
                       symbols))
           (lambda (name)
             (fprintf (current-error-port)
		      "warning: no debug information for symbol: ~a\n"
		      name)))))))))
