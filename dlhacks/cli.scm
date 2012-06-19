;; dlhacks CLI
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

(define-module (dlhacks cli)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 command-line)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 binary-ports)
  #:use-module (system repl error-handling)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (dlhacks)
  #:use-module (dlhacks elf)
  #:use-module (dlhacks dwarf)
  #:export (main))

(define *common-options*
  '((help (single-char #\h))
    (version (single-char #\v))
    (debug)))

(define (display-version)
  (version-etc "dltool"
               "0.1.0"
               #:copyright-year 2012
               #:copyright-holder "Andy Wingo <wingo@igalia.com>"
               #:command-name "dltool"
               #:license *LGPLv3+*))

(define* (display-usage #:optional (port (current-output-port)))
  (display "Usage: dltool [--help] [--version] COMMAND ARG...\n" port))

(define-record-type <command>
  (make-command name grammar docstring handler)
  command?
  (name command-name)
  (grammar command-grammar)
  (docstring command-docstring)
  (handler command-handler))

(define *commands* '())

(define (find-command name)
  (find (lambda (c) (equal? (command-name c) name)) *commands*))

(define-syntax-rule
  (define-command ((name (option param ...) ...) options . args)
    docstring
    code code* ...)
  (set! *commands*
        (cons (make-command
               (symbol->string 'name)
               '((option param ...) ...)
               docstring
               (case-lambda*
                ((options . args)
                 code code* ...)
                ((options . unrecognized)
                 (format (current-error-port) "Unexpected arguments: ~a\n"
                         unrecognized)
                 (format (current-error-port) "Usage: dltool ~a\n"
                         (car (string-split docstring #\newline)))
                 (exit 1))))
              *commands*)))

(define (unrecognized-command name)
  (with-output-to-port (current-error-port)
    (lambda ()
      (format #t "Unknown command: ~a\n" name)
      (display-usage)
      (newline)
      (display "For more information and a list of available commands,\n")
      (display "try `dltool help'.\n")))
  (exit 1))

(define-command ((help) options #:optional command)
  "help [COMMAND]

Display a general help message, or help for a particular command.
"
  (cond
   (command
    (cond
     ((find-command command)
      => (lambda (c)
           (display "Usage: dltool ")
           (display (command-docstring c))))
     (else
      (unrecognized-command command))))
   (else
    (display-usage)
    (display "A tool for extracting information out of ELF shared libraries.

Available commands:

")
    (for-each (lambda (c)
                (format #t "    dltool ~a\n"
                        (car (string-split (command-docstring c) #\newline))))
              (sort *commands*
                    (lambda (x y)
                      (string<? (command-name x) (command-name y)))))
    (display "
Try `dltool help COMMAND' for help on particular commands.

Many commands take library names as arguments.  In that case, if the
path is a bare file name without a slash, it is searched for in the load
path.  The load path is found by parsing the /etc/ld.so.conf file and
the LD_LIBRARY_PATH environment variable.

Some commands also try to locate debugging information for a library,
which may be in a separate file.  Many GNU/Linux distributions strip the
debugging information out their libraries, and instead offer that
information in separate \"-dbg\" packages.  In that case, the loadable
.so file has a link to the separate debug object.  This tool will load
separate debug objects, if needed.

Note that dltool does not yet support compressed debug
sections (e.g. \".zdebug_info\"), as used by some distributions.
")
    (emit-bug-reporting-address "dltool" "wingo@igalia.com"
                                #:url "https://gitorious.org/guile-dlhacks/"))))

(define (all-exports lib-elf)
  (sort (extract-exported-symbols lib-elf)
        (lambda (a b)
          (string<? (elf-symbol-name a) (elf-symbol-name b)))))

(define (load-die-roots lib)
  (read-die-roots (load-dwarf-context lib)))

(define-command ((print-decls) options lib . syms)
  "print-decls LIB [SYM...]
Print declarations for a library's publically exported symbols.

If the user passes one or more SYM names, declarations of all those
symbols are printed on the console, preceded by declararations of the
types that they use.  Otherwise, declarations for all exported
symbols (and the types they use) are printed.

This command uses the .dlsym section of the ELF file to find the
definitions, so it only works for exported functions and variables.  To
grovel for internal variables, use the \"print-one --grovel\" command.
"
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
             (format (current-error-port)
                     "warning: no debug information for symbol: ~a\n"
                     name)))))))))

(define-command ((list-exports) options lib)
  "list-exports LIB
Print a list of exported symbols.
"
  (for-each
   (lambda (sym)
     (format #t "~a: ~a at ~a\n"
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
                 get-bytevector-all)))))

(define-command ((dump) options lib)
  "dump LIB
Parse all debugging information out of a library.

This command prints all information that it can find out to the console.
It's a bit much, but it's useful for debugging.
"
  (for-each (lambda (x)
              (pretty-print (die->tree x) #:width 120))
            (load-die-roots lib)))

(define-command ((print-one (depth (value #t)) (grovel)) options lib name
                 #:optional tag)
  "print-one [--depth=N] [--grovel] LIB NAME [KIND]
Print the definition of a symbol.

By default, without the --grovel option, NAME is searched for in the
public exports (.dynsym entries) of LIB.  A representation of the
definition for the symbol is printed on the console, as a single
s-expression.  The depth of the type declarations in the definition can
be controlled with the --depth option.

With the --grovel option, instead of looking for NAME in the public
exports, the print-one command will grovel about in the debugging
entries for LIB, looking for one whose name matches.  Usually you don't
want this, but it is useful when looking for information not present in
.dynsym, like type definitions.

Note that there are two different identifier namespaces in C and C++:
the namespace of \"tagged types\" (structs, classes, unions and enums),
and another namespace for the rest of things, including functions and
variables, but also typedefs.

This command will simply return the first definition that it sees with
the given name.  If you want to specify that it be of a particular type,
then give the type as an additional argument.  Available types are
derived from the DWARF specification, and include enumeration-type,
structure-type, typedef, subprogram, union-type, etc.  See the DWARF
standard for full details (stripping the initial DW_TAG_ prefix, and
converting underscores to dashes).

Finally, we should note that each separate compilation unit effectively
instantiates a new type tree.  Some of those types will be shared with
other compilation units, but it is always possible to define a type
local to a compilation unit (e.g. inside the C file).  When using
--grovel, you should check the result to ensure its sanity.
"
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

(define (dispatch-command command args debug?)
  (let ((c (find-command command)))
    (unless c
      (unrecognized-command command))
    (let* ((options (getopt-long (cons command args)
                                 (command-grammar c)
                                 #:stop-at-first-non-option #t))
           (args (option-ref options '() '())))
      (call-with-error-handling
       (lambda ()
         (apply (command-handler c) options args))
       #:on-error (if debug? 'debug 'report)
       #:post-error (lambda args (exit 1))))))

(define (main args)
  (setlocale LC_ALL "")
  (let* ((options (getopt-long args *common-options*
                               #:stop-at-first-non-option #t))
         (args (option-ref options '() '()))
         (debug? (option-ref options 'debug #f)))
    (cond
     ((option-ref options 'help #f)
      (dispatch-command "help" '() debug?))
     ((option-ref options 'version #f)
      (display-version))
     (else
      (match args
        ((command . args)
         (dispatch-command command args debug?))
        (else
         (display-usage (current-error-port))
         (exit 1)))))
    (exit 0)))
