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
  (version-etc "dlhacks"
               "0.1.0"
               #:copyright-year 2012
               #:copyright-holder "Andy Wingo <wingo@igalia.com>"
               #:command-name "dlhacks"
               #:license *LGPLv3+*))

(define* (display-usage #:optional (port (current-output-port)))
  (display "Usage: dlhacks [--help] [--version] COMMAND ARG...\n" port))

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
  (begin
    (define name
      (case-lambda*
       ((options . args)
        code code* ...)
       ((options . unrecognized)
        (format (current-error-port) "Unexpected arguments: ~a\n"
                unrecognized)
        (format (current-error-port) "Usage: dlhacks ~a\n"
                (car (string-split docstring #\newline)))
        (exit 1))))
    (set! *commands*
          (cons (make-command (symbol->string 'name)
                              '((option param ...) ...) docstring name)
                *commands*))))

(define (unrecognized-command name)
  (with-output-to-port (current-error-port)
    (lambda ()
      (format #t "Unknown command: ~a\n" name)
      (display-usage)
      (newline)
      (display "For more information and a list of available commands,\n")
      (display "try `dlhacks help'.\n")))
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
           (display "Usage: dlhacks ")
           (display (command-docstring c))))
     (else
      (unrecognized-command command))))
   (else
    (display-usage)
    (display "A toolkit for extracting information out of ELF shared libraries.

Available commands:

")
    (for-each (lambda (c)
                (format #t "    dlhacks ~a\n"
                        (car (string-split (command-docstring c) #\newline))))
              (sort *commands*
                    (lambda (x y)
                      (string<? (command-name x) (command-name y)))))
    (display "
Try `dlhacks help COMMAND' for help on particular commands.

Many commands take library names as arguments.  In that case, if the
path is a bare file name, without a slash, it is searched for in the
load path.  The load path is found by parsing the /etc/ld.so.conf file
and the LD_LIBRARY_PATH environment variable.

Some commands also try to locate debugging information for a library,
which may be in a separate file.  Many GNU/Linux distributions strip the
debugging information out their libraries, and instead offer that
information in separate \"-dbg\" packages.  In that case, the loadable
.so file has a link to the separate debug object.  This tool will load
separate debug objects, if needed.
")
    (emit-bug-reporting-address "dlhacks" "wingo@igalia.com"
                                #:url "https://gitorious.org/guile-dlhacks/"))))

(define (load-elf file)
  (parse-elf (call-with-input-file file get-bytevector-all)))

(define (all-exports lib-path)
  (sort (map elf-symbol-name (extract-exported-symbols (load-elf lib-path)))
        string<?))

(define (load-debug lib-path)
  (let ((dbg (find-debug-object lib-path)))
    (unless dbg
      (error "No debugging symbols for library" lib-path))
    (read-debuginfo
     (elf->dwarf-context (load-elf dbg) 0 0))))

(define-command ((grovel) options lib . syms)
  "grovel LIB [SYM...]
Grovel a library for debugging information.

If the user passes one or more SYM names, declarations of all those
symbols are printed on the console, preceded by declararations of the
types that they use.  Otherwise, declarations for all exported symbols
are printed.
"
  (for-each pretty-print
            (let* ((lib-path (if (string-index lib #\/)
                                 lib
                                 (find-library lib))))
              (unless lib-path
                (error "Failed to find library" lib))
              (extract-definitions (if (null? syms)
                                       (all-exports lib-path)
                                       syms)
                                   (load-debug lib-path)))))

(define-command ((dump) options lib)
  "dump LIB
Parse all debugging information out of a library.

This command prints all information that it can find out to the console.
It's a bit much, but it's useful for debugging.
"
  (for-each (lambda (x)
              (pretty-print x #:width 120))
            (let* ((lib-path (if (string-index lib #\/)
                                 lib
                                 (find-library lib))))
              (unless lib-path
                (error "Failed to find library" lib))
              (debuginfo->tree (load-debug lib-path)))))

(define (main args)
  (setlocale LC_ALL "")
  (let* ((options (getopt-long args *common-options*
                               #:stop-at-first-non-option #t))
         (args (option-ref options '() '()))
         (debug? (option-ref options 'debug #f)))
    (cond
     ((option-ref options 'help #f)
      (apply help options args))
     ((option-ref options 'version #f)
      (display-version))
     (else
      (match args
        ((command . args)
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
        (else
         (display-usage (current-error-port))
         (exit 1)))))
    (exit 0)))
