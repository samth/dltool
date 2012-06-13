;; Guile-Dwarfutils
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
  #:use-module ((ice-9 i18n) #:select (string-locale<?))
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (system-library-search-path
            find-library
            ))

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
