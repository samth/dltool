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
  #:use-module (system vm elf)
  #:use-module (system vm dwarf)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 ftw)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (system-library-search-path
            
            ))

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
            (filter-map (lambda (s)
                          (let ((b (basename s)))
                            (and (not (equal? b "."))
                                 (not (equal? b ".."))
                                 (regexp-exec pat b)
                                 b)))
                        (scandir dir)))
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
