;;; Copyright 2013-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (fixnum inc)
	 define-macro-star)

(export string-empty?  (macro %string-empty?)
	string-every
	string-first-line)

(include "cj-standarddeclares.scm")


;; XX obsolete?, see string.null? from oo-vector-lib.scm
(define (string-empty? str)
  (zero? (string-length str)))

;; too early for cj-inline
(define-macro* (%string-empty? str)
  `(##fxzero? (string-length ,str)))


(define (string-every fn str)
  (let ((len (string-length str)))
    (let lp ((i 0))
      (if (< i len)
	  (if (fn (string-ref str i))
	      (lp (inc i))
	      #f)
	  #t))))

;; TEST see string-util-2

(define (string-first-line str)
  (let ((len (string-length str)))
    (let lp ((i 0))
      (if (< i len)
	  (if (char=? (string-ref str i) #\newline)
	      (substring str 0 i)
	      (lp (inc i)))
	  str))))

;; TEST see string-util-2
