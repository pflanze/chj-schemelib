;;; Copyright 2013-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require cj-source
	 (improper-list-1 improper*-map))

(export source-wrap-1
	source-wrap-1*
	source-wrap-n
	source-wrap-1+
	source:symbol-append ;; aliased to source.symbol-append by oo-util
	source.length
	source.car
	source.cdr 
	source.pair?
	source.symbol?
	source.string?
	source.number?
	;; move?
	symbol->keyword 
	source.symbol->keyword
	source.symbol->string
	;;/move?
	source.map
	source.improper*-map)


(define (source-wrap-1 fn)
  (lambda (x)
    (fn (source-code x))))

(define (source-wrap-1* fn)
  (lambda (x)
    (possibly-sourcify (fn (source-code x)) x)))

(define (source-wrap-n fn)
  (lambda xs
    (apply fn (map source-code xs))))

(define (source-wrap-1+ fn)
  (lambda (x . rest)
    (apply fn (source-code x) rest)))

(define (source-wrap-_1-n fn)
  (lambda (a . rest)
    (apply fn a (map source-code rest))))

(define source:symbol-append (source-wrap-n symbol-append))
;; XX keep source information? rarely used for symbols though.

(define source.length (source-wrap-1 length))
(define source.car (source-wrap-1 car))
(define source.cdr (source-wrap-1 cdr))
(define source.pair? (source-wrap-1 pair?))
(define source.symbol? (source-wrap-1 symbol?))
(define source.string? (source-wrap-1 string?))
(define source.number? (source-wrap-1 number?))

;; hm move to another lib?
(define (symbol->keyword v)
  (string->keyword (symbol->string v)))
(define source.symbol->keyword (source-wrap-1 symbol->keyword))

(define source.symbol->string (source-wrap-1 symbol->keyword))

;; from cj-source-util-2.scm
(define source.map source-map)
;; from predicates.scm
(define source.improper*-map (source-wrap-_1-n improper*-map))

