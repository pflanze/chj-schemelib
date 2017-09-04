;;; Copyright 2010-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.

(require define-macro-star)

(export (macro use-debuggable-promise)
	(##namespace ("debuggable#" delay force promise?))
	;; and debuggable-promise in "" namespace

	promise-evaluated?
	evaluated-promise-value
	)

(include "cj-standarddeclares.scm")


;; Debugging infrastructure for lazy code:

(define-macro* (use-debuggable-promise)
  `(##namespace ("debuggable#" delay force promise?)))


(define debuggable-promise-tag (list 'debuggable-promise))

(define (debuggable#make-promise thunk-or-value evaluated? capturectx)
  (vector debuggable-promise-tag
	  thunk-or-value evaluated? capturectx))

(define (debuggable-promise? v)
  (and (vector? v)
       (fx= (##vector-length v) 4)
       (eq? (##vector-ref v 0) debuggable-promise-tag)))

(define (@debuggable-promise-thunk-or-value v) (##vector-ref v 1))
(define (@debuggable-promise-thunk-or-value-set! v val) (##vector-set! v 1 val))

(define (@debuggable-promise-evaluated? v) (##vector-ref v 2))
(define (@debuggable-promise-evaluated?-set! v val) (##vector-set! v 2 val))

(define (@debuggable-promise-capturectx v) (##vector-ref v 3))


(define-macro* (debuggable#delay body0 . body)
  (define capturectx (gensym 'capturectx))
  `((let ()
      (##namespace (""))
      continuation-capture)
    (lambda (,capturectx)
      (debuggable#make-promise
       (##lambda () ,body0 ,@body)
       #f
       ,capturectx))))


(define (debuggable#force v)
  (if (debuggable-promise? v)
      (let ((tv (@debuggable-promise-thunk-or-value v)))
	(if (@debuggable-promise-evaluated? v)
	    tv
	    ;; forgot, is it OK to shift values like this (short-cut
	    ;; future evaluation of multi-layer promises)? just try..
	    (let ((val (debuggable#force (tv))))
	      (@debuggable-promise-thunk-or-value-set! v val)
	      (@debuggable-promise-evaluated?-set! v #t)
	      val)))
      (##force v)))

(define (debuggable#promise? v)
  (or (##promise? v)
      (debuggable-promise? v)))


(define (promise-evaluated? v)
  (cond ((##promise? v)
	 (@promise-evaluated? v))
	((debuggable-promise? v)
	 (@debuggable-promise-evaluated? v))
	(else
	 (error "not a promise:" v))))

(define (evaluated-promise-value v)
  (define (err)
    (error "promise not evaluated:" v))
  (cond ((##promise? v)
	 (if (@promise-evaluated? v)
	     (@promise-value v)
	     (err)))
	((debuggable-promise? v)
	 (if (@debuggable-promise-evaluated? v)
	     (@debuggable-promise-thunk-or-value v)
	     (err)))
	(else
	 (error "not a promise:" v))))
