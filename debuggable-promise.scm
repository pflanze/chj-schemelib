;;; Copyright 2010-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.

(require define-macro-star
	 (cj-struct-tag struct-tag-allocate! struct-metadata)
	 lazy-1)

(export (macro possibly-use-debuggable-promise) ;; see docs below
	(macro delay)
	;; (##namespace ("debuggable#" delay force promise?))
	;; and debuggable-promise in "" namespace

	promise? ;; well, specially via possibly-use-debuggable-promise, too
	evaluated-promise?
	promise-evaluated?
	evaluated-promise-value
	)

;; (include "cj-standarddeclares.scm")
;; nope, since that contains namespacing for "cj-struct#vector?" and we don't have that yet.
(declare (block)
	 (standard-bindings)
	 (extended-bindings))


;; Debugging infrastructure for lazy code:

;; use whenever you use cj-standarddeclares.scm in a module that
;; should see the global change that happens via loading of
;; debuggable-promise-everywhere:
(define-macro* (possibly-use-debuggable-promise)
  `(begin
     (set! make-promise make-promise)
     ;; instead of via updateable linking, can just 'set' directly,
     ;; since they work for both kinds of promises:
     (##namespace ("debuggable#" force force1 promise?))))




(define-if-not-defined
   debuggable-promise-tag
   (struct-tag-allocate! 'debuggable-promise
			 (struct-metadata 'make-debuggable-promise)))

(define (make-debuggable-promise thunk-or-value evaluated? capturectx)
  (vector debuggable-promise-tag
	  thunk-or-value evaluated? capturectx))

(define (debuggable-promise? v)
  (and (##vector? v)
       (fx= (##vector-length v) 4)
       (eq? (##vector-ref v 0) debuggable-promise-tag)))

(define (@debuggable-promise-thunk-or-value v) (##vector-ref v 1))
(define (@debuggable-promise-thunk-or-value-set! v val) (##vector-set! v 1 val))

(define (@debuggable-promise-evaluated? v) (##vector-ref v 2))
(define (@debuggable-promise-evaluated?-set! v val) (##vector-set! v 2 val))

(define (@debuggable-promise-capturectx v) (##vector-ref v 3))



(define (debuggable#make-promise thunk)
  (continuation-capture
   (lambda (cont)
     (make-debuggable-promise
      thunk
      #f
      cont))))

(define-macro* (delay body0 . body)
  `(make-promise
    (##lambda () ,body0 ,@body)))

(define-macro* (debuggable-delay body0 . body)
  `(debuggable#make-promise
    (##lambda () ,body0 ,@body)))

(define make-promise ##make-promise)
;; ^ changed by debuggable-promise-everywhere


(define (@debuggable-force1 v)
  (let ((tv (@debuggable-promise-thunk-or-value v)))
    (if (@debuggable-promise-evaluated? v)
	tv
	(let ((val (tv)))
	  (@debuggable-promise-thunk-or-value-set! v val)
	  (@debuggable-promise-evaluated?-set! v #t)
	  val))))

(define (debuggable#force1 v)
  (if (debuggable-promise? v)
      (@debuggable-force1 v)
      (##force v)))

(define (debuggable#force v)
  (if (debuggable-promise? v)
      (debuggable#force (@debuggable-force1 v))
      (if (##promise? v)
	  (debuggable#force (##force v))
	  v)))

(define force debuggable#force)

(define (debuggable#promise? v)
  (or (##promise? v)
      (debuggable-promise? v)))

(TEST
 > (define c 0)
 > (define v (debuggable-delay (debuggable-delay (inc! c) 2)))
 > (define v1 (debuggable#force1 v))
 > (debuggable#promise? v1)
 #t
 > c
 0
 > (define v2 (debuggable#force1 v))
 > c
 0
 > (debuggable#force1 v1)
 2
 > c
 1)

;; check that force does not (only?) store directly:
(TEST
 > (define c 0)
 > (define v (debuggable-delay (debuggable-delay (inc! c) 3)))
 > (debuggable#force v)
 3
 > c
 1
 > (debuggable#promise? (debuggable#force1 v))
 #t ;; should it really be that way?
 )




(define promise? debuggable#promise?)


(define (promise-evaluated? v)
  (cond ((##promise? v)
	 (@promise-evaluated? v))
	((debuggable-promise? v)
	 (@debuggable-promise-evaluated? v))
	(else
	 (error "not a promise:" v))))

(define (evaluated-promise? v)
  (cond ((##promise? v)
	 (@promise-evaluated? v))
	((debuggable-promise? v)
	 (@debuggable-promise-evaluated? v))
	(else
	 #f)))

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
