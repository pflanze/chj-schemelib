;;; Copyright 2014-2016 by Christian Jaeger, ch at christianjaeger ch

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; XX also see Status.scm and failure.scm; should this module
;; be ditched/merged? (But bootstrapping issues.)

(require cj-functional
	 define-macro-star
	 dot-oo
	 test
	 fallible-1)


;;lib
(define (1st/2 f)
  (lambda (a _b);;  . _rest ?
    (f a)))


;; propagate from Scheme #f to optional 'failures'

;; current-fallible is called in the context of the failure (hence could
;; continue or capture the continuation), with either the serialized
;; source code of the form (serialized-source) or the actual value
;; that failed as the first argument and the failure value as the
;; second. The idea is to collect all layers in the failure value
;; while leaving the predicate.

(define current-fallible (make-parameter false/2))

(define (with-fallible-handler handler thunk)
  (parameterize ((current-fallible handler))
		(thunk)))

(define (with-fallible-catcher handler thunk)
  (continuation-capture
   (lambda (return)
     (parameterize ((current-fallible
		     (lambda (x rest)
		       (continuation-graft
			return
			handler
			x
			rest))))
		   (thunk)))))


(define-struct. serialized-source
  #(u8vector? val))

(define. serialized-source.object
  (compose-function u8vector->object serialized-source.val))

(define-struct. value
  val)

(define. value.object value.val)


(define (fallible:and-expand If Or forms)
  (fold-right/last
   (lambda (form tail)
     (with-gensym
      V
      `(##let ((,V ,form))
	      (,If ,V
		   ,tail
		   ((current-fallible)
		    ',(serialized-source (object->u8vector form))
		    ,V)))))
   (lambda (form tail)
     (assert (null? tail))
     (with-gensym
      V
      `(##let ((,V ,form))
	      ,(Or V
		   `((current-fallible)
		     ',(serialized-source (object->u8vector form)) ,V)))))
   '()
   forms))

(TEST
 > (define TEST:equal? syntax-equal?)
 > (fallible:and-expand 'if
			(lambda (t f)
			  `(or ,t ,f))
			'("a" "b"))
 (##let ((GEN:V-2236 "a"))
	(if GEN:V-2236
	    (##let ((GEN:V-2258 "b"))
		   (or GEN:V-2258
		       ((current-fallible)
			'[(serialized-source) #u8(17 98)]
			GEN:V-2258)))
	    ((current-fallible)
	     '[(serialized-source) #u8(17 97)]
	     GEN:V-2236))))


(define. fallible.stack-update fallible-stack-update)
(define. fallible.stack fallible-stack)

(define. (fallible.deserialize v)
  (fallible.stack-update v
			(lambda (l)
			  (map (lambda (v)
				 (if (serialized-source? v)
				     (serialized-source.object v)
				     v))
			       l))))


(define. (value.show v)
  v)

(define. (serialized-source.show v)
  ;; HACK? Should really have proper source type?
  (vector 'source (cj-desourcify (serialized-source.object v))))

(define. fallible.show fallible-show)

(define. fallible.string fallible-string)


(define-macro* (fallible:if t a b)
  `(##if (##or (##not ,t) (fallible? ,t))
	 ,b
	 ,a))

(define-macro* (fallible:and . forms)
  (fallible:and-expand 'fallible:if
		   (lambda (t f)
		     `(fallible:if ,t ,t ,f))
		   forms))

(define active-fallible-handler
  (lambda (v rest)
    (if rest
	(fallible.stack-update
	 rest (lambda (vs)
		(cons v vs)))
	(fallible (cons v '())))))

(define (with-fails thunk)
  (with-fallible-handler active-fallible-handler
		     thunk))

(define (without-fails thunk)
  (with-fallible-handler false/2
		     thunk))

(define-typed (activate-fails! #(boolean? y))
  (current-fallible (if y active-fallible-handler
		    false/2)))

(define-macro* (%with-fails . body)
  `(with-fails (##lambda ()
			    ,@body)))

(define-macro* (%without-fails . body)
  `(without-fails (##lambda ()
			       ,@body)))

(TEST
 > (define (tests)
     (list (fallible:and 1 2)
	   (fallible:and 1 #f)
	   (fallible:and #f 2)
	   (fallible:and (even? 4) (even? 5))
	   (fallible:and (even? 7) (even? 8))
	   (fallible:and (even? 9) (even? 11))
	   (fallible:and (even? 10) (even? 12))
	   (fallible:and (even? -2)
			 (fallible:and (even? 0)
				       (odd? 0)))))
 > (without-fails tests)
 (2 #f #f #f #f #f #t #f)
 > (cj-desourcify
    (map (lambda (v)
	   (if (fallible? v)
	       (fallible.deserialize v)
	       v))
	 (with-fails tests)))
 (2
  [(fallible) (#f)]
  [(fallible) (#f)]
  [(fallible) ((even? 5))]
  [(fallible) ((even? 7))]
  [(fallible) ((even? 9))]
  #t
  [(fallible) ((fallible:and (even? 0)
			     (odd? 0))
	       (odd? 0))]))


;; ------------------------------------------------------------------
;; Alternative variants, don't use for consistency!

(IF #f
    (begin

      (define-macro* (fallible:_and . forms)
	(fallible:and-expand 'if
			 (lambda (t f)
			   `(or ,t ,f))
			 forms))

      (define-macro* (fallible:And . forms)
	(fallible:and-expand 'If
			 (lambda (t f)
			   `(If ,t #t ,f))
			 forms))



      (TEST
       > (fallible:_and 1 2)
       2
       > (with-fallible-catcher (1st/2 cj-desourcify) (& (fallible:_and (even? 4) (even? 5))))
       (even? 5)
       > (%try-error (fallible:And 1 2))
       #(error "If: expecting boolean, got:" 1)
       > (%try-error (fallible:And #t 2))
       #(error "If: expecting boolean, got:" 2)
       > (fallible:And #t (even? 6))
       #t
       > (fallible:And #t (even? 7))
       #f
       > (with-fallible-catcher (1st/2 cj-desourcify) (& (fallible:And #t (even? 7))))
       (even? 7)
       > (with-fallible-catcher (1st/2 cj-desourcify) (& (fallible:And (even? 5) (even? 7))))
       (even? 5)
       )


      ;; |iF|, accept #t as true, treat everything else as false?

      (define-macro* (iF t a b)
	`(##if (##eq? ,t #t)
	       ,a
	       ,b))

      (TEST
       > (iF #t 'y 'n)
       y
       > (iF 1 'y 'n)
       n
       > (iF #f 'y 'n)
       n)


      (define-macro* (fallible:anD . forms)
	(fallible:and-expand 'iF
			 (lambda (t f)
			   `(iF ,t #t ,f))
			 forms))

      (TEST
       > (fallible:anD 1 2)
       #f ;; because the default handler is false/1
       > (fallible:anD 1 #t)
       #f
       > (fallible:anD (even? 4) (even? 6))
       #t
       > (with-fallible-handler (1st/2 cj-desourcify) (& (fallible:anD (even? 4) (even? 7))))
       (even? 7)
       > (with-fallible-handler (1st/2 cj-desourcify) (& (fallible:anD (even? 5) (even? 7))))
       (even? 5))))


;; ------------------------------------------------------------------
;; supporting library

(define (fallible:every pred l)
  (let lp ((l l))
    (if (null? l)
	#t
	(let-pair ((a r) l)
		  (let ((v (pred a)))
		    (fallible:if v
			     (lp r)
			     ;; no source to show, but a value: (XX
			     ;; although, iff v is a failure already,
			     ;; it probably already has the value?)
			     ((current-fallible) (value a) v)))))))

;; XX (fallible:and (fallible:every )) etc.: should every fallible:-'function' be
;; a macro and maintain its own reporting? including fallible:and
;; reporting on the whole expression?

(TEST
 > (cj-desourcify
    (map .object
	 (fallible.stack
	  (%with-fails (fallible:and (fallible:every even? '(2 3 4)))))))
 ((fallible:every even? '(2 3 4)) 3))

