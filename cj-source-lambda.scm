;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; XXX shouldn't this be merged with dsssl.scm ?

(require test
	 (cj-source-util-2 assert)
	 (list-util let-pair)
	 (cj-source raise-source-error)
	 (srfi-1 append-reverse)
	 (cj-typed perhaps-typed.var))

(export source.bindings->app
	#!optional
	source.list-of-length-2?)

(include "cj-standarddeclares.scm")


;; XX move somewhere?
(define (source.list-of-length-2? v)
  (let ((v (source-code v)))
    (and (pair? v)
	 (let ((v (source-code (cdr v))))
	   (and (pair? v)
		(let ((v (source-code (cdr v))))
		  (null? v)))))))

(TEST
 > (source.list-of-length-2? 'v)
 #f
 > (source.list-of-length-2? '(v))
 #f
 > (source.list-of-length-2? '(v w))
 #t
 > (source.list-of-length-2? '(v w x))
 #f
 > (source.list-of-length-2? '(v w . x))
 #f
 > (source.list-of-length-2? '(v . x))
 #f)


;; turn a bindings form from a lambda definition and the values to be bound
;; into function application syntax

;; -> source
(define (source.bindings->app bind
			      #!optional
			      ;; two modi: either values, or calling
			      ;; expr
			      maybe-vs
			      maybe-exprs)
  ;; "state machine"
  ;; expect: 'positional | 'optional | 'key | 'rest | 'end
  (let lp ((s (source-code bind))
	   (maybe-vs maybe-vs)
	   (expect 'positional)
	   (res '()))

    (let ((handle
	   (lambda (s* b *b expect)
	     (cond ((or (symbol? *b)
			;; XX list-of-length-2 would be wrong
			;; in the case of rest, right? complain
			;; in this case.
			(source.list-of-length-2? *b))
		    (if (and maybe-vs (null? maybe-vs))
			(error "did not get enough value slots")

			(case expect
			  ((positional optional)
			   (lp s*
			       (and maybe-vs (cdr maybe-vs))
			       expect
			       (cons (if maybe-vs
					 (car maybe-vs)
					 b)
				     res)))

			  ((rest)
			   (lp s*
			       (and maybe-vs (cdr maybe-vs))
			       'end
			       (if maybe-vs
				   (append-reverse (car maybe-vs)
						   res)
				   (cons b res))))

			  ((key)
			   (lp s*
			       (and maybe-vs (cdr maybe-vs))
			       'key
			       (cons (if maybe-vs
					 (car maybe-vs)
					 b)
				     (cons (string->keyword (symbol->string *b))
					   res))))
			  (else
			   (error "in invalid mode:" expect)))))
		   (else
		    (raise-source-error
                     b "expecting symbol or list of length 2"))))))
    
      (cond ((null? s)
	     (if (or (not maybe-vs)
		     (null? maybe-vs))
		 (let ((e (reverse res)))
		   (if maybe-exprs
		       (let ((e (append maybe-exprs e)))
			 (if (eq? expect 'end)
			     ;; XX Is that really only after rest?
			     (cons 'apply e)
			     e))
		       e))
		 (error "got too many value slots, still have:"
			maybe-vs)))
	    ((pair? s)
	     (let-pair
	      ((b s*) s)
	      (let* ((b1 (perhaps-typed.var b))
		     (*b (source-code b1))
		     (lp-expect (lambda (expect)
				  (lp s* maybe-vs expect res))))
		(case *b
		  ((#!optional)
		   ;; XX replace all assert with proper source-error
		   ;; messaging? Or use a source-assert? What does the
		   ;; fancy one already do?
		   (assert (not (eq? expect 'optional)))
		   (lp-expect 'optional))
		  ((#!key)
		   (assert (not (eq? expect 'key)))
		   (lp-expect 'key))
		  ((#!rest)
		   (assert (not (eq? expect 'rest)))
		   (lp-expect 'rest))
		  ;; XX also handle #!rest #!key and such nonsense(?)
		  (else
		   (case expect
		     ((end)
		      (raise-source-error b "superfluous item"))

		     (else
		      (handle s* b1 *b expect))))))))
	    (else
	     ;; combination of #!rest-triggered rest and end modes
	     (assert (not (eq? expect 'rest)))
	     (let ((b1 (perhaps-typed.var s)))
	       (handle '() b1 (source-code b1) 'rest)))))))


(TEST
 > (define (t b #!optional vs f)
     (%try-error (source.bindings->app b vs f)))
 > (t '(a b c) '(10 11 12))
 (10 11 12)
 > (t '(a b c) #f '(f))
 (f a b c)
 > (t '(a b c) '(10 11))
 #(error "did not get enough value slots")
 > (t '(a b c) '(10 11 12 13))
 #(error "got too many value slots, still have:" (13))
 > (t '(a b #!optional c) '(10 11))
 #(error "did not get enough value slots")
 > (t '(a b #!optional c) #f '(F))
 (F a b c)
 > (t '(a b #!optional c) '(10 11 12))
 (10 11 12)
 > (t '(a b #!key c d) '(10 11 12 13))
 (10 11 c: 12 d: 13)
 > (t '(a b #!key c d) #f '(f))
 (f a b c: c d: d)
 > (t '(a b #!key c d) '(10 11 12))
 #(error "did not get enough value slots")
 > (t '(a b #!rest c) '(10 11 (12 13)))
 (10 11 12 13)
 > (t '(a b #!rest c) #f '(f))
 (apply f a b c)
 > (t '(a b . c) '(10 11 (12 13)))
 (10 11 12 13)
 > (t '(a b . c) #f '(f x))
 (apply f x a b c)
 > (t '(a b #!rest #!rest) '(10 11 (12 13)))
 #(error "assertment failure: (not (eq? expect 'rest))" (not (eq? 'rest 'rest)))
 > (t '(a b #!optional c #!rest d) '(10 11 12 (13 14)))
 (10 11 12 13 14)
 > (t '(a b #!optional c #!rest d) #f '(f))
 (apply f a b c d))

;; and typed stuff: (ugly adapted copy paste)
(TEST
 > (t '(a #(foo? b) #(bar? c)) '(10 11 12))
 (10 11 12)
 > (t '(a #(foo? b) #(bar? c)) #f '(f))
 (f a b c)
 > (t '(a b #(bar? c)) '(10 11))
 #(error "did not get enough value slots")
 > (t '(a b #(bar? c)) '(10 11 12 13))
 #(error "got too many value slots, still have:" (13))
 > (t '(a b #!optional #(baz? c)) '(10 11))
 #(error "did not get enough value slots")
 > (t '(a b #!optional #(baz? c)) #f '(F))
 (F a b c)
 > (t '(a b #!optional #(baz? c)) '(10 11 12))
 (10 11 12)
 > (t '(a #(bar? b) #!key #(fii? c) #(baz? d)) '(10 11 12 13))
 (10 11 c: 12 d: 13)
 > (t '(a #(bar? b) #!key #(fii? c) #(baz? d)) #f '(f))
 (f a b c: c d: d)
 > (t '(a #(bar? b) #!key #(fii? c) #(baz? d)) '(10 11 12))
 #(error "did not get enough value slots")
 > (t '(a b #!rest #((list-of foo?) c)) '(10 11 (12 13)))
 (10 11 12 13)
 > (t '(a b #!rest #((list-of foo?) c)) #f '(f))
 (apply f a b c)
 > (t '(a b . #((list-of foo?) c)) '(10 11 (12 13)))
 (10 11 12 13)
 > (t '(a b . #((list-of foo?) c)) #f '(f x))
 (apply f x a b c)
 > (t '(a b #!optional #(bar? c) #!rest #((list-of foo?) d)) '(10 11 12 (13 14)))
 (10 11 12 13 14)
 > (t '(a b #!optional #(bar? c) #!rest #((list-of foo?) d)) #f '(f))
 (apply f a b c d))
