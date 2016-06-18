;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; XXX shouldn't this be merged with dsssl.scm ?

(require test
	 (cj-source-util-2 assert)
	 (list-util let-pair)
	 (cj-source source-error)
	 (srfi-1 append-reverse))

(export source.bindings->app
	#!optional
	source.list-of-length-2?)

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
(define (source.bindings->app bind vs)
  ;; "state machine"
  ;; expect: 'positional | 'optional | 'key | 'rest | 'end
  (let lp ((s (source-code bind))
	   (vs vs)
	   (expect 'positional)
	   (res '()))

    (if (null? s)
	(if (null? vs)
	    (reverse res)
	    (error "got too many value slots, still have:"
		   vs))
	(let-pair
	 ((b s*) s)
	 (let ((*b (source-code b))
	       (lp-expect (lambda (expect)
			    (lp s* vs expect res))))
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
		 (source-error b "superfluous item"))

		(else
		 (cond ((or (symbol? *b)
			    (source.list-of-length-2? *b))
			(if (null? vs)
			    (error "did not get enough value slots")

			    (case expect
			      ((positional optional)
			       (lp s*
				   (cdr vs)
				   expect
				   (cons (car vs) res)))

			      ((rest)
			       (lp s*
				   (cdr vs)
				   'end
				   (append-reverse (car vs) res)))

			      ((key)
			       (lp s*
				   (cdr vs)
				   'key
				   (cons (car vs)
					 (cons (string->keyword (symbol->string *b))
					       res))))
			      (else
			       (error "in invalid mode:" expect)))))
		       (else
			(source-error b
				      "expecting symbol or list of length 2"))))))))))))


(TEST
 > (define (t b vs) (%try-error (source.bindings->app b vs)))
 > (t '(a b c) '(10 11 12))
 (10 11 12)
 > (t '(a b c) '(10 11))
 #(error "did not get enough value slots")
 > (t '(a b c) '(10 11 12 13))
 #(error "got too many value slots, still have:" (13))
 > (t '(a b #!optional c) '(10 11))
 #(error "did not get enough value slots")
 > (t '(a b #!optional c) '(10 11 12))
 (10 11 12)
 > (t '(a b #!key c d) '(10 11 12 13))
 (10 11 c: 12 d: 13)
 > (t '(a b #!key c d) '(10 11 12))
 #(error "did not get enough value slots")
 > (t '(a b #!rest c) '(10 11 (12 13)))
 (10 11 12 13)
 > (t '(a b #!rest #!rest) '(10 11 (12 13)))
 #(error "assertment failure: (not (eq? expect 'rest))" (not (eq? 'rest 'rest)))
 > (t '(a b #!optional c #!rest d) '(10 11 12 (13 14)))
 (10 11 12 13 14)
 )

