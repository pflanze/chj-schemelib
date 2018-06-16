;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 (simple-match-1 assert*)
	 srfi-1
	 char-util
	 test)

(export (macro $))

(include "cj-standarddeclares.scm")

(define (string-interpolate:variable-char? c)
  (or (char-alphanumeric? c)
      ;; do not allow '!', '.', ',', as those shouldn't be likely ('!'
      ;; and '.'  being used for procedure names hence unprintable)
      ;; and easily used as english punctuation. Similarly, '@', ':'.
      ((char-one-of?/ "-+<>=/*#?") c)))

(define (string-interpolate:expand-with str-expr converter-fn-expr)
  (assert*
   string? str-expr
   (lambda (str)
     (let ((len (string-length str)))
       (define (err rest msg . args)
	 (apply source-error str-expr
		(string-append "(at char pos "
			       (number->string (- len (length rest)))
			       ") "
			       msg)
		args))
       (let lp ((cs (string->list str))
		(rcs '())
		(fragments '()))
	 (define (fragments*)
	   (if (null? rcs)
	       fragments
	       (cons (list->string
		      (reverse rcs))
		     fragments)))
	 (if (null? cs)
	     `(string-append ,@(reverse (fragments*)))
	     (let-pair
	      ((c cs) cs)
	      (if (eq? c #\$)
		  (if (null? cs)
		      (err cs "need variable name after $")
		      (let-pair
		       ((c cs*) cs)
		       (case c
			 ((#\{)
			  (let* ((var (take-while
				       (lambda (c)
					 (not (eq? c #\}))) cs*))
				 (cs (drop cs* (length var))))
			    ;; even empty variable name is OK; even allow '{'?
			    (if (null? cs)
				(err cs "missing '}' after '${'")
				(lp (cdr cs)
				    '()
				    (cons `(,converter-fn-expr
					    ,(string->symbol
					      (list->string var)))
					  (fragments*))))))

			 ;; XX case $( )

			 (else
			  (let* ((var (take-while
				       string-interpolate:variable-char? cs))
				 (cs (drop cs (length var))))
			    (if (null? var)
				(err cs "invalid variable name after $ -- use ${ } for names containing unusual characters")
				(lp cs
				    '()
				    (cons `(,converter-fn-expr
					    ,(string->symbol
					      (list->string var)))
					  (fragments*)))))))))
		  (lp cs (cons c rcs) fragments)))))))))

(TEST
 > (with-exception-catcher source-error-message (& (string-interpolate:expand-with "foo $ " '.string)))
 "(at char pos 5) invalid variable name after $ -- use ${ } for names containing unusual characters"
 > (with-exception-catcher source-error-message (& (string-interpolate:expand-with "foo $" '.string)))
 "(at char pos 5) need variable name after $"
 > (with-exception-catcher source-error-message (& (string-interpolate:expand-with "foo $ {abc} d" '.string)))
 "(at char pos 5) invalid variable name after $ -- use ${ } for names containing unusual characters"
 > (with-exception-catcher source-error-message (& (string-interpolate:expand-with "foo ${abc d" '.string)))
 "(at char pos 11) missing '}' after '${'"
 
 > (string-interpolate:expand-with "foo $a" '.string)
 (string-append "foo " (.string a))
 > (string-interpolate:expand-with "foo $abc d" '.string)
 (string-append "foo " (.string abc) " d")
 > (string-interpolate:expand-with "foo ${abc} d" '.string)
 (string-append "foo " (.string abc) " d")
 > (string-interpolate:expand-with "foo ${abc }d" '.string)
 (string-append "foo " (.string |abc |) "d")
 > (string-interpolate:expand-with "foo ${abc{ }d" '.string)
 ;; XX worrysome, really allow?
 (string-append "foo " (.string |abc{ |) "d"))



(define-macro* ($ str #!optional (converter-fn '.string))
  (string-interpolate:expand-with str converter-fn))

(TEST
 > (define bar-world 11)
 > ($ "foo $bar-world, you")
 "foo 11, you")

