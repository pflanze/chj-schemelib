;;; Copyright 2013-2014 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require cj-source
	 cj-match
	 )

(define (maybe-if-form-1ary-head v then)
  (and (pair? v)
       (pair? (cdr v))
       (null? (cddr v))
       (then (car v))))

(define (form-_? sym)
  (lambda (v)
    (maybe-if-form-1ary-head v (lambda (v)
				 (eq? (source-code v) sym)))))

(define form-quote? (form-_? 'quote))
(define form-quasiquote? (form-_? 'quasiquote))
(define form-unquote? (form-_? 'unquote))
(define form-unquote-splicing? (form-_? 'unquote-splicing))

(TEST
 > (form-unquote? '())
 #f
 > (form-unquote? '(unquote a))
 #t
 > (form-unquote? '(unquote))
 #f
 > (form-unquote? '(unquote a . b))
 #f
 > (form-quote? ''a)
 #t
 > (form-quasiquote? '`a)
 #t
 > (form-unquote-splicing? ',@a)
 #t
 )

(define assert:possibly-symbolize-procedures
  ;; XX basically just want to know if it's a toplevel procedure. sgh.
  '(
    ;; R5RS hmm
    = > < >= <= cons car cdr vector vector-ref list list-ref
    length vector-length f64vector-length u8vector-length f32vector-length
    pair? null? zero? negative?
    eq? eqv? equal?
    ;; own
    /= inc dec quotient + - * / arithmetic-shift square sqrt log expt
    eql?
    .type .info
    ))

(define assert:stopping-syntax-forms
  ;; forms that stop from recursing inside
  '(quote quasiquote lambda))

(define assert:syntax-forms
  (append '(and or)
	  assert:stopping-syntax-forms))

(both-times
 (define self-quoting?
   (either string? char? number? boolean?)))

(define (assert:possibly-symbolize v)
  (let ((v* (source-code v)))
    (if (procedure? v*)
	(let lp ((ss assert:possibly-symbolize-procedures))
	  (if (null? ss)
	      (or (##procedure-name v) ;; could use that from the start...? well.
		  v)
	      (let ((sym (car ss))
		    (_else (lambda () (lp (cdr ss)))))
		(cond ((with-exception-catcher
			(lambda (e)
			  (if (unbound-global-exception? e)
			      #f
			      (raise e)))
			(lambda ()
			  (eval sym)))
		       => (lambda (symv)
			    (if (eq? v* symv)
				sym
				(_else))))
		      (else (_else))))))
	(if (self-quoting? v)
	    v
	    `(quote ,v)))))

(compile-time
 (define (assert-replace-expand e)
   (mcase e
	  (pair?
	   (let* ((e* (source-code e)))
	     (if (memq (source-code (car e*)) assert:stopping-syntax-forms)
		 `(quote ,e)
		 `(##cons ,(assert-replace-expand (car e*))
			  ,(assert-replace-expand (cdr e*))))))
	  (symbol?
	   (if (or (define-macro-star-maybe-ref (source-code e))
		   (memq (source-code e) assert:syntax-forms))
	       ;; even though it might be shadowed by a local
	       ;; definition, since we don't have (thanks expander) a
	       ;; way to check for that, we have to be conservative to
	       ;; avoid referencing errors at runtime.
	       `',e
	       `(assert:possibly-symbolize ,e)))
	  (null?
	   ''())
	  (self-quoting?
	   e)
	  (else
	   `(quote (quote ,e))))))

(TEST
 > (assert-replace-expand '(= e1 e2))
 (##cons (assert:possibly-symbolize =)
	 (##cons (assert:possibly-symbolize e1)
		 (##cons (assert:possibly-symbolize e2) '())))
 > (assert-replace-expand '((on foo bar) e1 e2))
 (##cons (##cons 'on
		 (##cons (assert:possibly-symbolize foo)
			 (##cons (assert:possibly-symbolize bar) '())))
	 (##cons (assert:possibly-symbolize e1)
		 (##cons (assert:possibly-symbolize e2) '())))
 > (assert-replace-expand ''e1)
 ''e1
 > (assert-replace-expand '(eq? 'a 'b))
 (##cons (assert:possibly-symbolize eq?) (##cons ''a (##cons ''b '())))
 > (eval #)
 (eq? 'a 'b)
 > (define a 'x)
 > (eval (assert-replace-expand '(eq? "0" a)))
 (eq? "0" 'x)
 )

(define-macro* (assert expr)
  `(if (not ,expr)
       (error ,(string-append "assertment failure: "
			      (scm:object->string (cj-desourcify expr)))
	      ,(assert-replace-expand expr))))

;; > (define a 'a)
;; > (assert (eq? a 'b))
;; *** ERROR IN (console)@5.1 -- assertment failure: (eq? a 'b) (eq? 'x 'b)

;; an |and| that errors for non-true values
(define-macro* (assert-and . es)
  (let rec ((es es))
    (if (null? es)
	`#t
	`(if ,(car es)
	     ,(rec (cdr es))
	     (source-error (source-dequote ',(source-quote (car es)))
			   "xand: got false")))))

;; > (assert-and 1 (= 3 4) 5)
;; *** ERROR IN (console)@86.1 -- This object was raised: #<source-error #7 source: #(#(source1) (#(#(source1) = (console) 983125) #(#(source1) 3 (console) 1114197) #(#(source1) 4 (console) 1245269)) (console) 917589) message: "xand: got false" args: ()>
;; 1> (show-source-error #7)
;; *** ERROR IN syntax, (console)@86.15 -- xand: got false
;; XX: well, not an error in syntax really!f


(define-macro* (V . rest)
  (with-gensym V
	       `(let ((,V ,rest))
		  (warn "V"
			',rest
			'|:|
			,(assert-replace-expand rest)
			'=
			,V)
		  ,V)))

;; > (define i 3)
;; > (V + 2 i)
;; V (+ 2 i) : (+ 2 3) = 5
;; 5
;; > (V (lambda (x) .append) i)
;; V ((lambda (x) .append) i) : ((lambda (x) .append) 3) = #<procedure #2 .append>
;; #<procedure #2 .append>
;;(hm why does the lambda appear decompiled?)


;; adapted COPY from cj-env:
(define (pp-through-source a . r)
  (let* ((port (current-error-port))
	 (pretty (lambda (v)
		   (pretty-print (cj-desourcify v) port)
		   v)))
    (if (pair? r)
	(if (null? (cdr r))
	    (begin
	      (display a port)
	      (display ":" port)
	      (newline port)
	      (pretty (car r)))
	    (error "too many arguments"))
	(begin
	  (pretty a)))))

