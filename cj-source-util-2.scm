;;; Copyright 2013-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require cj-source
	 (scheme-meta self-quoting?)
	 test)

(export (macros assert
		assert-privately
		assert-and
		V)
	pp-through-source
	no-pp-through-source
	source-map
	(macros with-source
		with-source*))

(include "cj-standarddeclares-1--include.scm")


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


(define assert:stopping-syntax-forms
  ;; forms that stop from recursing inside
  '(quote quasiquote lambda let let* letrec))

(define assert:syntax-forms
  ;; note: define-macro* forms are automatically handled independently
  (append '(if unless and or time cond case)
	  assert:stopping-syntax-forms))

(define (assert:possibly-symbolize v)
  (if (procedure? v)
      (or (##procedure-name v)
	  v)
      (if (self-quoting? v)
	  v
	  `(quote ,v))))

(both-times
 (define (assert-replace-expand e)
   (let ((e* (source-code e)))
     (cond
      ((pair? e*)
       (let* ()
	 (if (memq (source-code (car e*)) assert:stopping-syntax-forms)
	     `(quote ,e)
	     `(##cons ,(assert-replace-expand (car e*))
		      ,(assert-replace-expand (cdr e*))))))
      ((symbol? e*)
       (if (or (define-macro-star-maybe-ref (source-code e))
	       (memq (source-code e) assert:syntax-forms))
	   ;; even though it might be shadowed by a local
	   ;; definition, since we don't have (thanks expander) a
	   ;; way to check for that, we have to be conservative to
	   ;; avoid referencing errors at runtime.
	   `',e
	   `(assert:possibly-symbolize ,e)))
      ((null? e*)
       ''())
      ((self-quoting? e*)
       e)
      (else
       `(quote (quote ,e)))))))

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
  ;; namespacing ##if is important here, to avoid confusion with if
  ;; from easy-1.scm (yeah, when am I going to do namespacing?)
  `(##if (not ,expr)
         (error ,(string-append "assertment failure: "
                                (scm:object->string (cj-desourcify expr)))
                ,(assert-replace-expand expr))))

(define (assert-privately:error)
  (error "assertment failure"))

;; An assert that does *not* show values or expression (you can get
;; that from the continuation if you've got access to that). This is
;; also necessary when macros are being used in expr (with the current
;; macro expansion system it's not possible to handle these, uh).
(define-macro* (assert-privately expr)
  `(##if (##not ,expr)
	 (assert-privately:error)))


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
			   "assert-and: got false")))))

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

(define no-pp-through-source no-pp-through)


(define (source-map fn vs)
  (possibly-sourcify (map fn (source-code vs)) vs))


;; Are there better names for these?

(define-macro* (with-source vars . body)
  `(let ,(source-map (lambda (var)
		       `(,var (source-code ,var)))
		     vars)
     ,@body))

(define-macro* (with-source* vars . body)
  `(let ,(source-map (lambda (var)
		       `(,(symbol-append (source-code var) "*")
			 (source-code ,var)))
		     vars)
     ,@body))

