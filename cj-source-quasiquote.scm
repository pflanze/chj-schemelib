;;; Copyright 2013 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require cj-source
	 cj-match
	 define-macro-star
	 ;; etc..
	 )


(define (form-unquote? v)
  (and (pair? v)
       (pair? (cdr v))
       (null? (cddr v))
       (eq? (source-code (car v)) 'unquote)))

(define (form-unquote-splicing? v)
  (and (pair? v)
       (pair? (cdr v))
       (null? (cddr v))
       (eq? (source-code (car v)) 'unquote-splicing)))

(TEST
 > (form-unquote? '())
 #f
 > (form-unquote? '(unquote a))
 #t
 > (form-unquote? '(unquote))
 #f
 > (form-unquote? '(unquote a . b))
 #f
 > (form-unquote-splicing? ',@a)
 #t
 )


(define (source-quasiquote-run u8vec alis)
  ;; alis is list of (symbol splice? . val)
  (cdr
   (let rec ((src (u8vector->object u8vec)))
     (if (symbol? src) ;; no source-code call (more performant)
	 (cond ((assq src alis)
		=> cdr)
	       (else
		(cons #f src)))
	 (if (pair? (source-code src))
	     (cons #f
		   (possibly-sourcify
		    (improper-fold-right*
		     (lambda (improper? src rest)
		       (let-pair
			((splice? val) (rec src))
			(if improper?
			    (if splice?
				(error "can't splice in improper tail pos")
				val)
			    (if splice?
				(append val rest)
				(cons val rest)))))
		     '()
		     (source-code src))
		    src))
	     ;; XX support for arrays!
	     (cons #f src))))))

(TEST
 > (source-quasiquote-run (object->u8vector '(a b c . d)) '((b #f . B)
							    (c #t 1 2 3)
							    (d #f . 9)))
 (a B 1 2 3 . 9)
 )

(define (source-quasiquote-expand src)
  (let* ((forms '())
	 (push-form! (lambda (form)
		       (set! forms (cons form forms)))))
    `(source-quasiquote-run
      ',(object->u8vector
	 ;; XX support quasiquote nesting (either source-quasiquote or
	 ;; quasiquote or both; but how exactly again now)
	 (let rec ((src src))
	   (mcase src
		  (form-unquote?
		   (with-gensym ID
				(begin
				  (push-form!
				   `(##cons ',ID
					    (##cons #f
						    ,(cadr (source-code src)))))
				  ID)))
		  (form-unquote-splicing?
		   (with-gensym ID
				(begin
				  (push-form!
				   `(##cons ',ID
					    (##cons #t
						    ,(cadr (source-code src)))))
				  ID)))
		  (pair?
		   (let ((src* (source-code src)))
		     (possibly-sourcify (cons (rec (car src*))
					      (rec (cdr src*)))
					src)))
		  ;; XX handle arrays
		  (else
		   src))))
      (##list ,@(reverse forms)))))

(TEST
 > (source-quasiquote-expand '(a b c))
 (source-quasiquote-run '#u8(100 1 97 0 100 1 98 1 100 1 99 1 114) (##list))
 > (eval #)
 (a b c)
 > (eval (source-quasiquote-expand '(a b ,(inc 9))))
 (a b 10)
 > (eval (source-quasiquote-expand '(a b ,@(list 9 10))))
 (a b 9 10)
 > (eval (source-quasiquote-expand '(a b ,@(list 9 10) 11)))
 (a b 9 10 11)
 > (eval (source-quasiquote-expand '(a b ,@(list 9 10) . 11)))
 (a b 9 10 . 11)
 > (eval (source-quasiquote-expand '(a b . ,(list 9 10))))
 (a b 9 10)
 )


;; call it quasiquote-source or source-quasiquote?

(define-macro* (quasiquote-source src)
  (source-quasiquote-expand src))

(TEST
 > (quasiquote-source (a b ,(inc 10)))
 #(#(source2)
    (#(#(source1) a (console) 1310891) #(#(source1) b (console) 1441963) 11)
    (console)
    1245355)
 > (eval (quasiquote-source (inc ,(inc 10))))
 12
 > (eval (quasiquote-source (inc ,(let ((code `(inc 10))) code))))
 12
 > (eval (quasiquote-source (inc ,(let ((code (quasiquote-source (inc 10))))
				    code))))
 12
 )

