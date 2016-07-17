;;; Copyright 2013-2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require cj-source
	 cj-match
	 define-macro-star
	 cj-source-util-2
	 (vector-util vector-map))


(define (source-quasiquote-run u8vec alis)
  ;; alis is list of (symbol splice? . val)
  (cdr
   (let rec ((src (u8vector->object u8vec)))
     (if (symbol? src) ;; no source-code call (more performant)
	 (cond ((assq src alis)
		=> cdr)
	       (else
		(cons #f src)))
	 (let ((run-list
		(cut improper-fold-right*
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
		     <>))
	       (src* (source-code src)))
	   (cond ((pair? src*)
		  (cons #f (possibly-sourcify (run-list src*) src)))
		 ((vector? src*)
		  (cons #f (possibly-sourcify (list->vector
					       (run-list (vector->list src*)))
					      src)))
		 (else
		  (cons #f src))))))))

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
		  (vector?
		   (possibly-sourcify (vector-map rec (source-code src))
				      src))
		  (else
		   src))))
      (##list ,@(reverse forms)))))

(TEST
 > (source-quasiquote-expand '("a" "b"))
 (source-quasiquote-run '#u8(100 17 97 100 17 98 114) (##list))
 > (eval #)
 ("a" "b")
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
 ;; > (quasiquote-source (a b ,(inc 10)))
 ;; #(#(source2)
 ;;    (#(#(source1) a (console) 1310891) #(#(source1) b (console) 1441963) 11)
 ;;    (console)
 ;;    1245355)
 ;; with changing positions of course.
 > (eval (quasiquote-source (inc ,(inc 10))))
 12
 > (eval (quasiquote-source (inc ,(let ((code `(inc 10))) code))))
 12
 > (eval (quasiquote-source (inc ,(let ((code (quasiquote-source (inc 10))))
				    code))))
 12
 )

;; quote multiple pieces of source as a list, return it with the
;; outmost layer being a bare list (e.g. to be used in
;; unquote-splicing forms)
(define-macro* (quasiquote-source-list . src)
  (source-quasiquote-expand src))

(define-macro* (quote-source src)
  `(u8vector->object ',(object->u8vector src)))

;; same as for quasiquote-source-list
(define-macro* (quote-source-list . src)
  `(u8vector->object ',(object->u8vector src)))
