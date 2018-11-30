;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 test)

;; https://www.scheme.com/tspl3/control.html

;; (define-syntax fluid-let
;;   (syntax-rules ()
;;     ((_ ((x v)) e1 e2 ...)
;;      (let ((y v))
;;        (let ((swap (lambda () (let ((t x)) (set! x y) (set! y t)))))
;;          (dynamic-wind swap (lambda () e1 e2 ...) swap))))))


(define-macro* (fluid-let var+expr-s . body)
  (assert*
   list? var+expr-s
   (lambda (var+expr-s*)

     (let ((list-of-2? (lambda (v)
			 (and (list? v)
			      (= (length v) 2)))))
       (let ((.var (lambda (var+expr)
		     (assert* list-of-2? var+expr
			      (lambda (var+expr)
				(assert* symbol? (car var+expr) values)))))
	     (.expr (lambda (var+expr)
		      (assert* list-of-2? var+expr cadr))))

	 (let* ((var->kept_
		 (map (lambda (var+expr)
			(cons (.var var+expr) (gensym)))
		      var+expr-s*))
		(var->kept (lambda (var)
			     (assert* symbol? var
				      (lambda (var)
					(cdr (assq var var->kept_))))))
		(SWAP (gensym 'swap))
		(TMP (gensym 'tmp)))
	   `(##let ,(map (lambda (var+expr)
			   `(,(var->kept (.var var+expr)) ,(.expr var+expr)))
			 var+expr-s*)
		   (##let ((,SWAP (##lambda ()
				       ,@(map (lambda (var+expr)
						(let ((var (.var var+expr))
						      (expr (.expr var+expr)))
						  (let ((kept (var->kept var)))
						    `(##let ((,TMP ,var))
							    (##set! ,var ,kept)
							    (##set! ,kept ,TMP)))))
					      var+expr-s*))))
			  (##dynamic-wind
			   ,SWAP
			   (##lambda ()
				,@body)
			   ,SWAP)))))))))


(TEST
 > (define TEST:equal? syntax-equal?)
 > (expansion#fluid-let ((a 1) (b 2)) a)
 (##let ((GEN:-3915 1) (GEN:-3916 2))
	(##let ((GEN:swap-3917
		 (##lambda ()
		      (##let ((GEN:tmp-3918 a))
			     (##set! a GEN:-3915)
			     (##set! GEN:-3915 GEN:tmp-3918))
		      (##let ((GEN:tmp-3918 b))
			     (##set! b GEN:-3916)
			     (##set! GEN:-3916 GEN:tmp-3918)))))
	       (##dynamic-wind GEN:swap-3917 (##lambda () a) GEN:swap-3917))))


(TEST
 > (let ((x 3))
     (+ (fluid-let ((x 5))
	  x)
	x))
 8
 ;; A fluid-bound variable also reverts to the old value if a
 ;; continuation created outside of the fluid-let is invoked.
 > (let ((x 'a))
     (let ((f (lambda () x)))
       (cons (call/cc
	      (lambda (k)
		(fluid-let ((x 'b))
		  (k (f)))))
	     (f))))
 (b . a)
 ;; If control has left a fluid-let body, either normally or by the
 ;; invocation of a continuation, and control reenters the body by the
 ;; invocation of a continuation, the temporary value of the
 ;; fluid-bound variable is reinstated. Furthermore, any changes to
 ;; the temporary value are maintained and reflected upon reentry.
 > (define reenter #f)
 > (define x 0)
 > (define _return values)
 ;; this wrapper is needed as otherwise we would rely on the function
 ;; position in the use of |return| being evaluated after its
 ;; argument:
 > (define (return x) (_return x))
 > (return
    (fluid-let ((x 1))
      (call/cc (lambda (k) (set! reenter k)))
      (set! x (+ x 1))
      x))
 2
 > x
 0
 > (define (rerun)
     (call/cc (lambda (c)
		(set! _return c)
		(reenter '*))))
 > (rerun)
 3
 > (rerun)
 4
 > x
 0)
