;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.

;;;
;;;; Infrastructure to write monads
;;;

;; general, but depending on the following in the user's scope: >> >>=

(define (m:begin-expand-for maybe-monadprefix)
  (define >> (if maybe-monadprefix
		 (symbol-append maybe-monadprefix ":>>")
		 '>>))
  (named self
	 (lambda exprs
	   (match-list*
	    exprs
	    ((e1 e2 . exprs*)
	     `(,>> ,e1 ,(apply self e2 exprs*)))
	    ((e1)
	     e1)))))

(define-macro* (m:begin . exprs)
  (apply (m:begin-expand-for #f) exprs))

(define-macro* (letm bind expr)
  (match-list*
   bind
   ((var1 expr1)
    `(>>= ,expr1
	  (lambda (,var1) ,expr)))))

;; (define (m:lift f . args)
;;   ..)

;; can't use a function because the >>= >> or m:let needs to be take
;; from the context syntactically.

(define-macro* (m:lift f . args)
  (let ((vs (map (lambda_ (gensym)) args))) ;; XX do I have sth or not for this?
    `(m:let ,(map list vs args)
	    (return ,f ,@vs))))

;; totally general

(define-macro* (m:let binds expr)
  (match-list*
   binds
   ((bind . binds*)
    `(letm ,bind
	   (m:let ,binds*
		  ,expr)))
   (()
    expr)))


;; could write variants taking the kind of monad as parameter; or, if
;; mixing of monads can always be controlled with the following, that
;; seems better:

(define-macro* (use-monad monadname)
  (assert* symbol? monadname
	   (lambda (monadname*)
	     `(begin
		(define >> ,(symbol-append monadname* ":>>"))
		(define >>= ,(symbol-append monadname* ":>>="))
		(define return ,(symbol-append monadname* ":return"))
		;; and a proxy macro..
		(##define-syntax
		 m:inline! ;; with m: prefix?
		 (lambda (stx)
		   (##sourcify-deep
		    (apply
		     (lambda (_name . args)
		       (cons ',(symbol-append monadname* ":inline!") args))
		     (##source-code stx))
		    stx)))))))

(define-macro* (in-monad monadname . body)
  (assert* symbol? monadname
	   (lambda (monadname*)
	     `(let ()
		(use-monad ,monadname)
		(let ()
		  ;; ^ necessary so that "Scheme" doesn't complain
		  ;; about further defines after the ##define-syntax
		  ;; from use-monad?
		  ,@body)))))

