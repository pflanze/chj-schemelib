;;; Copyright 2010-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy)

;;;
;;;; Infrastructure to write monads
;;;


;; This variable, if bound to a non-#f value, determines the monad
;; "type", rather the prefix for .>>, .>>=, ... to use. If #f, the
;; generic .>>, .>>= etc. are used.

;;(def $M #f)
;;XXX bah   no?  would  have  to  wll    eval  access  not lexical,  just not ofcrs.f
;;w bout a macro b  expand it in  macro? no ahve access.  CANT  DO IT !!!!!!!!!!!
;; SICK macro sys FCK_X


(define-macro* (use-monad monadname)
  (assert* symbol? monadname
	   (lambda (monadname*)
	     `(begin

                ;; OK to keep those as functions?
		(define >>= ,(symbol-append monadname* ".>>="))
		(define return ,(symbol-append monadname* ".return"))

		;; (define >> ,(symbol-append monadname* ".>>"))
                ;; Because monadname.>> may need to be a macro, use a
                ;; macro here as well, okay? HACK?
                (##define-syntax
		 >>
		 (lambda (stx)
		   (##sourcify-deep
		    (apply
		     (lambda (_name . args)
		       (cons ',(symbol-append monadname* ".>>") args))
		     (##source-code stx))
		    stx)))

                (##define-syntax
		 minline! ;; with m: prefix?
		 (lambda (stx)
		   (##sourcify-deep
		    (apply
		     (lambda (_name . args)
		       (cons ',(symbol-append monadname* ".inline!") args))
		     (##source-code stx))
		    stx)))))))

(defmacro (in-monad monadname . body)
  (assert* symbol? monadname
	   (lambda (monadname*)
	     `(let ()
		(use-monad ,monadname)
		(let ()
		  ;; ^ necessary so that "Scheme" doesn't complain
		  ;; about further defines after the ##define-syntax
		  ;; from use-monad?
		  ,@body)))))


;; General (except the -in variants), but depending on the above in
;; their scope or using the slower generics:

(def (mdo-expand stx exprs)
     (right-associate (lambda (e r)
                        (sourcify
                         `(>> ,e ,r)
                         e))
                      exprs
                      (lambda (errmsg)
                        (source-error stx errmsg))))

(defmacro (mdo . exprs)
  (mdo-expand stx exprs))

(defmacro (mdo-in monadname . exprs)
  `(in-monad ,monadname
             ,(mdo-expand stx exprs)))

(TEST
 > (expansion#mdo a b c)
 (>> a (>> b c)))



(def (mlet-expand stx bindS expr)
     (def (expand1 bind expr)
          (mcase bind
                 (`(`var1 `expr1)
                  `(>>= ,expr1
                        (lambda (,var1) ,expr)))))
     (assert* pair? bindS
              (lambda (bindS*)
                (if (pair? (source-code (car bindS*)))
                    ;; normal multiple let
                    (fold-right expand1
                                expr
                                bindS*)
                    ;; single "beautiful" let
                    (expand1 bindS expr)))))

(defmacro (mlet bindS expr)
  (mlet-expand stx bindS expr))

(defmacro (mlet-in monadname bindS expr)
  `(in-monad ,monadname
             ,(mlet-expand stx bindS expr)))

(TEST
 > (expansion#mlet ((x 2) (y 3)) x)
 (>>= 2 (lambda (x) (>>= 3 (lambda (y) x))))
 > (expansion#mlet ((x 2)) x)
 (>>= 2 (lambda (x) x))
 > (expansion#mlet (x 2) x)
 (>>= 2 (lambda (x) x)))



;; mlift: can't use a function because the >>= >> or mlet needs to be
;; taken from the context syntactically.

(defmacro (mlift f . args)
  (let ((vs (map (lambda_ (gensym)) args))) ;; XX do I have sth or not for this?
    `(mlet ,(map list vs args)
           (return ,f ,@vs))))

;; XXX: mlift or lift? Is this not one of the methods like return ?

