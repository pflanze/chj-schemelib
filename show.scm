;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require dot-oo
	 (cj-source-util-2 assert)
	 (scheme-meta self-quoting)
	 (cj-gambit-sys procedure-name)
	 (srfi-11 values? values->list)
	 test)


(export (method show)
	try-show
	#!optional
	toplevel-procedure?
	struct->values)


(define. (self-quoting.show v)
  v)

(define. (symbol.show v)
  `(quote ,v))


;; XX if not struct. ugly implicit assumption through ordering
;; currently. More generic must be earlier.

(define. (vector.show v)
  `(vector ,@(map .show (vector->list v))))

(define. (pair.show v)
  `(cons ,(.show (car v))
	 ,(.show (cdr v))))

(define. (list.show v)
  (cons 'list (map .show v)))


(define. (values.show v)
  (cons 'values (map .show (values->list v))))

(define. (box.show v)
  `(box ,(.show (unbox v))))


;; XX move? to predicates or rather cj-gambit-sys?
(define (toplevel-procedure? v)
  (and (procedure? v)
       (maybe-procedure-name v)
       #t))

(define. toplevel-procedure.show maybe-procedure-name)

;; structs:

(define (struct->values v)
  (let ((l (vector->list v)))
    (assert (struct-tag? (car l)))
    (cdr l)))

;; XXX HACK for now, should generate for each struct according to its
;; (default) constructor:

(define. (struct.show v)
   ;; The HACK is: assumption that the constructor takes positional
   ;; arguments
  (cons (struct-constructor-name v)
	(map .show (struct->values v))))



(TEST
 > (.show '(1 2 3))
 (list 1 2 3)
 > (.show '(1 2 . 3))
 (cons 1 (cons 2 3))
 > (.show (values (+ 1 2) 2))
 (values 3 2))



(define (try-show v)
  (with-exception-catcher
   (lambda (e)
     `(try-show ,v))
   (lambda ()
     (.show v))))

;; (def. (exception.show e)
;;   `(raise ,e))
;; oh {##,}exception? doesn't exist

(define. (error-exception.show e)
  `(error ,(error-exception-message e)
	  ,@(map try-show (error-exception-parameters e))))

(define. (unbound-global-exception.show e)
  ;;`(unbound-global-exception )  hmm or really simply?:
  (unbound-global-exception-variable e)
  )