;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require dot-oo
	 (cj-source-util-2 assert)
	 (scheme-meta self-quoting)
	 (cj-gambit-sys procedure-name))



(define. (self-quoting.show v)
  v)

(define. (symbol.show v)
  `(quote ,v))


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
   ;; The HACK is: assumption that the constructor carries the same
   ;; name as the type, and that it takes positional arguments
  (cons (struct-type-name v)
	(map .show (struct->values v))))

