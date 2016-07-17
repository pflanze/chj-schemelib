;;; Copyright 2013 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require dot-oo ;; not cj-struct
	 test
	 cj-typed)

;; XX oh, now eliminate in favor of typed-list.scm ?

;; uniform lists

;; just for fun

;; also have their length known right away, ok?

(define-struct. upair
  type? ;; a predicate
  length
  car
  cdr)

(define-struct. unull
  type?)

(define. unull.length zero/1)

(define ulist? (either upair? unull?))

(define (ucons val rest)
  (let ((type? (.type? rest)))
    (type-check type? val
		(upair type?
		       (inc (.length rest))
		       val
		       rest))))

(define (list.u.rappend args rest)
  (if (null? args)
      rest
      (list.u.rappend (cdr args)
		      (ucons (car args)
			     rest))))

(define (ulist type? . args)
  (list.u.rappend (reverse args) (unull type?)))

(define (list.u.append a b)
  (list.u.rappend (reverse a) b))

(define (uappend a b)
  (if (unull? a)
      b
      (ucons (.car a)
	     (uappend (.cdr a) b))))


(define. (upair.show v)
  (cons (.car v)
	(.show (.cdr v))))

(define. (unull.show v)
  '())


(TEST
 > (.show (ulist integer? 1 2 3))
 (1 2 3)
 > (.length (ulist integer? 1 2 3))
 3
 > (.show (.cdr (ulist integer? 1 2 3)))
 (2 3)
 > (.car (ulist integer? 1 2 3))
 1
 > (.show (uappend (ulist integer? 1 2) (ulist integer? 10 11)))
 (1 2 10 11)
 > (%try-error (uappend (ulist symbol? 'a 'b) (ulist integer? 10 11)))
 #(error "does not match type?:" b) ;; XX aw, type-check is actually
				    ;; stupid. report function please. FUTURE.
 > (.show (uappend (ulist symbol?) (ulist integer? 10 11)))
 (10 11) ;; XX is this alright or should there be a type error reported?
 )

