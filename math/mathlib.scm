;;; Copyright 2013-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 test)

(include "../cj-standarddeclares.scm")


(define (twice x)
  (+ x x))

(define-inline (fx.twice@ x)
  (declare (not safe) (fixnum))
  (fx+ x x))


(define-typed (integer:flfx.* #(flonum? x) #(fixnum? i))
  (integer (* x i)))

(define-inline (integer:flfx.*@ x i)
  (##c-code "
double x= ___FLONUM_VAL(___ARG1);
long i = ___INT(___ARG2);
long res= x * i;
___RESULT=___FIX(res);
" x i))


(define (_integer:flfx.*@ x i)
  (integer:flfx.*@ x i))

(TEST
 > (integer:flfx.* 3.2 3)
 9
 > (_integer:flfx.*@ 3.2 3)
 9
 > (integer:flfx.* 3.2 -3)
 -10
 > (_integer:flfx.*@ 3.2 -3)
 -9
 ;; oh.
 )


