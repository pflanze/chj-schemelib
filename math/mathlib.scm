(declare (standard-bindings)
	 (extended-bindings)
	 (block))


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


