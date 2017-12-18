;;; Copyright 2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 test)


;; XX move
(defmacro (@values! v . exprs)
  (assert* symbol? v)
  `(begin
     ,@(map/iota (lambda (e i)
		   `(##vector-set! ,v ,i ,e))
		 exprs)
     ,v))


;; https://stackoverflow.com/questions/171301/whats-the-fastest-way-to-divide-an-integer-by-3
;;  https://en.wikipedia.org/wiki/Montgomery_reduction

(def (@/3 x vals)
     (if (fixnum? x)
	 (##c-code "
___WORD x= ___INT(___ARG1);
___SCMOBJ vals = ___ARG2;

___VECTORSET(vals, ___FIX(0), ___FIX(x/3));
___VECTORSET(vals, ___FIX(1), ___FIX(x%3));
___RESULT= vals;
" x vals)
	 (@values! vals
		   (quotient x 3)
		   (remainder x 3))))

(def (/3 x)
     (@/3 x (values #f #f)))

(TEST
 > (.list (/3 4))
 (1 1)
 > (.list (/3 1000))
 (333 1)
 > (.list (/3 100000000))
 (33333333 1)
 > (.list (/3 100000000000000000))
 (33333333333333333 1)
 > (.list (/3 100000000000000000000000))
 (33333333333333333333333 1)
 > (.list (/3 -10000000))
 (-3333333 -1)
 > (.list (/3 -100000000000000000000000))
 (-33333333333333333333333 -1))


