;;; Copyright 2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; ternary logic, compress into binary representation

(require easy
	 ;; cj-cmp -- 0 1 2 is easier than cmp values
	 fast-math
	 test)


(def (trit? v)
     (and (fixnum? v)
	  (fx<= 0 v 2)))

(jclass (ternary-number #((list-of trit?) list))

	(def (ternary-number* . l)
	     (ternary-number l))

	(def-method* (length s)
	  (length list))
	
	(def-method* (integer s)
	  (let lp ((tot 0)
		   (l list))
	    (if (pair? l)
		(let-pair ((a l*) l)
			  (lp (+ (* tot 3) a)
			      l*))
		tot))))



(def. (exact-natural0.ternary-number x)
  (ternary-number
   (if (zero? x)
       '(0)
       (let ((vs (values #f #f)))
	 (let lp ((x x)
		  (l '()))
	   (if (zero? x)
	       l
	       (begin
		 (@/3 x vs)
		 (lp (fst vs)
		     (cons (snd vs) l)))))))))

(TEST
 > (map (comp .list .ternary-number) (iota 7))
 ((0)
  (1)
  (2)
  (1 0)
  (1 1)
  (1 2)
  (2 0))
 > (map (comp .integer .ternary-number) (iota 10))
 (0 1 2 3 4 5 6 7 8 9))


(TEST
 > (def t (ternary-number* 0 1 2 0 2 1 1 2 0 2 1))
 > (.length t)
 11
 > (.integer t)
 #b1000011101011000 ;; 34648
 > (.length #)
 16
 > (* 11 3/2)
 33/2
 > (integer #)
 16)


