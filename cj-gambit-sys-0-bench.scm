;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.


(require easy
	 cj-gambit-sys-0)

(include "cj-standarddeclares.scm")

;; just to verify that cj-gambit-sys-0's ops are properly optimized.

(def (cj-gambit-sys-0-bench:run n vec i val)
     (let ((a (time (repeat n (@vector-ref vec i))))
	   (b (time (repeat n (@vector-length vec))))
	   (c (time (repeat n (@vector-set! vec i val)))))
       (vector a b c)))


(TEST
 > (cj-gambit-sys-0-bench:run 100000000 (vector 39 20 10 4) 2 7)
 [10 4 [39 20 7 4]])

