;;; Copyright 2014-2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 test)

(def. (exact.remainder x)
  (remainder (numerator x)
	     (denominator x)))

(TEST
 > (.remainder 8/7)
 1
 > (.remainder 8/9)
 8
 > (.remainder 10/9)
 1
 )

;; floor is there already
;; > (floor 6/7)
;; 0
;; > (floor 8/7)
;; 1


(def. (integer.binary x)
  (number->string x 2))

