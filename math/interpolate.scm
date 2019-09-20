;;; Copyright 2014-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         test)

(export interpolate
        interpolate*)

(include "../cj-standarddeclares.scm")


(def (interpolate* x1 y1 x2 y2 x)
     (+ y1
        (* (- y2 y1)
           (/ (- x x1) (- x2 x1)))))

(def (interpolate p1 p2 x)
     (let-pair ((x1 y1) p1)
	       (let-pair ((x2 y2) p2)
			 (interpolate* x1 y1 x2 y2 x))))

(TEST
 ;; *does* work on imaginary numbers, too:
 > (def t (C interpolate (cons 1 (sqrt -2)) (cons 2 (sqrt 2)) _))
 > (t 1)
 +1.4142135623730951i
 > (t 2)
 1.4142135623730951+0.i
 > (t 1.5)
 .7071067811865476+.7071067811865476i
 > (t 0.5)
 -.7071067811865476+2.121320343559643i)

