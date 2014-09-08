;;; Copyright 2013-2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(declare (standard-bindings)
	 (extended-bindings)
	 (block))


(require cj-math)

(def 90° (* 0.5 pi))

(class 2d-polar
       (struct #(real? angle)
	       #(real? distance))
       (method (point v)
	       (let-2d-polar
		((angle distance) v)
		(2d-point (* distance (cos angle))
			  (* distance (sin angle))))))

(TEST
 > (.point (2d-polar 0 0))
 #(2d-point 0 0)
 > (.point (2d-polar 0 1))
 #(2d-point 1 0)
 > (.point (2d-polar 90° 1))
 #(2d-point 6.123233995736766e-17 1.) ;; or something
 > (.point (2d-polar (* 2 90°) 1))
 #(2d-point -1. 1.2246467991473532e-16)
 > (.point (2d-polar (* 3 90°) 1))
 #(2d-point -1.8369701987210297e-16 -1.)
 )

(def. (2d-point.polar v)
  (let-2d-point
   ((x y) v)
   (let ((distance (sqrt (+ (square x)
			    (square y)))))
     (2d-polar (atan y x)
	       distance))))

(TEST
 > (.polar (2d-point 2 2))
 #(2d-polar .7853981633974483 2.8284271247461903)
 > (.point #)
 #(2d-point 2.0000000000000004 2.)
 > (.polar (2d-point 2 -2))
 #(2d-polar -.7853981633974483 2.8284271247461903)
 > (.point #)
 #(2d-point 2.0000000000000004 -2.)

 > (.polar (.point (2d-polar 0 0)))
 #(2d-polar 0 0)
 > (.polar (.point (2d-polar 0 1)))
 #(2d-polar 0 1)
 > (.polar (.point (2d-polar 90° 2)))
 #(2d-polar 1.5707963267948966 2.)
 > (.polar (.point (2d-polar 1 2)))
 #(2d-polar 1. 2.)

 > (.polar (2d-point 1 2000))
 #(2d-polar 1.5702963268365633 2000.0002499999844)
 > (.polar (2d-point 1 -2000))
 #(2d-polar -1.5702963268365633 2000.0002499999844)
 > (.polar (2d-point 0 2000))
 #(2d-polar 1.5707963267948966 2000)
 > (.polar (2d-point -1 2000))
 #(2d-polar 1.57129632675323 2000.0002499999844)

 > (.polar (.point (2d-polar -2 2)))
 #(2d-polar -2. 2.)
 > (.polar (.point (2d-polar 2 2)))
 #(2d-polar 2. 2.))

