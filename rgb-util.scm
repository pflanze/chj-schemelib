;;; Copyright 2013-2016 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 ;; test
	 )

(export rgb:0..1?)


(def 01-margin 0.001)
(def min01 (- 01-margin))
(def max01 (+ 1 01-margin))

(def (rgb:0..1? v)
     (and (real? v)
	  (<= min01 v)
	  (<= v max01)))



