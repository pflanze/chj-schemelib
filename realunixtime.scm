;;; Copyright 2016-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 jclass)

(export (jclass realunixtime
		(methods integer + -))
	current-realunixtime)


;; for representing time points (absolute) only
(jclass (realunixtime #(real? real))

	(def-method (integer r)
	  (integer real))

	(def-method- (+ r #(real? x))
	  (.real-update r (C + _ x)))

	(def-method (- r #(realunixtime? x))
	  (- real (realunixtime.real x))))


(def (current-realunixtime)
     (realunixtime (time->seconds (current-time))))

