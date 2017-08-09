;; Copyright 2016-2017 by Christian Jaeger <ch@christianjaeger.ch>

(require easy
	 jclass)

(export (jclass realunixtime
		(methods integer + -))
	current-realunixtime)


(jclass (realunixtime #(real? real))

	(def-method* (integer r)
	  (integer real))

	(def-method (+ r #(realunixtime? x))
	  (.real-update r (C + _ (realunixtime.real x))))

	(def-method (- r #(realunixtime? x))
	  (.real-update r (C - _ (realunixtime.real x)))))


(def (current-realunixtime)
     (realunixtime (time->seconds (current-time))))

