;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

(require easy
	 (cj-env-2 current-unixtime))

(export (class realunixtime)
	(methods integer + -)
	current-realunixtime)


(class realunixtime
       (struct #(real? real))

       (method (integer r)
	       (let-realunixtime ((v) r)
				 (integer v)))

       (method (+ r #(real? x))
	       (.real-update r (C + _ x)))

       (method (- r #(real? x))
	       (.real-update r (C - _ x))))


(def (current-realunixtime)
     (realunixtime (time->seconds (current-time))))

