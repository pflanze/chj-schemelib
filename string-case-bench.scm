;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 string-case)

(include "cj-standarddeclares.scm")


(def (t1 v)
     (string-case v
		  (("ho") 'ho)
		  (("hi") 'yes)
		  (("hello world") 'theworld)
		  (("hello lovely world how are you today? it's been a long way.")
		   'theworld2)
		  (else 'nomatch)))

(def (t2 v)
     (cond ((string=? v "ho") 'ho)
	   ((string=? v "hi") 'yes)
	   ((string=? v "hello world") 'theworld)
	   ((string=? v "hello lovely world how are you today? it's been a long way.")
	    'theworld2)
	   (else 'nomatch)))

(def (string-case-bench str n)
     (equal? (time (repeat n (t1 str)))
	     (time (repeat n (t2 str)))))

