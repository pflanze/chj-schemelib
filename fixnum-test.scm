;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require fixnum
	 (cj-gambit-sys max-fixnum)
	 test)


;; "should allow anything that works for vector access. Then could do
;; unsafe. What about wrap-around?"


;; compiled:
(def fixnum-test:inc (C inc _))
(def fixnum-test:dec (C dec _))

(TEST
 > (with-exception-catcher fixnum-overflow-exception?
			   (& (fx+ max-fixnum 1)))
 #t
 > (with-exception-catcher fixnum-overflow-exception?
			   (& (inc max-fixnum)))
 #t
 > (with-exception-catcher fixnum-overflow-exception?
			   (& (dec min-fixnum)))
 #t

 > (with-exception-catcher fixnum-overflow-exception?
			   (& (fixnum-test:inc max-fixnum)))
 #t
 > (with-exception-catcher fixnum-overflow-exception?
			   (& (fixnum-test:dec min-fixnum)))
 #t)

