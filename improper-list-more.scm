;;; Copyright 2010-2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.

(require easy
	 Maybe
	 test)


(export improper-list-rparts
	improper-list-parts)


(def (improper-list-rparts vS)
     (let lp ((l '())
	      (vS vS))
       (cond ((pair? vS)
	      (let-pair ((a vS*) vS)
			(lp (cons a l)
			    vS*)))
	     ((null? vS)
	      (values l (Nothing)))
	     (else
	      (values l (Just vS))))))


(def (improper-list-parts vS)
     (letv ((rl *v) (improper-list-rparts vS))
	   (values (reverse rl)
		   *v)))

(TEST
 > (values.vector (improper-list-parts '(a b c)))
 #((a b c) #((Nothing)))
 > (values.vector (improper-list-parts '(a b . c)))
 #((a b) #((Just) c))
 > (values.vector (improper-list-parts 'c))
 #(() #((Just) c))
 > (values.vector (improper-list-parts '()))
 #(() #((Nothing))))

