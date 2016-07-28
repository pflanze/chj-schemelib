;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 (srfi-1 fold)
	 (cj-functional flip)
	 test)

;; left-associative nested application ("uncurry", or rather "uncut")

(def (U-expand args)
     (fold (flip list)
	   (car args)
	   (cdr args)))

(TEST
 > (U-expand '(A b c))
 ((A b) c)
 > (U-expand '(A b))
 (A b)
 > (U-expand '(A))
 A)


(define-macro* (U . args)
  (U-expand args))

