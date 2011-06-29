;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.


;; utility:

(define-macro* (define-nested bind expr)
  (let ((bind* (source-code bind)))
    (if (pair? bind*)
       (let-pair
	((a r) bind*)
	`(define-nested ,a
	   (lambda ,r
	     ,expr)))
       `(define ,bind ,expr))))

(TEST
 > (define-nested ((f x) y) (list x y))
 > ((f 1) 2)
 (1 2)
 > (define-nested ((f) y) (list  y))
 > ((f) 2)
 (2)
 > (define-nested (f y) (list  y))
 > (f 2)
 (2))


