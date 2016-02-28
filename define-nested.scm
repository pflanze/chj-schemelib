;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 cj-source
	 test)


;; utility:

(define-macro* (define-nested bind expr)
  (let ((bind* (source-code bind)))
    (if (pair? bind*)
	`(define-nested ,(car bind*)
	   (lambda ,(cdr bind*)
	     ,expr))
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


