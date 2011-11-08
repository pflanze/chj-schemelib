;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (lib.define-macro-star)
	 (lib.test)
	 (lib.cj-env)
	 (lib.cj-symbol))


;; not sure this is the end of ideas, but, giving it a try
;; (This is an alternative to |cut| from the srfis)

(define-macro* (curry n-more-args f . args)
  (assert** natural? n-more-args
	    (let rec ((n n-more-args)
		      (vs args))
	      (with-gensym
	       v
	       `(lambda (,v)
		  ,(let ((n (dec n))
			 (vs (cons v vs)))
		     (if (zero? n)
			 `(,f ,@(reverse vs))
			 (rec n vs))))))))

(TEST
 > ((curry 1 vector) 'a)
 #(a)
 > (((curry 2 vector) 'a) 'b)
 #(a b)
 > ((curry 1 vector 'zero) 'a)
 #(zero a)
 )
