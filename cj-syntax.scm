;;; Copyright 2013 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Random syntax extensions? (Utilities.)


(require cj-match
	 define-macro-star)


;; Like part of mcase, but simpler and not doing the overhead of
;; calling source-code: 'predicate case'
(define-macro* (pcase expr . cases)
  ;; hard coded optional 'else' syntax in last case (why not just bind
  ;; else to #t? dunno. Isn't that an optimization? Evil?)
  (let ((rcases (reverse cases)))
    (letv ((cases* maybe-else-body)
	   (mcase (car rcases)
		  (`(else . `body)
		   (values (reverse (cdr rcases)) body))
		  (else
		   (values cases #f))))
	  (with-gensym
	   V
	   `(let ((,V ,expr))
	      (cond ,@(map
		       ;; heh now falling back to mcase interesting..
		       (mcase-lambda
			(`(`pred . `body)
			 `((,pred ,V)
			   ,@body)))
		       cases*)
		    ;; don't permit nonmatches by default
		    (else
		     ,@(or maybe-else-body
			   `((pcase-error ,V))))))))))

(define (pcase-error val)
  (error "no match for:" val))

(TEST
 > (pcase "foo" (string? 'yes))
 yes
 > (%try-error (pcase 'foo (string? 'yes)))
 #(error "no match for:" foo)
 > (pcase 'foo (string? 'yes) (else 'no))
 no
 )

