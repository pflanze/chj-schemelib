;;; Copyright 2010-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 cj-phasing
	 fixnum
	 (cj-source source-code))

(export (macro first-then/)
	(macro on))


(both-times
 (define (make-list n v)
   (let lp ((n n)
	    (res '()))
     (if (positive? n)
	 (lp (dec n)
	     (cons v res))
	 res))))

(define-macro* (first-then/ arity* access cmp)
  ;; copy to avoid dependency
  (define natural? (lambda (x) (and (integer? x) (positive? x))))
  ;;/copy
  (let ((arity (eval arity*)))
    (if (natural? arity)
	(let* ((VARS (map gensym (make-list arity 'v)))
	       (lam (lambda (ACCESS)
		      `(lambda ,VARS
			 (,cmp ,@(map (lambda (V)
					`(,ACCESS ,V))
				      VARS))))))
	  (if (symbol? (source-code access))
	      (lam access)
	      (let ((ACCESS (gensym 'access)))
		`(let ((,ACCESS ,access))
		   ,(lam ACCESS))))))))


;; tests see cj-env

(define-macro* (on access cmp)
  `(first-then/ 2 ,access ,cmp))

