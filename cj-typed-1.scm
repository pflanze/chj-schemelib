;;; Copyright 2010-2014 by Christian Jaeger, ch at christianjaeger ch

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (fallible-1 fallible? fallible-string))


(define (cj-typed#type-check-error use-source-error? maybe-exprstr predstr w v)
  ;; v = value
  ;; w = result of predicate

  (let ((err (lambda strs
	       (let ((msg (apply string-append
				 (if maybe-exprstr
				     (string-append maybe-exprstr " "))
				 "does not match "
				 predstr
				 strs)))
		 (if use-source-error?
		     (source-error v msg)
		     (error (string-append msg ":") v))))))
    (cond ((eq? w #f)
	   (err))
	  ((fallible? w)
	   (err " " (fallible-string w)))
	  (else
	   (error "predicate "
		  predstr
		  " returned invalid non-boolean value:"
		  w)))))


