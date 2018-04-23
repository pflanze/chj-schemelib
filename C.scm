;;; Copyright 2013 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 cj-symbol
	 ;; (cj-env-1 identity) cj-source, sigh
	 (srfi-1 filter))


;; cut, Curry, using |_| as placeholder, without depending on the cut
;; srfi
(define-macro* (C . args)
  (let ((maybe-p-s (map (lambda (v)
			  (if (eq? (source-code v) '_)
			      (gensym)
			      #f))
			args)))
    `(lambda ,(filter identity maybe-p-s)
       ,(map (lambda (v maybe-p)
	       (or maybe-p v))
	     args
	     maybe-p-s))))
