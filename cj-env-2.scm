;;; Copyright 2013 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (lib.define-macro-star)
	 (lib.cj-match);?
	 (lib.cj-symbol);?
	 (lib.test)
	 ;; ?
	 (lib.cj-env-1))

;; stop hand-rolling these 0..n-1 loops
(define-macro* (for..< var-from-to . body)
  (mcase var-from-to
         (`(`var `from `to)
          (with-gensym LP
                       `(let ,LP ((,var ,from))
                             (if (< ,var ,to)
                                 (begin
                                   ,@body
                                   (,LP (inc ,var)))))))))

(TEST
 > (let ((v (make-vector 5))) (for..< (i 0 5) (vector-set! v i (* i i))) v)
 #(0 1 4 9 16)
 )


;; move to where?
(define (current-unixtime)
  (inexact->exact (floor (time->seconds (current-time)))))

