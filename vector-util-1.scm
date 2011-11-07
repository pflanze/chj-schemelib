;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require)


(define (vector-map-1 fn vec)
  ;;(list->vector (map fn (vector->list vec)))
  (let* ((len (vector-length vec))
	 (res (make-vector len)))
    (let lp ((i 0))
      (if (= i len)
	  res
	  (begin
	    (vector-set! res i (fn (vector-ref vec i)))
	    (lp (inc i)))))))

