;;; Copyright 2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require cj-source ;; basic location stuff
	 cj-cmp
	 test)


(define (location-cmp a b)
  (define (err n v)
    (error (string-append
	    "location-cmp: " n " argument is not a location object:") v))

  (if (location? a)
      (if (location? b)
	  (let ((a-file (location-container a))
		(b-file (location-container b)))
	    (if (eq? a-file b-file)
		(let ((a-pos (location-position a))
		      (b-pos (location-position b)))
		 (let ((a-line (position-line a-pos))
		       (b-line (position-line b-pos)))
		   (if (< a-line b-line)
		       'lt
		       (if (= a-line b-line)
			   (let ((a-col (position-column a-pos))
				 (b-col (position-column b-pos)))
			     (if (< a-col b-col)
				 'lt
				 (if (= a-col b-col)
				     'eq
				     'gt)))
			   'gt))))
		(error "locations are not of the same container:" a b)))
	  (err "second" b))
      (err "first" a)))

(TEST
 > (def c "foo")
 > (def (loc line col)
	(location c (position line col)))
 > (location-cmp (loc 10 10) (loc 10 10))
 eq
 > (location-cmp (loc 9 10) (loc 10 10))
 lt
 > (location-cmp (loc 10 9) (loc 10 10))
 lt
 > (location-cmp (loc 9 10) (loc 10 11))
 lt
 > (location-cmp (loc 10 11) (loc 9 10))
 gt
 > (location-cmp (loc 10 11) (loc 9 11))
 gt)

