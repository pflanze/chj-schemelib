;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Functions converting lists to vectors.

(require easy
	 ;;(cj-functional list-of) ;; part of easy?
	 test)

(export list.every-second-vector
	list.map-vector
	list.rmap-vector
	list.rvector)


;; Would really profit from guaranteed-proper length-maintaining (and
;; type maintaining?) lists. Well I've got them. Not using them?
;; Advantage would be that oo dispatch could actually be used sanely,
;; and saving the length trip.

;; allow for lists of uneven length (so that we can get a vector from
;; l then a vector from (cdr l) where l holds key value parings).
(def. (list.every-second-vector l)
  (if (null? l)
      '#()
      (let* ((len (length l))
	     (len* (arithmetic-shift (inc len) -1))
	     (v (make-vector len*)))
	(let lp ((i 0)
		 (l l))
	  (let-pair ((a r) l)
		    (vector-set! v i a)
		    (let ((i* (inc i)))
		      (if (< i* len*)
			  (lp i* (cdr r))))))
	v)))

(TEST
 > (list.every-second-vector '())
 #()
 > (list.every-second-vector '(a 1))
 #(a)
 > (list.every-second-vector '(a 1 b 2))
 #(a b)
 > (list.every-second-vector '(1 b 2))
 #(1 2)
 )

;; really def. ?.. well, above already.
(def. (list.map-vector l fn)
  (let* ((len (length l))
	 (v (make-vector len)))
    (let lp ((i 0)
	     (l l))
      (if (< i len)
	  (let-pair ((a l*) l)
		    (vector-set! v i (fn a))
		    (lp (inc i)
			l*))
	  v))))

(TEST
 > (list.map-vector '(1 2 3) inc)
 #(2 3 4))


(def. (list.rmap-vector l fn)
  (let* ((len (length l))
	 (v (make-vector len)))
    (let lp ((i (dec len))
	     (l l))
      (if (>= i 0)
	  (let-pair ((a l*) l)
		    (vector-set! v i (fn a))
		    (lp (dec i)
			l*))
	  v))))

(TEST
 > (list.rmap-vector '(1 2 3) inc)
 #(4 3 2))


(def. (list.rvector l)
  (list.rmap-vector l identity))

(TEST
 > (list.rvector '(1 2 3))
 #(3 2 1))




