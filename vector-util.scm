;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (lib.cj-env)
	 (lib.test)
	 (lib.vector-util-1))


(define (vectors-map fn vecs accept-uneven-lengths?)
  (let* ((lens (map vector-length vecs))
	 (len (apply min lens))
	 (cont (lambda ()
		 (let* ((res (make-vector len)))
		   (let lp ((i 0))
		     (if (= i len)
			 res
			 (begin
			   (vector-set! res
					i
					(apply fn
					       (map (lambda (vec)
						      (vector-ref vec i))
						    vecs)))
			   (lp (inc i)))))))))
    (if accept-uneven-lengths?
	(cont)
	(let ((lenmax (apply max lens)))
	  (if (= len lenmax)
	      (cont)
	      (error "uneven lengths of input vectors (min max):"
		     len lenmax))))))

(define (vector-map fn . vecs)
  (vectors-map fn vecs #f))

(define (vector-map* fn . vecs)
  (vectors-map fn vecs #t))

(TEST
 > (vector-map + '#(1 2 3) '#(3 4 5))
 #(4 6 8)
 > (with-exception-catcher error-exception-message (lambda () (vector-map + '#(1 2 3) '#(3 4))))
 "uneven lengths of input vectors (min max):"
 > (vectors-map + '(#(1 2 3) #(3 4)) #t)
 #(4 6)
 > (vector-map* + '#(1 2 3) '#(3 4))
 #(4 6)
 )

(define (vector-fold-right fn tail vec)
  (let ((len (vector-length vec)))
    (let rec ((i 0))
      (if (= i len)
	  tail
	  (fn (vector-ref vec i)
	      (rec (inc i)))))))

(define (vector-fold fn tail vec)
  (let ((len (vector-length vec)))
    (let lp ((res tail)
	     (i 0))
      (if (= i len)
	  res
	  (lp (fn (vector-ref vec i)
		  res)
	      (inc i))))))

(TEST
 > (fold vector 'null '(1 2 3))
 #(3 #(2 #(1 null)))
 > (vector-fold vector 'null '#(1 2 3))
 #(3 #(2 #(1 null)))
 > (fold-right vector 'null '(1 2 3))
 #(1 #(2 #(3 null)))
 > (vector-fold-right vector 'null '#(1 2 3))
 #(1 #(2 #(3 null)))
 )

(define (vector-for-each proc vec)
  (vector-fold (lambda (v _)
		 (proc v))
	       #f
	       vec))


;; (define (vector-unfold end? fmap fnext start)
;;   )
;; has the problem of not knowing how many elements it will contain in advance.
;; hence rather: vector-generate

(define (vector-generate size fn/i)
  (let ((res (make-vector size)))
    (let lp ((i 0))
      (if (< i size)
	  (begin
	    (vector-set! res i (fn/i i))
	    (lp (inc i)))))
    res))

(TEST
 > (vector-generate 10 square)
 #(0 1 4 9 16 25 36 49 64 81)
 )


;; (define (vector/list.for-each proc/i+V+L starti endi v l)
;;   ;; (for..< (i starti endi)
;;   ;; 	  )
;;   (let lp ((i starti)
;; 	   (l l))
;;     (if (< i endi)
;; 	(begin
;; 	  (proc/V+L i
;; 		    (vector-ref v i)
;; 		    (car l))
;; 	  (lp (inc i)
;; 	      (cdr l))))))


