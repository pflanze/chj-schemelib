;;; Copyright 2013-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.

(require easy test
	 warn-plus)

;; searching functionality

;; inverse: *newton right?*
;; ww.

(define (iterative-improve good-enough? improve-guess start)
  (let iter ((x start))
    (warn "x" x)
    (if (good-enough? x)
	x
	(let ((x* (improve-guess x)))
	  (if (= x x*)
	      (error "improved guess is unchanged:" x)
	      (iter x*))))))


(define (average x y)
  (/ (+ x y) 2))
;; vs |mean| hm?

(def (good-enough y f lo hi)
     ;; y: known value, (f x) where x is what we search.
     ;; make sure that the absolute difference is lower than:
     (def maxdiff (* (- hi lo) 1e-7))
     (lambda (x)
       (< (abs (- (f x) y)) maxdiff)))

;; (def (rec709:transfer.lum t)
;;      (iterative-improve (good-enough t rec709:lum.transfer 0 255)
;; 			(lambda_
;; 			 (average _ (rec709:lum.transfer _)))...etc wll
;; 			128))
;;wrong



;; actually rather need binary search ????
;; how to encode  previous  ?   ?  ?

;; just  f binary search.


;; binary function search
(def (binfsearch y f lo hi)
     (let ((good-enough (good-enough y f lo hi)))
       (let iter ((l lo)
		  (h hi)
		  (yl (f lo))
		  (yh (f hi)))
	 (let* ((x (average l h))
		(y* (f x)))
	   (WARN "iter" (variables x y* yl yh))
	   (if (good-enough x) ;; y*, add variant that does not need f  ? !
	       x
	       ;; In which range for x in (f x) is y to be found? x..l
	       ;; or x..h? Check in which range we are currently, then
	       ;; choose the other side.  (XX do we need to include =
	       ;; cases here?)
	       (let ((cont (lambda (<>)
			     (cond ((<> yl y* y)
				    (WARN "need to go towards h" <>)
				    (iter x h y* yh))
				   ((<> y y* yh)
				    (WARN "need to go towards l" <>)
				    (iter l x yl y*))
				   (else (error "???"))))))
		 (cond ((<= yl y yh)
			(cont <))
		       ((> yl y yh)
			(cont >))
		       (else
			(error "y out of range (non-continuous function, or wrong starting range):" y yl yh)))))))))


(TEST
 > (parameterize
    ((current-WARN-mode #f))
    (local-TEST
     > (binfsearch 2 square 0. 1000000.)
     1.430511474609375
     > (binfsearch 2 sqrt 0. 1000000.)
     3.814697265625
     > (sqrt 3.814697265625)
     1.953125
     ;; hu ? ah. range
     > (binfsearch 2 sqrt 0. 10.)
     3.9999961853027344
     > (binfsearch 0.5 (C - 1.5 _) 0. 255.)
     .9999847412109375
     > (binfsearch 3 (C - 1.5 _) -10. 255.)
     -1.5000176429748535
     > (binfsearch 0.5 (C / 1.5 _) 0.0001 255.)
     3.0000530471801756)))

(def (inverse f lo hi)
     (lambda (x)
       (binfsearch x f lo hi)))

(def (inverse-0-x f)
     (lambda (x)
       (binfsearch x f 0 x)))

(def mysqrt (inverse-0-x square))

(TEST
 > (parameterize
    ((current-WARN-mode #f))
    (local-TEST
     > (mysqrt 4)
     2
     > (mysqrt 2)
     11863283/8388608
     ;; (heh, kinda nice for testing)
     > (mysqrt 10000)
     3355443125/33554432
     ;; 99.99999776482582
     ;; (hmm heh. need more precision, really?)
     )))

;; > (plot mysqrt 0.01 9.99)
;; *** ERROR IN iter, "color.scm"@97.4 -- y out of range (non-continuous function, or wrong starting range): .01 0 1e-4
;; hmm

;; (plot (inverse square 0 10) 0.01 9.99)
;; (plot (list sqrt (inverse square 0 10)) 0.01 9.99)
;; (plot (lambda (x) (- (sqrt x) ((inverse square 0 10) x))) 0.01 9.99)

