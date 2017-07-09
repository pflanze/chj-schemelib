;;; Copyright 2013-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 (cj-math square)
	 parallel
	 math/image/pnmatrix
	 math/mathlib
	 math/image/pnmatrix-compiled ;; math/image/pnmatrix-macros ;; pgm8.ref etc.
	 ;;XX?:
	 ;; math/vectorlib
	 )


;; add rim around text or line drawings

(include "../../../cj-standarddeclares.scm")


;; (define (addrim in #!optional out)
  
;;   )

;; sgh how to do that  later  wht  ? .


(define (u8.white? v)
  (declare (not safe))
  (fx> v 250))

(define (u8u8u8.white? r g b)
  (declare (not safe))
  (and (fx> r 250)
       (fx> g 250)
       (fx> b 250)))


;; 'width' of a circle of radius r at distance d from center:
(define (circle-width r d)
  (sqrt (- (square r) (square d))))


(define white-near-r 6)

(define white-near-widths
  (.u8vector (map (compose* integer round (cut circle-width white-near-r <>))
		  (iota white-near-r))))


(define-macro* (define-addrim type _.white? CS BLACK
		 #!optional
		 maybe-HACK-postfix
		 hack-from0
		 hack-to0
		 hack-REPLACEMENTCOLOR)
  (let ((T (symbol-replace-_-with/ type))
	(CS (source-code CS))
	(BLACK (source-code BLACK))
	(maybe-HACK-postfix (source-code maybe-HACK-postfix))
	(hack-REPLACEMENTCOLOR (source-code hack-REPLACEMENTCOLOR)))
    (quasiquote-source
     (begin
       ,(if maybe-HACK-postfix
	    `(begin)
	    (quasiquote-source
	     (begin
	       (define (,(T '_.white-near?@) m s0 s1 i0 i1)
		 (declare (fixnum) (not safe))
		 (continuation-capture
		  (lambda (return)
		    (let ((found (thunk (continuation-return return #t)))
			  (white? (lambda (j0 j1)
				    (and (fx>= j0 0)
					 (fx>= j1 0)
					 (fx< j0 s0)
					 (fx< j1 s1)
					 (,(T '_.ref@) m j0 j1 ,_.white?)))))
		      (for..<* (d0 1 white-near-r)
			       (let ((d1* (u8vector-ref white-near-widths d0)))
				 (if (or (white? (fx+ i0 d0) i1)
					 (white? (fx- i0 d0) i1))
				     (found)
				     (for..<* (d1 1 d1*)
					      (if (or (or (white? i0 (fx+ i1 d1))
							  (white? i0 (fx- i1 d1)))
						      (white? (fx+ i0 d0) (fx+ i1 d1))
						      (white? (fx+ i0 d0) (fx- i1 d1))
						      (white? (fx- i0 d0) (fx+ i1 d1))
						      (white? (fx- i0 d0) (fx- i1 d1)))
						  (found))))))
		      #f))))

	       (define. ,(T '_.copy-range!)
		 (typed-lambda
		  (#(size0? l0) #(size0? l1)
		    #(,(T '_?) t) #(size0? t0) #(size0? t1)
		    #(,(T '_?) f) #(size0? f0) #(size0? f1))
	  
		  (let-values (((ts0 ts1) (.sizes t))
			       ((fs0 fs1) (.sizes f)))
		    (assert (<= (+ t0 l0) ts0))
		    (assert (<= (+ t1 l1) ts1))
		    (assert (<= (+ f0 l0) fs0))
		    (assert (<= (+ f1 l1) fs1))
		    (parallel-for..<*
		     500
		     (i0 0 l0)
		     (for..<*
		      (i1 0 l1)
		      (,(T '_.ref@)
		       f (fx+ f0 i0) (fx+ f1 i1)
		       (lambda ,CS
			 (,(T '_.set!@) t (fx+ t0 i0) (fx+ t1 i1) ,@CS))))))))

	       (define. ,(T '_.fill-range!)
		 (typed-lambda
		  (#(,(T '_?) m) #(size0? p0) #(size0? p1) #(size0? l0) #(size0? l1)
		    ;;XX type check CS ?.
		    ,@CS)
		  (letv
		   ((s0 s1) (.sizes m))
		   (assert (<= (+ p0 l0) s0))
		   (assert (<= (+ p1 l1) s1))
		   (parallel-for..<*
		    1000
		    ;; XX should it check number of pixels ? I
		    ;; mean. hm. pass num of jobs here instead? or, est
		    ;; cost per iteration.
		    (i0 p0 (+ p0 l0))
		    (for..<*
		     (i1 p1 (+ p1 l1))
		     (,(T '_.set!@) m i0 i1 ,@CS))))))

	       (define. ,(T '_.cut)
		 ;; from0 from1 len0 len1
		 (lambda (m f0 f1 l0 l1
			    #!optional
			    (newborder 0)
			    ,@(map (lambda (var)
				     `(,var 255))
				   CS))
		   (let* ((s0 (+ l0 (* 2 newborder)))
			  (s1 (+ l1 (* 2 newborder)))
			  (res (,(T 'make-_) "cut" s0 s1)))
		     (,(T '_.copy-range!) l0 l1 res newborder newborder m f0 f1)
		     (if (not (zero? newborder))
			 (begin
			   ;; left
			   (,(T '_.fill-range!) res 0 0 s0 newborder ,@CS)
			   ;; top
			   (,(T '_.fill-range!) res 0 0 newborder s1 ,@CS)
			   ;; right
			   (,(T '_.fill-range!) res 0 (- s1 newborder) s0 newborder ,@CS)
			   ;; bottom
			   (,(T '_.fill-range!) res (- s0 newborder) 0 newborder s1  ,@CS)))
		     res)))

	       (define. ,(T '_.scale-to)
		 (lambda (m s0* s1*)
		   (letv ((s0 s1) (.sizes m))
			 (let* ((res (,(T 'make-_) "scale" s0* s1*))
				(n0 (integer (+ 0.5 (/ s0 s0*))))
				(n1 (integer (+ 0.5 (/ s1 s1*))))
				(n (* n0 n1))
				(scale0 (exact->inexact (/ s0 s0*)))
				(scale1 (exact->inexact (/ s1 s1*)))
				(num (u32vector 0))
				(tot (u32vector ,@(iota (length CS)))))
			   (assert (not (zero? n0)))
			   (assert (not (zero? n1)))
			   (parallel-for-all
			    100
			    res
			    (s0* s1*)
			    (i0 i1)
			    (let ()
			      (declare (not safe))
			      ;;init
			      (u32vector-set! num 0 0)
			      ,@(map (lambda (i)
				       `(u32vector-set! tot ,i 0))
				     (iota (length CS)))
			      ;; sum
			      (let ((start0 (integer:flfx.*@ scale0 i0))
				    (start1 (integer:flfx.*@ scale1 i1)))
				(for..<* (j0 start0 (fx+ start0 n0))
					 (for..<* (j1 start1 (fx+ start1 n1))
						  (,(T'_.ref@) m j0 j1
						   (lambda ,CS
						     ,@(map (lambda (c i)
							      `(u32vector-set!
								tot ,i
								(fx+ (u32vector-ref tot ,i)
								     ,c)))
							    CS
							    (iota (length CS))))))))
			      (,(T'_.set!@) res i0 i1 ,@(map (lambda (i)
							       `(##fixnum.quotient
								 (u32vector-ref tot ,i)
								 n))
							     (iota (length CS))))))
			   res))))

	       (define. ,(T '_.scale)
		 (lambda (m factor)
		   (letv ((s0 s1) (.sizes m))
			 (,(T '_.scale-to)
			  m
			  (integer (* factor s0))
			  (integer (* factor s1)))))))))

       (define. (,(symbol-append (T '_.addrim) (or maybe-HACK-postfix "")) m1)
	 (letv ((s0 s1) (.sizes m1))
	       (let* ((m2 (,(T 'make-_) "addrim" s0 s1)))
		 (parallel-for..<*
		  100
		  (i0 0 s0)
		  (for..<* (i1 0 s1)
			   (,(T '_.ref@)
			    m1 i0 i1
			    (lambda ,CS
			      (if (,_.white? ,@CS)
				  ;;(,(T '_.set!@) m2 i0 i1 ,@CS)
				  ,(if maybe-HACK-postfix
				       `(if (fx< ,hack-from0 i0 ,hack-to0)
					    (,(T '_.set!@) m2 i0 i1
					     ,@hack-REPLACEMENTCOLOR)
					    (,(T '_.set!@) m2 i0 i1 ,@CS))
				       `(,(T '_.set!@) m2 i0 i1 ,@CS))
				  (if (,(T '_.white-near?@) m1 s0 s1 i0 i1)
				      (,(T '_.set!@) m2 i0 i1 ,@BLACK)
				      (,(T '_.set!@) m2 i0 i1 ,@CS)))))))
		 m2)))))))


(define-addrim pgm8 u8.white? (v) (0))
(define-addrim ppm8 u8u8u8.white? (r g b) (0 0 0))

(define-addrim ppm8 u8u8u8.white? (r g b) (0 0 0)
  ;; yellow ffea00 ffe500
  -hackyellow 802 1752 (255 229 0))

