;;; Copyright 2013-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 math/vectorlib ;; Mr ...
	 math/image/effects/utils
	 (math/mathlib fx.twice@)
	 (math/vectorlib-1 view)
	 constants
	 )


(declare (standard-bindings)
	 (extended-bindings)
	 (block))

;; matrix of random values?


(define (random-boolean)
  (zero? (random-integer 2)))

(define (random-sign)
  (dec (* 2 (random-integer 2))))

(define (random-offset)
  ;;(* (- (random-real) 0.5) 5)
  ;;XX rather  log something  pls  expt  wt.
  (* (random-sign) (log (random-real))))


(define-inline (lowpass-attenuation-at@ x)
  ;; x 0..1
  ;; 0..1 -> 0..1 ?
  (declare (flonum)
	   (not safe)
	   )
  (expt 200. (cos (* (freqmod@ x) (CONST (* pi 0.5))))))

(define (r:ii./ a b)
  (declare (flonum))
  (/ (exact->inexact a) (exact->inexact b)))

(define-macro* (UNSAFE . body)
  `(let ()
     (declare (not safe))
     ,@body))

;; (define (*real x . rs)
;;   (+ (apply * (real-part x) rs)
;;      (imag-part x)))

(define-inline (cr.*real@ x r)
  (+ (UNSAFE (fl* (real-part x) r))
     (UNSAFE (imag-part x))))
(define-inline (crr.*real@ x r1 r2)
  (+ (UNSAFE (fl* (real-part x) r1 r2))
     (UNSAFE (imag-part x))))


;; (define (lowpass0 m)
;;   (let ((s0 (.size0 m)))
;;     (.map+ m (lambda (x i0 i1)
;; 	       (cr.*real@ x (square@ (lowpass-attenuation-at@ (r:ii./ i0 s0))))))))

;; (define (lowpass1 m)
;;   (let ((s1 (.size1 m)))
;;     (.map+ m (lambda (x i0 i1)
;; 	       (cr.*real@ x (square@ (lowpass-attenuation-at@ (r:ii./ i1 s1))))))))

;; (define (lowpass2 m)
;;   (letv ((s0 s1) (.sizes m))
;; 	(.map+ m (lambda (x i0 i1)
;; 		   (crr.*real@ x
;; 			       (lowpass-attenuation-at@ (r:ii./ i0 s0))
;; 			       (lowpass-attenuation-at@ (r:ii./ i1 s1)))))))

(define-inline (Mc.map+real! m fn)
  ;; consumes m (but returns it)
  (letv ((s0 s1) (Mc.sizes m))
	(let ((data (Mc.data m)))
	  (declare (not safe) (fixnum))
	  (for..< (i0 0 s0)
		  (let ((row (@Vc.data (vector-ref data i0))))
		    (for..< (i1 0 s1)
			    (let ((i (fx.twice@ i1)))
			      ;;(declare (not safe) (flonum)) ;; needed?
			      (Vr.set!@ row i
					(fn (Vr.ref@ row i) i0 i1))))))))
  m)

(define-inline (Mr..*! m x)
  ;; consumes m (but returns it)
  (letv ((s0 s1) (Mr.sizes m))
	(let ((data (Mr.data m)))
	  (declare (not safe) (fixnum))
	  (for..< (i0 0 s0)
		  ;; XX don't have @Vr.data
		  (let ((row (Vr.data (vector-ref data i0))))
		    (for..< (i1 0 s1)
			    ;;(declare (not safe) (flonum));; needed?
			    (Vr.set!@ row i1 (fl* (Vr.ref@ row i1) x)))))))
  m)


(define (lowpass2! m)
  (letv ((s0 s1) (.sizes m))
	(Mc.map+real! m
		      (lambda (xreal i0 i1)
			(declare (not safe) (flonum))
			(fl* xreal
			     (lowpass-attenuation-at@ (r:ii./ i0 s0))
			     (lowpass-attenuation-at@ (r:ii./ i1 s1)))))))


;; move to visualize.scm?

;; test random-offset

(define offset-max 15)

(define (view-offset n)
  (let* ((mult 15)
	 (s/2 (* mult offset-max))
	 (s (* s/2 2))
	 (m (Mr:zeros s s))
	 (gen (lambda ()
		(integer (+ s/2 (* mult (random-offset)))))))
    (for..< (i 0 n)
	    (let ((x (gen))
		  (y (gen)))
	      (.update! m x y inc)))
    (view m)))


(define (fast-real-Mr.fold m fn start)
  (letv ((s0 s1) (Mr.sizes m))
	(let ((res (f64vector start)))
	  (declare (not safe) (fixnum))
	  (for..< (i0 0 s0)
		  (for..< (i1 0 s1)
			  (let ()
			    (declare (not safe) (flonum))
			    (f64vector-set! res
					    0
					    (fn (Mr.ref@ m i0 i1)
						(f64vector-ref res 0))))))
	  (f64vector-ref res 0))))

(define. (Mr.energy m)
  ;; Mc.energy too?
  (letv ((s0 s1) (Mr.sizes m))
	(/ (fast-real-Mr.fold m
			      (lambda (v rest)
				(+ (sqrt (* v v)) rest))
			      0.)
	   ;; XX I thought I could move sqrt out? but how?
	   (* s0 s1))))


(define (random-m h w energy)
  (let* ((fft (lambda (m) (CTX (Mc:.fft m ctx))))
	 (ifft (lambda (m) (CTX (Mr:.ifft m w #f ctx)))))
    (let* ((m (ifft
	       (lowpass2!
		(fft
		 (Mr:generate (lambda (x y) (random-offset)) h w)))))
	   (e (.energy m)))
      (Mr..*! m (/ energy e)))))


(define-inline (lenient-M_-ref sizes ref m default)
  (letv ((s0 s1) (sizes m))
	(lambda (i0 i1)
	  (declare (fixnum))
	  ;; (sizes could be moved into the type as optimization...)
	  (if (and (>= i0 0) (< i0 s0)
		   (>= i1 0) (< i1 s1))
	      (ref m i0 i1)
	      default))))

;;(define .lenient-ref (lenient-M_-ref .sizes .ref))
;;(define Mr.lenient-ref (lenient-M_-ref Mr.sizes Mr.ref))

(define-inline (integer+rest x values)
  (let* ((f (floor x))
	 (r (- x f)))
    (values (inexact->exact f) r)))


;;XXXXlib
(define-macro* (import-inlined name)
  (let ((nameval (eval name)))
    (assert (procedure? nameval))
    `(define ,name ,(##decompile nameval))))
(import-inlined inc)

;; but, (not only, will it really be inlined, but also,) if you want
;; safety settings of surroundings...:
(define-inline (inc% i)
  (+ i 1))
;; or but then:
(define-inline (fx.inc@ i)
  (declare (not safe) (fixnum))
  (+ i 1))

;; read pixels from non-integer positions
(define (lenient-picture-ref m f0 f1 default)
  ;; (since float numbers, not using quotient+rest. well, yeh.anyway
  ;; not 'super sampled')
  (declare (flonum)
	   (not safe)
	   )

  (define ref (lenient-M_-ref Mr.sizes (lambda (m a b)
					 (Mr.ref@ m a b)) m default))
  
  ;; 1.  2.  3.

  (integer+rest
   f0
   (lambda (i0 r0)
     (integer+rest
      f1
      (lambda (i1 r1)
	(declare (flonum)
		 ;;(not safe)
		 )
	(let ((left (+ (* (ref i0 i1) (- 1. r0))
		       (* (ref (fx.inc@ i0) i1) r0)))
	      (right (+ (* (ref i0 (fx.inc@ i1)) (- 1. r0))
			(* (ref (fx.inc@ i0) (fx.inc@ i1)) r0))))
	  (+ (* left (- 1. r1))
	     (* right r1))))))))

(define (newspaper m energy)
  (letv ((s0 s1) (Mr.sizes m))
	(let ((m0 (random-m s0 s1 energy))
	      (m1 (random-m s0 s1 energy))
	      (res (@make-Mr s0 s1)))
	  ;;(step)
	  ;; (Mr.map+ m
	  ;; 	   (lambda (_v i0 i1)
	  ;; 	     (let ((b0 (Mr.ref@ m0 i0 i1))
	  ;; 		   (b1 (Mr.ref@ m1 i0 i1)))
	  ;; 	       (lenient-picture-ref m
	  ;; 				    (+ i0 b0)
	  ;; 				    (+ i1 b1)
	  ;; 				    1.))))
	  ;;hm(declare (not safe) (fixnum))
	  (for..< (i0 0 s0)
		  (for..< (i1 0 s1)
			  (let ((b0 (Mr.ref@ m0 i0 i1))
				(b1 (Mr.ref@ m1 i0 i1)))
			    (Mr.set!@
			     res i0 i1
			     (lenient-picture-ref m
						  (+ i0 b0)
						  (+ i1 b1)
						  1.)))))
	  res)))

(CONSTANTS)
