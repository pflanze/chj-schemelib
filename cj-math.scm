(require test
	 (test-logic ∀))

;; can this calculation be optimized?
(define (quotient+modulo x y)
  (values (quotient x y)
	  (modulo x y)))

;; > (quotient+modulo 14 12)
;; 1
;; 2
;; Uh?:
;; > (quotient+modulo -14 12)
;; -1
;; 10


(define (/= a b)
  (not (= a b)))


(define (integer x)
  (inexact->exact (floor x)))

(define (integer-ceiling x)
  (inexact->exact (ceiling x)))

(TEST
 > (map integer-ceiling '(-2 -1.9 -1.1 -1 -0.9 -0.1 -0 0 0.1 0.9 1 1.1 1.9 2))
 (-2 -1 -1 -1 0 0 0 0 1 1 1 2 2 2))

(define (exact x)
  (let ((x* (inexact->exact x)))
    (assert (= x* x))
    x*))

(define (square x)
  (* x x))

;;(define integer:double (cut arithmetic-shift <> 1))
(define integer:half (cut arithmetic-shift <> -1))


(define (integer-average a b)
     (arithmetic-shift (+ a b) -1))

(TEST
 > (integer-average 1 1)
 1
 > (integer-average 1 2)
 1
 > (integer-average 1 3)
 2
 > (integer-average 0 3)
 1
 > (integer-average 3 0)
 1
 > (integer-average 3 -4)
 -1)

(define integer:average integer-average)

(define pi (* (asin 1) 2))


(define-macro* (let-complex bind . body)
  (match* bind
	  ((vars expr)
	   (match* vars
		   ((vr vi)
		    (with-gensym
		     V
		     `(let ((,V ,expr))
			(let ((,vr (real-part ,V))
			      (,vi (imag-part ,V)))
			  ,@body))))))))

(define (conj z)
  (let-complex ((r i) z)
	       (make-rectangular r (- i))))

(TEST
 > (conj (sqrt -2))
 -1.4142135623730951i
 > (sqrt -2+0.5i)
 .17543205637629397+1.425053124063947i
 > (conj (sqrt -2+0.5i))
 .17543205637629397-1.425053124063947i
 > (conj 2)
 2
 )


;; Same as quotient, but rounds the result up to the next bigger
;; integer instead of down. (Formerly called quotient-roundup and
;; quotient/ceiling.)
(define (quotient-ceiling a b)
  (let* ((q (quotient a b)))
    (if (= (* q b) a)
        q
        (+ q 1))))

(TEST
 > (quotient-ceiling 32 2)
 16
 > (quotient 33 2)
 16
 > (quotient-ceiling 33 2)
 17
 > (quotient-ceiling 32 -2)
 -16
 > (quotient 33 -2)
 -16
 > (quotient-ceiling 33 -2)
 -15)



(define natural0.bitsize integer-length)

;; I had this as (integer-ceiling (/ (log (+ n 1)) (log 2))) but that
;; actually fell down at around 64 bit numbers since it would
;; underflow the double range...

;; (define (natural0.bitsize* n)
;;   (/ (log (+ n 1)) (log 2)))
;; > (natural0.bitsize* (expt 2 20))
;; 20.000001375860553
;; > (natural0.bitsize* (dec* (expt 2 20)))
;; 20.
;; > (natural0.bitsize* (expt 2 100))
;; 100.
;; > (natural0.bitsize* (dec* (expt 2 100)))
;; 100.

;; and integer-ceiling would round up 20.000001375860553 but not 100.


(TEST
 > (map natural0.bitsize (iota 5))
 (0 1 2 2 3)
 > (natural0.bitsize 15)
 4
 > (natural0.bitsize 16)
 5
 > (∀ (map (C expt 2 _) (iota 10))
      (lambda (bits)
	(define ex (expt 2 bits))
	(and (= (natural0.bitsize (dec* ex)) bits)
	     (= (natural0.bitsize ex) (inc bits)))))
 ()
 > (map (comp (C map natural0.bitsize _)
	      (dup dec* id inc*)
	      (C arithmetic-shift 1 _))
	(append (iota 15) (iota 6 62)))
 ((0 1 2)
  (1 2 2)
  (2 3 3)
  (3 4 4)
  (4 5 5)
  (5 6 6)
  (6 7 7)
  (7 8 8)
  (8 9 9)
  (9 10 10)
  (10 11 11)
  (11 12 12)
  (12 13 13)
  (13 14 14)
  (14 15 15)
  (62 63 63)
  (63 64 64)
  (64 65 65)
  (65 66 66)
  (66 67 67)
  (67 68 68)))

