(require test)

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

