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

