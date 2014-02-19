
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

