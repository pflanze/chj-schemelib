

(def. (exact.remainder x)
  (remainder (numerator x)
	     (denominator x)))

(TEST
 > (.remainder 8/7)
 1
 > (.remainder 8/9)
 8
 > (.remainder 10/9)
 1
 )

;; floor is there already
;; > (floor 6/7)
;; 0
;; > (floor 8/7)
;; 1


(def. (integer.binary x)
  (number->string x 2))

