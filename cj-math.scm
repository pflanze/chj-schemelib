;;; Copyright 2013-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require srfi-1
         list-util
         slib-sort
         test
	 (test-logic ∀ qcheck*)
         (srfi-11 values->vector letv))

(export quotient+modulo
        quotient+remainder
        /=
        integer
        integer-ceiling
        exact
        square

        average
        list-average
        list-median
        list-variance-from
        list-standard-deviation-from
        list-variance
        list-standard-deviation
        
        integer:half
        integer-average integer:average
        pi
        (macro let-complex)
        conj
        quotient-ceiling
        natural0.bitsize ;; just an alias for integer-length
        integer->alphabetic26-string)


(include "cj-standarddeclares.scm")



;; can this calculation be optimized?
(define (quotient+modulo x y)
  (values (quotient x y)
	  (modulo x y)))

(TEST
 > (values->vector (quotient+modulo 14 12))
 [1 2]
 > (values->vector (quotient+modulo -14 12))
 [-1 10])


(define (quotient+remainder.1 x y)
  (values (quotient x y)
	  (remainder x y)))

(define (quotient+remainder x y)
  (let ((q (quotient x y)))
    (values q
            (- x (* q y)))))

(TEST
 > (values->vector (quotient+remainder 14 12))
 [1 2]
 > (values->vector (quotient+remainder -14 12))
 [-1 -2]
 > (define (wrap f)
     ;; oh, need to change to vector here, too, as Gambit's equal?
     ;; silently returns #f for values tuples! :( XX patch it! ?
     (lambda-pair ((a b))
             (with-exception-catcher
              (lambda (e)
                (if (divide-by-zero-exception? e)
                    'divide-by-zero
                    (raise e)))
              (lambda ()
                (values->vector (f a b))))))
 > (qcheck* (cartesian-product-2 (iota 10 -5) (iota 10 -5))
            (wrap quotient+remainder.1)
            (wrap quotient+remainder))
 ())


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



;; Or "mean"?
(define (average x y)
  ;; For overflowing numbers like machine integers would have to
  ;; calculate (+ (/ x 2) (/ y 2)) instead.
  (/ (+ x y) 2))

(define (list-average l)
  ;; XX improve
  (/ (list-sum l)
     (length l)))

(define (list-median l)
  ;; XX optim: make vector, sort in-place, pick middle values. Don't
  ;; even have that sort.
  (let* ((ls (sort l <))
         (len (length l))
         (len/2 (integer:half (dec len)))
         (ls* (drop ls len/2)))
    (let-pair ((a ls**) ls*)
              (if (odd? len)
                  a
                  (average a (first ls**))))))

(TEST
 > (list-average '(1))
 1
 > (list-average '(1 2))
 3/2
 > (list-average '(1 2 1))
 4/3

 > (list-median '(1))
 1
 > (list-median '(1 2))
 3/2
 > (list-median '(1 2 1))
 1
 > (list-median '(10 -100 1 2 1 4))
 3/2)


(define (list-variance-from avg whole? l)
  (/ (fold (lambda (x tot)
             (+ (square (- x avg)) tot))
           0
           l)
     (let ((len (length l)))
       (If whole? len
           ;; https://en.wikipedia.org/wiki/Bessel%27s_correction
           (dec len)))))

(define (list-standard-deviation-from avg whole? l)
  (sqrt (list-variance-from avg whole? l)))

(define (list-variance l #!key whole?)
  (list-variance-from (list-average l) whole? l))

(define (list-standard-deviation l #!key whole?)
  (list-standard-deviation-from (list-average l) whole? l))


(TEST
 ;; https://en.wikipedia.org/wiki/Standard_deviation
 > (define vss
     '((female 727.7 
               1086.5
               1091.0
               1361.3
               1490.5
               1956.1)
       (male 525.8 	
             605.7 	
             843.3 	
             1195.5 	
             1945.6 	
             2135.6 	
             2308.7 		
             2950.0)))
 > (define vs (cdr (assoc 'female vss)))
 > (* (list-variance vs) 5)
 886047.0883333331
 > (list-standard-deviation vs)
 420.96248961952256
 > (list-standard-deviation (cdr (assoc 'male vss)))
 894.372699158466
 
 > (define grades '(2 4 4 4 5 5 7 9))
 > (list-average grades)
 5
 > (list-variance grades)
 32/7
 > (list-variance grades whole?: #t)
 4
 > (list-standard-deviation grades whole?: #t)
 2)


;; Use "namespace" approach to indicate desired operation kind

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

;; Again 'Use "namespace" approach to indicate desired operation kind'
;; (then the above shouldn't be offered though?)

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



(define (integer->alphabetic26-string c) ;; -> string?
  (let lp ((c c)
           (res '()))
    (letv ((r n) (quotient+modulo c 26))
          (let ((res (cons (integer->char
                            (+ n (insert-result-of (char->integer #\A))))
                           res)))
            (if (zero? r)
                (list->string res)
                (lp r res))))))

(TEST
 > (integer->alphabetic26-string 0)
 "A"
 > (integer->alphabetic26-string 25)
 "Z"
 > (integer->alphabetic26-string 26)
 "BA"
 ;; ^ ever interesting. Had this already some time some where. It's
 ;; consistent with how we handle numbers so don't change it, okay?
 > (integer->alphabetic26-string (square 26))
 "BAA")
