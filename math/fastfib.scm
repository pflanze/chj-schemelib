;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; According to
;; https://news.ycombinator.com/item?id=22946710
;; Fast Fibonacci numbers using Monoids

;; private static BigInteger Fibonacci(UInt32 n)
;; {
;;   if (n == 0)
;;   {
;;     return BigInteger.Zero;
;;   }
;;   else
;;   {
;;     var (a, b) = (BigInteger.Zero, BigInteger.One);
;;     var mask = 1u << (Int32)Math.Floor(Math.Log(n, 2));
;;     while ((mask >>= 1) != 0)
;;     {
;;         var s = a + b;
;;         var (a2, b2, s2) = (a * a, b * b, s * s);
;;         (a, b) = ((n & mask) == 0) ? (a2 + b2, s2 - a2) : (s2 - a2, s2 + b2);
;;     }
;;     return b;
;;   }
;; }

(require easy
         test)

(include "../cj-standarddeclares.scm")

(def (stupidfib n)
     (if (<= n 1)
         1
         (+ (stupidfib (- n 1))
            (stupidfib (- n 2)))))

(TEST
 > (map stupidfib (iota 16))
 (1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987))

(def slowfibs
     (delay (cons* 1 1 (stream-map + slowfibs (delay (.rest slowfibs))))))

(TEST
 > (=> (.take slowfibs 16) F)
 (1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987)
 > (time (.ref slowfibs 1000))
 70330367711422815821835254877183549770181269836358732742604905087154537118196933579742249494562611733487750449241765991088186363265450223647106012053374121273867339111198139373125598767690091902245245323403501)

(def (slowfib n)
     (let lp ((a 1)
              (b 1)
              (n (- n 3)))
       (let (c (+ a b))
         (if (negative? n)
             c
             (lp b c (dec n))))))

(TEST
 > (time (slowfib 1000))
 70330367711422815821835254877183549770181269836358732742604905087154537118196933579742249494562611733487750449241765991088186363265450223647106012053374121273867339111198139373125598767690091902245245323403501)



(def (logn x base) (/ (log x) (log base)))

(def (Fibonacci [fixnum-natural0? n])
     (if (zero? n)
         0 ;; ah
         (let lp ((a 0)
                  (b 1)
                  (mask (arithmetic-shift 1 (integer (logn n 2)))))
           (let (mask (arithmetic-shift mask -1))
             (if (zero? mask)
                 b
                 (let (s (+ a b))
                   (let ((a2 (* a a))
                         (b2 (* b b))
                         (s2 (* s s)))
                     (let (cont (lambda (a b)
                                  (lp a b mask)))
                       (if (zero? (bitwise-and n mask))
                           (cont (+ a2 b2) (- s2 a2))
                           (cont (- s2 a2) (+ s2 b2)))))))))))

(def Fibonacci* (comp Fibonacci inc))

(TEST
 > (map Fibonacci* (iota 16))
 (1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987))

(TEST
 > (time (Fibonacci* 1000))
 70330367711422815821835254877183549770181269836358732742604905087154537118196933579742249494562611733487750449241765991088186363265450223647106012053374121273867339111198139373125598767690091902245245323403501)

