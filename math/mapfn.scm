;;; Copyright 2014-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         math/interpolate
         vector-binsearch
	 test
         test-logic)

(export mapfn
        (method number-vector.mapfn
                iseq-of-number.mapfn)
	#!optional
        number-vector?
        iseq-of-number?)

(include "../cj-standarddeclares.scm")


(def (cond-ordered-assoc x l < before between after)
     (if (< x (caar l))
	 (before (car l))
	 (let lp ((l (cdr l))
		  (prev-a (car l)))
	   (if (null? l)
	       (after prev-a)
	       (let-pair ((a l*) l)
			 (if (< x (car a))
			     (between prev-a a)
			     (lp l* a)))))))

(defstruct before value)
(defstruct on-or-after value)
(defstruct between fst snd)

(def (ordered-assoc x l #!optional (< <))
     (cond-ordered-assoc x l < before between on-or-after))

(TEST
 > (ordered-assoc 0 '((1 a) (2 b) (3 c)))
 [(before) (1 a)]
 > (ordered-assoc 1 '((1 a) (2 b) (3 c)))
 [(between) (1 a) (2 b)]
 > (ordered-assoc 1.1 '((1 a) (2 b) (3 c)))
 [(between) (1 a) (2 b)]
 > (ordered-assoc 2 '((1 a) (2 b) (3 c)))
 [(between) (2 b) (3 c)]
 > (ordered-assoc 2.1 '((1 a) (2 b) (3 c)))
 [(between) (2 b) (3 c)]
 > (ordered-assoc 3 '((1 a) (2 b) (3 c)))
 [(on-or-after) (3 c)])


(def ((mapfn/sorted-alist l) [real? x])
     (cond-ordered-assoc
      x l <
      (lambda-pair ((x1 y1))
                   (error "out of range, value too small:" x x1))
      (C interpolate _ _ x)
      (lambda-pair ((x1 y1))
                   (if (= x x1)
                       y1
                       (error "out of range, value too large:" x x1)))))



(def ((mapfn/sorted-vectorpair keys vals) [real? x])
     (let (r (vector-binsearch keys x real-cmp #t))
       (cond ((pair? r)
              (interpolate* (vector-ref keys (car r)) (vector-ref vals (car r))
                            (vector-ref keys (cdr r)) (vector-ref vals (cdr r))
                            x))
             ((fixnum-natural0? r)
              (vector-ref vals r))
             (else
              (if (< x (vector-ref keys 0))
                  (error "out of range, value too small:"
                         x (vector-ref keys 0))
                  (error "out of range, value too large:"
                         x (vector-ref* keys -1)))))))

(def (mapfn [(list-of (pair-of real? number?)) alis]
            #!optional (algo 'sorted-vectorpair))
     (let (l (sort alis (on car <)))
       (xcase algo
              ((sorted-alist)
               (mapfn/sorted-alist l))
              ((sorted-vectorpair)
               (mapfn/sorted-vectorpair (=>> l (map car) list->vector)
                                        (=>> l (map cdr) list->vector))))))


(TEST
 ;; imaginary vals work
 > ((mapfn (list (cons 1 -2i) (cons 3 2))) 2)
 1-i

 > (def m (mapfn '((1 . 100) (2 . 200) (0 . 10) (-1 . 5) (3 . 310))
                 ;;'sorted-alist
                 ))
 > (m 0)
 10
 > (m 1)
 100
 > (m 3)
 310
 > (%try-error (m 3.1))
 [error "out of range, value too large:" 3.1 3]
 > (%try-error (m -2))
 [error "out of range, value too small:" -2 -1]
 > (m 0.5)
 55.
 > (m 1.5)
 150.)

(TEST
 > (def l '((1950 . 4510.) (1955 . 4630.) (1960 . 4810.) (1965 . 4780.)
            (1970 . 4780.) (1975 . 4770.) (1980 . 4810.) (1985 . 4890.)
            (1990 . 4910.) (1995 . 4840.) (2000 . 4540.)))
 > (def f0 (mapfn l 'sorted-alist))
 > (def f1 (mapfn l 'sorted-vectorpair))
 > (def (catching f) (lambda (x) (%try-error (f x))))
 > (qcheck* (make-list! 100 (& (+ 1940. (* (random-real) 70.))))
            (catching f0)
            (catching f1))
 ())


(def number-vector?
     (either homogenous-vector?
             (vector-of number?)))

(def. (number-vector.mapfn v)
  (let* ((len-1 (dec (-> positive? (.length v))))
         (real-in-range (both real?
                              (C <= 0 _ len-1)))
         (ref (CAN. .ref v)))
    (lambda ([real-in-range x])
      (if (= x len-1)
          (ref v len-1)
          (let* ((x1 (integer x))
                 (y1 (ref v x1))
                 (x2 (inc x1))
                 (y2 (ref v x2)))
            (interpolate* x1 y1 x2 y2 x))))))

(def iseq-of-number?
     (iseq-of number?))

(def. iseq-of-number.mapfn
  (=>* .vector number-vector.mapfn))


(TEST
 > (def f (.mapfn '(10)))
 > (f 0)
 10
 > (%try (f 1))
 (exception text: "x does not match real-in-range: 1\n")
 > (%try (f 0.1))
 (exception text: "x does not match real-in-range: .1\n")

 > (def f (.mapfn '(10 11)))
 > (f 0)
 10
 > (f 1)
 11
 > (f 1/2)
 21/2

 > (def f (.mapfn '(10 14 -2)))
 > (f 1)
 14
 > (f 3/2)
 6)

