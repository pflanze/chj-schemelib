;;; Copyright 2014-2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require test
	 srfi-1
	 (list-util improper-fold-right)
	 (char-util char-one-of?/)
	 cj-functional
	 C
	 (string-util-4 string-empty?
			string-every)
	 (improper-list improper-any))


(export box-of
	forced ;; rename to possibly-promise-of ?
	any? true/1
	false/2
	false? ;; == not
	true?
	true
	nonnegative-real?
	nonpositive-real?
	negative-real?
	inexact-real?
	exact-real?
	exact-integer?
	exact-number?
	pair-or-null?
	pair-with-car
	nonempty-string?
	improper*-map/tail ;; XX move
	improper*-map	   ;; dito
	string-of
	string-of-length
	improper-every	  ;; XX move
	improper-list-of  ;; hmm
	char-one-of	  ;; move to char lib?
	perhaps-source-of ;; XX rename to possibly-source-of ?
	source-of
	perhaps-source*-of ;; dito
	source*-of
	length-=
	length-<=
	length->=
	length-is ;; see also list-of/length  -- rename to list-of-length ?
	length=
	list-of-length
	lists?
	0..1? ;; see also rgb:0..1?
	in-signed-range?
	parameter?)


(define (box-of pred)
  (lambda (v)
    (and (box? v)
	 (pred (unbox v)))))

(define (forced pred)
  (lambda (v)
    (pred (force v))))

(define (true/1 v)
  #t)
(define (false/2 a b)
  #f)

(define any? true/1)

(define false? not) ;; so as to be able to use "false." as OO prefix

;; maybe also, since at it:
(define (true? x) (eq? x #t)) ;; dangerous to mistake?
(define (true x)
  ;; any kind of true; identity
  (not (not x)))


;; XX move positive-real? from cj-env.scm here.

(define nonnegative-real? (both real? (complement negative?)))

(define nonpositive-real? (both real? (complement positive?)))

(define negative-real? (both real? negative?))

(define inexact-real? (both real? inexact?))

(define exact-real? (both real? exact?))

(define exact-integer? (both integer? exact?))

(define exact-number? (both number? exact?))

(TEST
 > (exact-real? 3+2i)
 #f
 > (exact-number? 3+2i)
 #t
 > (exact-real? 0.0)
 #f
 > (exact-real? 1)
 #t
 > (exact-real? 1/2)
 #t
 > (exact-integer? 0.0)
 #f
 > (exact-integer? 1)
 #t
 > (exact-integer? 1/2)
 #f
 ;; TODO, sigh, exact versions of all the others... really?
 > (natural0? 1.)
 #t
 )


(define (pair-or-null? v)
  (or (pair? v)
      (null? v)))

(define (pair-with-car pred)
  (lambda (v)
    (and (pair? v)
	 (pred (car v)))))

;; btw should probably move predicates stuff from cj-functional here

(define nonempty-string?
  (both string?
	(complement string-empty?)))

;; improper->proper-map

(define (improper*-map/tail fn v tail)
  (improper-fold-right (lambda (a r)
			 (cons (fn a) r))
		       tail
		       v))

(define improper*-map (C improper*-map/tail _ _ '()))

(TEST
 > (improper*-map true? '("" . ""))
 (#f #f))


(define (string-of pred)
  (lambda (v)
    (and (string? v)
	 (string-every pred v))))

(TEST
 > (map (string-of char-alphanumeric?) '(foo "" " " "foo" "foo bar" "foo:" "Foo_"))
 (#f #t #f #t #f #f #t))


(define (string-of-length len)
  (if (natural0? len)
      (lambda (v)
	(and (string? v)
	     (= (string-length v) len)))
      (error "not a natural0:" len)))

(TEST
 > (map (string-of-length 3) '(foo "" "a" "ab" "abc" "abcd"))
 (#f  #f #f #f #t #f))


(define (improper-every pred v)
  (cond ((pair? v)
	 (and (pred (car v))
	      (improper-every pred (cdr v))))
	((null? v)
	 #t)
	(else
	 (pred v))))

(define (improper-list-of pred)
  (C improper-every pred _))

(TEST
 > (map (improper-list-of (string-of char-alphanumeric?))
	'("foo" ("a" "b") ("a" . "b") ("a" . b) ("a" ("b"))))
 (#t #t #t #f #f))


(define char-one-of char-one-of?/)


(define (perhaps-source-of pred)
  (lambda (v)
    (pred (source-code v))))

(define (source-of pred)
  (lambda (v)
    (and (source? v)
	 (pred (source-code v)))))

(define (perhaps-source*-of pred)
  (lambda (v)
    (pred (cj-desourcify v))))

(define (source*-of pred)
  (lambda (v)
    (and (source? v)
	 (pred (cj-desourcify v)))))


(define (length-= l len)
  (if (null? l)
      (zero? len)
      (if (zero? len)
	  #f
	  (length-= (cdr l) (dec len)))))

(TEST
 > (define (t-length= length*)
     (map (lambda (l)
	    (apply length* l))
	  '((() 0)
	    (() 1)
	    ((a) 1)
	    ((a) 0)
	    ((a b) 2)
	    ((a b) 3))))
 > (t-length= length-=)
 (#t #f #t #f #t #f))

(define (length= l1 l2)
  (if (null? l1)
      (null? l2)
      (if (null? l2)
	  #f
	  (length= (cdr l1) (cdr l2)))))

(TEST
 > (length= '() '())
 #t
 > (length= '(a) '())
 #f
 > (length= '(a) '(1))
 #t
 > (length= '(a) '(1 2))
 #f
 > (length= '(a b) '(1 2))
 #t
 > (%try (length= '(a . b) '(1 . 2)))
 (exception text: "(Argument 1) PAIR expected\n(cdr 'b)\n"))


(define (length-<= l len)
  (if (null? l)
      #t
      (if (zero? len)
	  #f
	  (length-<= (cdr l) (dec len)))))

(TEST
 > (t-length= length-<=)
 (#t #t #t #f #t #t))

(define (length->= l len)
  (if (null? l)
      (zero? len)
      (if (zero? len)
	  #t
	  (length->= (cdr l) (dec len)))))

(TEST
 > (t-length= length->=)
 (#t #f #t #t #t #f))


;; see also list-of/length
(define (length-is len)
  (lambda (l)
    (length-= l len)))


;; XX base on length-= for simplification?
(define (list-of-length n)
  (lambda (v)
    (let lp ((v v)
	     (len 0))
      (cond ((pair? v)
	     (if (< len n)
		 (lp (cdr v) (inc len))
		 #f))
	    ((null? v)
	     (= len n))
	    (else
	     ;; not a list
	     #f)))))

(TEST
 > (define vals '(() (a) (a b) (a b c) (a b . c) a))
 > (map (list-of-length 2) vals)
 (#f #f #t #f #f #f)
 > (map (list-of-length 0) vals)
 (#t #f #f #f #f #f)
 > (map (list-of-length 4) vals)
 (#f #f #f #f #f #f))


(define lists? (list-of list?))

(TEST
 > (lists? '())
 #t
 > (lists? '(() (1)))
 #t
 > (lists? '(() (1) 2))
 #f)


(define (0..1? v)
  (and (real? v)
       (<= 0 v)
       (<= v 1)))

;; also see rgb:0..1? which accepts 1 milli unit of change.



(define (in-signed-range? wordsize-bits v)
  (let ((half (expt 2 (dec wordsize-bits))))
    (and (<= (- half) v)
	 (< v half))))

(TEST
 > (define (test basenum v)
     (list (in-signed-range? 8 v)
	   (number->string (+ basenum v) 2)))
 > (test (expt 2 16) -1)
 (#t "1111111111111111")
 ;;   1234567812345678
 > (test (expt 2 16) -126)
 (#t "1111111110000010")
 > (test (expt 2 16) -127)
 (#t "1111111110000001")
 > (test (expt 2 16) -128)
 (#t "1111111110000000")
 > (test (expt 2 16) -129)
 (#f "1111111101111111")
 > (test (expt 2 15) 0)
 (#t "1000000000000000")
 > (test (expt 2 15) 127)
 (#t "1000000001111111")
 > (test (expt 2 15) 128)
 (#f "1000000010000000")
 ;;   1234567812345678
 )


(define parameter? ##parameter?)

