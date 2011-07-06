;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; check for equal? with cache

(define check-equal-f:cache-size 8)
(define check-equal-f:cache #f)
(define check-equal-f:cache-pointer #f);; 0..size-1; last written pos; newer up

(define (check-equal-f:cache-clear!)
  (set! check-equal-f:cache-pointer 0)
  (set! check-equal-f:cache
	(make-vector (* check-equal-f:cache-size 2) (gensym 'nothing))))

(check-equal-f:cache-clear!)

(define (check-equal-f_ possibly-shortcutting ok not-found)
  (lambda (a b)
    (define c check-equal-f:cache)
    (define p check-equal-f:cache-pointer)
    (define siz check-equal-f:cache-size)
    (possibly-shortcutting
     a b
     (lambda ()
       (let lp ((i p))
	 (if (and (eq? (vector-ref c i) a)
		  (eq? (vector-ref c (+ i siz)) b))
	     (ok a b)
	     ;; search down, to the older
	     (let ((i (let ((i (dec i)))
			(if (negative? i) (+ i siz) i))))
	       (if (= i p)
		   (not-found a b c p siz)
		   (lp i)))))))))

(define (check-equal-f:equal)
  (lambda (a b c p siz)
    (if (equal? a b)
	(let ((newp (let ((newp (inc p)))
		      (if (>= newp siz) 0 newp))))
	  (vector-set! c newp a)
	  (vector-set! c (+ newp siz) b)
	  (set! check-equal-f:cache-pointer newp)
	  a)
	(error "not equal?:" a b))))

(define check-equal-f
  (check-equal-f_
   (lambda (a b cont)
     (if (eq? a b)
	 a
	 (cont)))
   (lambda (a b)
     a)
   (check-equal-f:equal)))

(define check-equal-or-single
  ;; if one of the two values is false, return the other one.
  (check-equal-f_
   (lambda (a b cont)
     (if a
	 (if b
	     (if (eq? a b)
		 a
		 (cont))
	     a)
	 (if b
	     b
	     (error "got two false values"))))
   (lambda (a b)
     a)
   (check-equal-f:equal)))


(TEST
 > (check-equal-f:cache-clear!)
 > (check-equal-f 'a 'a)
 a
 > check-equal-f:cache-pointer
 0
 > (define aa1 (string-append "a" "a"))
 > (define aa2 (string-append "a" "a"))
 > (check-equal-f aa1 aa2)
 "aa"
 > check-equal-f:cache-pointer
 1
 > (check-equal-f aa1 aa2)
 "aa"
 > check-equal-f:cache-pointer
 1
 > (with-exception-catcher error-exception-message
			   (thunk (check-equal-f "a" "b")))
 "not equal?:"
 > check-equal-f:cache-pointer
 1
 > (define aa3 (string-append "a" "a"))
 > (define aa4 (string-append "a" "a"))
 > (check-equal-f aa3 aa4)
 "aa"
 > check-equal-f:cache-pointer
 2
 > (eq? (check-equal-f aa1 aa2) aa1)
 #t
 > check-equal-f:cache-pointer
 2

 ;; and the -or-false:
 > (with-exception-catcher error-exception-message (thunk (check-equal-or-single #f #f)))
 "got two false values"
 > (check-equal-or-single #f aa2)
 "aa"
 > (check-equal-or-single aa2 #f)
 "aa"
 > check-equal-f:cache-pointer
 2
 > (check-equal-or-single aa2 aa3)
 "aa"
 > check-equal-f:cache-pointer
 3

 ;; wrap-around:
 ;; almost clear it:
 > (let lp ((i 7))
     (if (zero? i)
	 check-equal-f:cache-pointer
	 (begin
	   (check-equal-or-single (string-append "x" "x") (string-append "x" "x"))
	   (lp (dec i)))))
 2
 ;;> (check-equal-f aa1 aa2) no, that's the oldest, that one's gone
 > (check-equal-or-single aa2 aa3)
 "aa"
 > check-equal-f:cache-pointer
 2
 > (check-equal-f aa3 aa4)
 "aa"
 > check-equal-f:cache-pointer
 3
 > (check-equal-or-single aa2 aa3)
 "aa"
 > check-equal-f:cache-pointer
 4
 > (check-equal-or-single 'aa2 'aa2)
 aa2
 > check-equal-f:cache-pointer
 4
 )
