;;; Copyright 2010-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; no require form, included in cj-source.scm


(define (inc-function n)
  (declare (fixnum))
  (+ n 1))

(define (dec-function n)
  (declare (fixnum))
  (- n 1))


(define (parameter-inc! p)
  (let ((x (inc-function (p))))
    (p x)
    x))

(define (parameter-dec! p)
  (let ((x (dec-function (p))))
    (p x)
    x))

(define (parameter-add! p x)
  (let ((x (+ (p) x)))
    (p x)
    x))

(define (parameter-update! p fn)
  (let ((x (fn (p))))
    (p x)
    x))

(define (parameter-push! p v)
  (let ((x (cons v (p))))
    (p x)
    x))

;; this one deviates from the above in that its return value is used
;; for the "primary purpose":
(define (parameter-pop! p)
  (let ((l (p)))
    (p (cdr l))
    (car l)))


(define (list-join lis val #!optional (tail '()))
  ;; copy to avoid circular dependency
  (define (null-list? l)
    (cond ((pair? l) #f)
	  ((null? l) #t)
	  (else (error "null-list?: argument out of domain" l))))
  (define (fold-right kons knil lis1)
    (let recur ((lis lis1))		; Fast path
      (if (null-list? lis) knil
	  (let ((head (car lis)))
	    (kons head (recur (cdr lis)))))))
  ;;/copy
  (if (null? lis)
      tail
      (cons (car lis)
	    (fold-right (lambda (v l)
			  (cons val (cons v l)))
			tail
			(cdr lis)))))

(define (scm:object->string v)
  (parameterize ((current-readtable
		  (readtable-max-write-level-set
		   (readtable-max-write-length-set
		    (current-readtable)
		    12)
		   6)))
		(object->string v)))

(define (scm:objects->string objs
			     #!key
			     (prepend #f) ;; maybe type, *or* boolean
			     (separator " "))
  (apply string-append
	 (let ((m (list-join
		   (map scm:object->string
			objs)
		   separator)))
	   (if (and prepend (pair? objs))
	       (cons (if (string? prepend) prepend separator) m)
	       m))))


(define (identity x)
  x)
