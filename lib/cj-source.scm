;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(define (source? o)
  (##source? o))

(define (check type? typename)
  (let ((msg (string-append "not a "typename" object:")))
    (lambda (proc)
      (lambda (o)
	(if (type? o)
	    (proc o)
	    (error msg o))))))

(define source-check (check source? "source"))

(define (source-code x)
  (if (##source? x)
      (##source-code x)
      (begin
	;;(warn "not source code:" x)
	x)))

(define (mk-source-code/? type?)
  (lambda (v)
    ;; need a copy of improper-map (~bootstrapping issue):
    (define (improper-map fn l #!optional (tail '()))
      (let rec ((l l))
	(cond ((null? l)
	       tail)
	      ((pair? l)
	       (cons (fn (car l))
		     (rec (cdr l))))
	      (else
	       (fn l)))))
    ;; /copy
    (improper-map (lambda (v)
		    (let ((c (source-code v)))
		      (if (type? c)
			  c
			  v)))
		  (source-code v))))

(define source-code/clean-keywords
  (mk-source-code/? keyword?))

(define (meta-object? v)
  (or (eq? v #!optional)
      (eq? v #!rest)
      (eq? v #!key)))

(define source-code/clean-meta-objects
  (mk-source-code/? meta-object?))


(define source-location (source-check ##source-locat))

(define (location? o)
  ;; well.
  (and (vector? o)
       (= (vector-length o) 2)))

(define location-check (check location? "location"))

(define location-container (location-check ##locat-container))

(define (container->path container)
  (if ##container->path-hook
      (or (##container->path-hook container)
          container)
    container))


;; he does it yet more nested (not sure it's warranted here):
(define location-position (location-check ##locat-position))

(define (position? o)
  ;; hmm
  (##fixnum? o))

(define position-check (check position? "position"))

(define position-line
  (position-check (lambda (pos)
		    (+ 1 (bitwise-and pos 65535)))))
(define position-column
  (position-check (lambda (pos)
		    (+ 1 (quotient pos 65536)))))


(define (make-position line column)
  (let ((l (- line 1))
	(c (- column 1)))
    (if (<= 0 l 65535)
	(if (<= 0 c)
	    (let ((r (bitwise-ior l
				  (arithmetic-shift c 16))))
	      (if (##fixnum? r)
		  r
		  (error "column out of range:" column)))
	    (error "column out of range:" column))
	(error "line out of range:" line))))

(define (make-location container position)
  ((position-check
    (lambda (position)
      (##make-locat container position)))
   position))

(define (make-source code locat)
  (let ((cont
	 (lambda (locat)
	   (##make-source code locat))))
    (if locat
	((location-check
	  cont)
	 locat)
	;; have to accept #f as locat, just as ##make-source
	(cont locat))))

(define (sourcify x src)
  ((source-check
    (lambda (src)
      (##sourcify x src)))
   src))

(define (cj-sourcify-deep s master)
  ;; COPIES (bootstrapping issue):
  (define (improper-map fn l #!optional (tail '()))
    (let rec ((l l))
      (cond ((null? l)
	     tail)
	    ((pair? l)
	     (cons (fn (car l))
		   (rec (cdr l))))
	    (else
	     (fn l)))))
  (define (vector-map fn vec)
    ;; COPY
    (define (inc n)
      (+ n 1))
    ;; /COPY
    ;;(list->vector (map fn (vector->list vec)))
    (let* ((len (vector-length vec))
	   (res (make-vector len)))
      (let lp ((i 0))
	(if (= i len)
	    res
	    (begin
	      (vector-set! res i (fn (vector-ref vec i)))
	      (lp (inc i)))))))
  ;; /COPIES
  (let ((master-loc (source-location master)))
    (let rec ((s s))
      ((lambda (process)
	 (if (source? s)
	     (make-source (process (source-code s))
			  (source-location s))
	     (make-source (process s)
			  master-loc)))
       ;; "where process ="
       (lambda (c)
	 (cond ((pair? c)
		(improper-map rec c))
	       ((vector? c)
		;; quoted vectors (syntax for constants)
		(vector-map rec c))
	       ((box? c)
		(box (rec (unbox c))))
	       ((or (##structure? c)
		    ;; some more?
		    )
		;; v- again, how to give location info/exceptions generally?
		(error "cj-sourcify-deep: type of this object unexpected:" c))
	       (else
		;; also e.g. (u8vector? c):
		;; doesn't contain anything, so:
		c)))))))

(define (cj-possibly-sourcify-deep s master)
  (if (source? master)
      (cj-sourcify-deep s master)
      s))

(define (cj-desourcify x)
  (let ((x (if (##source? x) (##source-code x) x)))
    (cond ((pair? x)
	   (cons (cj-desourcify (car x))
		 (cj-desourcify (cdr x))))
	  ((vector? x)
	   (vector-map cj-desourcify x))
	  ;; XXX boxes? and more?
	  (else
	   x))))
