;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.


;; Library for association lists of various key types

(define (symbol-equal? a b)
  (if (symbol? a)
      (if (symbol? b)
	  (eq? a b)
	  (error "not a symbol:" b))
      (error "not a symbol:" a)))

(define (true/1 v)
  #t)

(define any-type? true/1)

(define (cj-alist:error-not-found alis key)
  (error "key not found:" alis key))

(define (false/2 a b)
  #f)

(define (_-alist-ref key-type? equal? not-found)
  (lambda (alis key)
    (if (key-type? key)
	(let lp ((l alis))
	  (cond ((pair? l)
		 (let-pair
		  ((a r) l)
		  (if (equal? key (car a))
		      (cdr a)
		      (lp r))))
		((null? l)
		 (not-found alis key))
		(else
		 (error "improper list:" alis))))
	(error "wrong type of key:" key))))

(define symbol-alist-ref
  (_-alist-ref symbol? symbol-equal?
	       cj-alist:error-not-found))

(define number-alist-ref
  (_-alist-ref number? =
	       cj-alist:error-not-found))

(define eq-alist-ref
  (_-alist-ref any-type? eq?
	       cj-alist:error-not-found))

(define symbol-alist-maybe-ref
  (_-alist-ref symbol? symbol-equal?
	       false/2))

(define number-alist-maybe-ref
  (_-alist-ref number? =
	       false/2))

(define eq-alist-maybe-ref
  (_-alist-ref any-type? eq?
	       false/2))


(define (_-alist-replace key-type? equal?)
  (lambda (alis key+val)
    ;; die if not found.ok?
    (let ((key (car key+val)))
      (if (key-type? key)
	  (let rec ((alis alis))
	    (cond ((pair? alis)
		   (let* ((ali (car alis))
			  (a (car ali)))
		     (if (equal? a key)
			 (cons key+val
			       (cdr alis))
			 (cons ali
			       (rec (cdr alis))))))
		  ((null? alis)
		   ;; show key, show key and alis, show nothing?..
		   (error "key not found:" key))
		  (else
		   (error "improper alis ending in:" alis))))
	  (error "wrong type of key:" key)))))

(define symbol-alist-replace
  (_-alist-replace symbol? symbol-equal?))

(define number-alist-replace
  (_-alist-replace number? =))

(define eq-alist-replace
  (_-alist-replace any-type? eq?))

(TEST
 > (symbol-alist-replace '((b c) (d e) (a z) (x f)) '(a b c))
 ((b c) (d e) (a b c) (x f))
 > (symbol-alist-replace '((b c) (d e) (a z)) '(a b c))
 ((b c) (d e) (a b c))
 > (symbol-alist-replace '((a z) (d e)) '(a b c))
 ((a b c) (d e))
 > (symbol-alist-replace '((a z)) '(a b c))
 ((a b c))
 ;; > (symbol-alist-replace '((z a)) '(a b c))
 ;; *** ERROR IN rec, ... -- alist-replace: key not found: a
 ;; 1>
 )

(define (_-alist-add key-type? equal?)
  (lambda (alis key+val)
    (if (key-type? (car key+val))
	(cons key+val alis)
	(error "wrong type of key:"(car key+val)))))

(define symbol-alist-add
  (_-alist-add symbol? symbol-equal?))

(define number-alist-add
  (_-alist-add number? =))

(define eq-alist-add
  (_-alist-add any-type? eq?))

