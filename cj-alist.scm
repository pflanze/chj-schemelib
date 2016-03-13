;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require test)


;; Library for association lists of various key types

(define (symbol-equal? a b)
  (if (symbol? a)
      (if (symbol? b)
	  (eq? a b)
	  (error "not a symbol:" b))
      (error "not a symbol:" a)))

(define (keyword-equal? a b)
  (if (keyword? a)
      (if (keyword? b)
	  (eq? a b)
	  (error "not a keyword:" b))
      (error "not a keyword:" a)))

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

(define keyword-alist-ref
  (_-alist-ref keyword? keyword-equal?
	       cj-alist:error-not-found))

(define number-alist-ref
  (_-alist-ref number? =
	       cj-alist:error-not-found))

(define string-alist-ref
  (_-alist-ref string? string=?
	       cj-alist:error-not-found))

(define eq-alist-ref
  (_-alist-ref any-type? eq?
	       cj-alist:error-not-found))

(define symbol-alist-maybe-ref
  (_-alist-ref symbol? symbol-equal?
	       false/2))

(define keyword-alist-maybe-ref
  (_-alist-ref keyword? keyword-equal?
	       false/2))

(define number-alist-maybe-ref
  (_-alist-ref number? =
	       false/2))

(define string-alist-maybe-ref
  (_-alist-ref string? string=?
	       false/2))

(define eq-alist-maybe-ref
  (_-alist-ref any-type? eq?
	       false/2))


(define (_-alist-replace key-type? equal?
			 key-found/2 key-not-found/1)
  (lambda (alis key+val)
    ;; die if not found.ok?
    (let ((key (car key+val)))
      (if (key-type? key)
	  (let rec ((alis alis))
	    (cond ((pair? alis)
		   (let* ((ali (car alis))
			  (a (car ali)))
		     (if (equal? a key)
			 (key-found/2 key+val
				      (cdr alis))
			 (cons ali
			       (rec (cdr alis))))))
		  ((null? alis)
		   ;; show key, show key and alis, show nothing?..
		   (key-not-found/1 key))
		  (else
		   (error "improper alis ending in:" alis))))
	  (error "wrong type of key:" key)))))

(define _alist-replace-key-not-found (cut error "key not found:" <>))

(define symbol-alist-replace
  (_-alist-replace symbol? symbol-equal?
		   cons _alist-replace-key-not-found))

(define keyword-alist-replace
  (_-alist-replace keyword? keyword-equal?
		   cons _alist-replace-key-not-found))

(define number-alist-replace
  (_-alist-replace number? =
		   cons _alist-replace-key-not-found))

(define eq-alist-replace
  (_-alist-replace any-type? eq?
		   cons _alist-replace-key-not-found))

(define string-alist-replace
  (_-alist-replace string? string=?
		   cons _alist-replace-key-not-found))

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

;; return alis if it doesn't contain key
(define (string-alist-eliminate alis key)
  (continuation-capture
   (lambda (return)
     ((_-alist-replace string? string=?
		       (lambda (_key+val rest)
			 rest)
		       (lambda (_)
			 (continuation-return return alis)))
      alis (cons key #f)))))

(TEST
 > (define ali '(("a" 1) ("b" 2) ("c" 3)))
 > (string-alist-eliminate ali "b")
 (("a" 1) ("c" 3))
 > (string-alist-eliminate ali "c")
 (("a" 1) ("b" 2))
 > (string-alist-eliminate ali "d")
 (("a" 1) ("b" 2) ("c" 3))
 > (eq? # ali)
 #t
 > (string-alist-eliminate '() "d")
 ()
 )



(define (_-alist-add key-type? equal?)
  (lambda (alis key+val)
    (if (key-type? (car key+val))
	(cons key+val alis)
	(error "wrong type of key:"(car key+val)))))

(define symbol-alist-add
  (_-alist-add symbol? symbol-equal?))

(define keyword-alist-add
  (_-alist-add keyword? keyword-equal?))

(define number-alist-add
  (_-alist-add number? =))

(define eq-alist-add
  (_-alist-add any-type? eq?))

(TEST
 > (keyword-alist-add '() (cons foo: 1))
 ((foo: . 1))
 > (keyword-alist-add '((foo: . 2)) (cons foo: 1))
 ((foo: . 1) (foo: . 2))
 ;; hm, does *not* clean up, on purpose for cheaper sharing? But not
 ;; always what one wants There's of course:
 > (keyword-alist-replace '((foo: . 2)) (cons foo: 1))
 ((foo: . 1))
 ;; but:
 > (%try-error (keyword-alist-replace '((bar: . 2)) (cons foo: 1)))
 #(error "key not found:" foo:))


;; set entries by replacing if existing, adding otherwise
(define (_-alist-set key-type? equal?)
  (lambda (alis key+val)
    (let ((key (car key+val)))
      (if (key-type? key)
	  (let lp ((l alis))
	    (if (null? l)
		;; key not found, add entry
		(cons key+val alis)
		(let ((frame (car l)))
		  (if (equal? (car frame) key)
		      ;; replace, i.e. keep tail, replace current, add
		      ;; newer frames on top
		      (let ((tail (cons key+val (cdr l))))
			(let rec ((l2 alis))
			  (let ((frame2 (car l2)))
			    (if (eq? frame2 frame)
				;; arrived at same place again
				tail
				(cons frame2
				      (rec (cdr l2)))))))
		      (lp (cdr l))))))
	  (error "wrong type of key:" key)))))

(define symbol-alist-set
  (_-alist-set symbol? symbol-equal?))

(define keyword-alist-set
  (_-alist-set keyword? keyword-equal?))

(TEST
 > (keyword-alist-set '((a: . 0)) '(foo: . 1))
 ((foo: . 1) (a: . 0))
 > (keyword-alist-set '((a: . 1)
			(b: . 2)
			(foo: . 3)
			(bar: . 4))
		      '(foo: . 1))
 ((a: . 1)
  (b: . 2)
  (foo: . 1)
  (bar: . 4)))

;; XX todo: add extensive tests (qcheck).

