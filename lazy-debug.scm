;;; Copyright 2010, 2011 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require lazy
	 debuggable-promise
	 (vector-util vector-map)
	 (srfi-11 values-map values->list))

(export F
	F1
	S
	#!optional
	alloc-optim-cons)


(possibly-use-debuggable-promise)

(define (alloc-optim-cons old a r)
  (if (and (eq? (car old) a)
	   (eq? (cdr old) r))
      old
      (cons a r)))

(TEST
 > (let ((old (cons 1 2))) (eq? (alloc-optim-cons old 1 2) old))
 #t
 > (let ((old (cons 1 2))) (eq? (alloc-optim-cons old 2 1) old))
 #f)


;; and after allocating the pad already, but 'could' still be beneficial:
(define (alloc-optim-vectorlikes length ref)
  (lambda (old new)
    (let ((l0 (length old))
	  (l1 (length new)))
      (if (and (= l0 l1)
	       (let lp ((i 0))
		 (if (< i l0)
		     (and (eq? (ref old i)
			       (ref new i))
			  (lp (fx+ i 1)))
		     #t)))
	  old
	  new))))

(define alloc-optim-vector (alloc-optim-vectorlikes vector-length vector-ref))
(define alloc-optim-values (alloc-optim-vectorlikes ##vector-length ##vector-ref))
;; well could have used the latter for each case..


(define (F/ promise-evaluated? force)
  (named F (lambda (s)
	     (let ((process (lambda (s)
			      (cond ((pair? s)
				     (alloc-optim-cons s
						       (F (car s))
						       (F (cdr s))))
				    ((vector? s)
				     (alloc-optim-vector s (vector-map F s)))
				    ((values? s)
				     (alloc-optim-values s (values-map F s)))
				    (else
				     s)))))
	       (if (promise? s)
		   (if (promise-evaluated? s)
		       (process (force s))
		       s)
		   (process s))))))

;; make copy forcing everything
(define F (F/ (lambda (v) #t)
	      force))

;; only copy what was already evaluated
(define S (F/ promise-evaluated?
	      evaluated-promise-value))


;; make copy forcing everything, but show <P> wherever there was a
;; promise (and one level at that for each). XX force1 is not
;; consistently implemented now.
(define (F1 s)
  (let F ((s s))
    (cond ((promise? s)
	   (vector '<P>
		   (let ((s (force1 s)))
		     (F s))))
	  ((pair? s)
	   (cons (F (car s))
		 (F (cdr s))))
	  (else
	   s))))


(TEST
 > (F '(1 2))
 (1 2)
 > (F (delay 1))
 1
 > (promise? (S (delay 1)))
 #t
 > (define s (delay (cons 1 (delay (cons 2 (delay (cons 3 '())))))))
 > (promise? (S s))
 #t
 > (car (force s))
 1
 > (promise? (S s))
 #f
 > (promise? s)
 #t
 > (promise? (cdr (S s)))
 #t
 > (promise? (cdr (F s)))
 #f
 > (promise? (cdr (S s)))
 #f
 > (S s)
 (1 2 3)

 ;; optim
 > (eq? (S s) (S s))
 #f ;; that's not what the optim does
 > (let ((t (cons -1 s))) (eq? (S t) (S t)))
 #f
 > (define s (delay (cons 1 (delay (cons 2 (delay (cons 3 '())))))))
 > (let ((t (cons -1 s))) (eq? (S t) (S t)))
 #t

 ;; values:
 > (define s (delay (cons (delay (values 'a 1))
			  (delay (cons (values 'b 2)
				       (delay (cons (values 'c 3)
						    (delay '()))))))))
 > (define s* (F s))
 > (length s*)
 3
 > (values->list (car s*))
 (a 1)
 ;; and their optim:
 > (define s (delay (cons 1 (delay (cons 2 (delay (cons 3 '())))))))
 > (let ((t (cons -1 (values 2 s)))) (eq? (S t) (S t)))
 #t
 )


