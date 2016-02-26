;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (monad.syntax))


;; particular monad for use in transactions:

;; each monad (?ç) takes the commit (state) and a continuation, and
;; passes the new commit and a new continuation to the latter.

;; monads that return values first pass the value to the continuation
;; -- ç ehr, b ist eben kein monad? sondern returns einen ok?. -- ah
;; aber still true, passen den value zu ihrer eigenen continuation.

(define (transaction-cps:>> a b)
  (lambda (commit cont)
    (a commit
       (lambda (commit)
	 (b commit
	    cont)))))

(define (transaction-cps:>>= a b)
  (lambda (commit cont)
    (a commit
       (lambda (commit val)
	 ((b val)
	  commit
	  cont)))))

(define (transaction-cps:return val)
  (lambda (commit cont)
    (cont commit
	  val)))


;; turn a normal expression into one that is just run as a side-effect
;; of the monad's execution

(define-macro* (transaction-cps:inline! expr)
  `(lambda (commit cont)
     ,expr ;; could be done with thunks instead of a macro, of course..
     (cont commit)))


(TEST
 ;; example, list creation
 > (define (m:create val)
     (lambda (commit cont)
       (cont (cons val commit))))
 > (define (m:nth n)
     (lambda (commit cont)
       (cont commit
	     (list-ref commit n))))

 > (define k0
     (m:create "hallo"))
 > (k0 '() identity)
 ("hallo")

 > (define k1
     (in-monad
      transaction-cps
      (>> (m:create "hallo")
	  (m:create "welt"))))
 > (k1 '() identity)
 ("welt" "hallo")

 > (define msgs '())
 > (define (msg! . args)
     (set! msgs (cons (apply string-append args) msgs)))
 > (define (k2 whichcase)
     (in-monad
      transaction-cps
      (>> (m:create "hallo")
	  (>> (m:create "welt")
	      (case whichcase
		((0)
		 (>>= (m:nth 1)
		      (lambda (x)
			(m:inline! (msg! "got0: " x)))))
		((1)
		 (letm (v (m:nth 0))
		       (m:inline! (msg! "got1: " v))))
	      
		((2)
		 (m:let ((v (m:nth 0)))
			(m:inline! (msg! "got2: " v)))))))))
 > ((k2 0) '() identity)
 ("welt" "hallo")
 > ((k2 1) '() identity)
 ("welt" "hallo")
 > ((k2 2) '() identity)
 ("welt" "hallo")
 > msgs
 ("got2: welt" "got1: welt" "got0: hallo")
 > (set! msgs '())

 > (define (k3)
     (in-monad
      transaction-cps
      (m:begin (m:create "hallo")
	    (m:create "welt")
	    (letm (v (m:nth 0))
		  (m:inline! (msg! "got I: " v)))
	    (m:create "andsoon")
	    (letm (v (m:nth 0))
		  (m:inline! (msg! "got II: " v))))))
 > ((k3) '() identity)
 ("andsoon" "welt" "hallo")
 > msgs
 ("got II: andsoon" "got I: welt")
 > (set! msgs '())

 > (define (k4)
     (in-monad
      transaction-cps
      (m:begin (m:create "hallo")
	    (m:create "welt")
	    (m:let ((hal (m:begin
			  (m:create "neue")
			  (return "Hellou")))
		    (wel (m:nth 0)))
		   (m:inline! (msg! "k4: " hal wel))))))
 > ((k4) '() identity)
 ("neue" "welt" "hallo")
 > msgs
 ("k4: Hellouneue")

 ;; monad rules:

 ;; (stupid tests, but a start)
 > (define (test . args)
     (define (run m)
       (m '(X) vector))
     (apply equal? (map run args)))

 ;; return a >>= k  ==  k a
 > (define a "A")
 > (define k (lambda (v)
	       (in-monad transaction-cps
			 (return (cons "k got: " v)))))
 > (in-monad transaction-cps
	     (test (>>= (return a) k)
		   (k a)))
 #t
 
 ;; m >>= return  ==  m
 ;; m must return a value, e.g.:
 > (define m (m:nth 0))
 > (in-monad transaction-cps
	     (test (>>= m return)
		   m))
 #t

 ;; m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h
 > (define h (lambda (v)
 	       (in-monad transaction-cps
 			 (return (cons "k got: " v)))))
 > (in-monad transaction-cps
	     (test (>>= m
			(lambda (x)
			  (>>= (k x)
			       h)))
		   (>>= (>>= m k)
			h)))
 #t
 )

