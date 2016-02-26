;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require syntax
	 srfi-11)


;; particular monad for use in transactions:

;; each monad (?ç) takes the commit (state), and passes on (returns)
;; the new commit

;; monads that return values also return the value

(define (transaction-multival:>> a b)
  (lambda (commit)
    (b (a commit))))

(define (transaction-multival:>>= a b)
  (lambda (commit)
    (letv ((commit* val) (a commit))
	  ((b val) commit*))))

(define (transaction-multival:return val)
  (lambda (commit)
    (values commit
	    val)))


;; turn a normal expression into one that is just run as a side-effect
;; of the monad's execution

(define-macro* (transaction-multival:inline! expr)
  `(lambda (commit)
     ,expr ;; could be done with thunks instead of a macro, of course..
     commit))


(TEST
 ;; example, list creation
 > (define (m:create val)
     (lambda (commit)
       (cons val commit)))
 > (define (m:nth n)
     (lambda (commit)
       (values commit
	       (list-ref commit n))))

 > (define k0
     (m:create "hallo"))
 > (k0 '())
 ("hallo")

 > (define k1
     (in-monad
      transaction-multival
      (>> (m:create "hallo")
	  (m:create "welt"))))
 > (k1 '())
 ("welt" "hallo")

 > (define msgs '())
 > (define (msg! . args)
     (set! msgs (cons (apply string-append args) msgs)))
 > (define (k2 whichcase)
     (in-monad
      transaction-multival
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
 > ((k2 0) '())
 ("welt" "hallo")
 > ((k2 1) '())
 ("welt" "hallo")
 > ((k2 2) '())
 ("welt" "hallo")
 > msgs
 ("got2: welt" "got1: welt" "got0: hallo")
 > (set! msgs '())

 > (define (k3)
     (in-monad
      transaction-multival
      (m:begin (m:create "hallo")
	    (m:create "welt")
	    (letm (v (m:nth 0))
		  (m:inline! (msg! "got I: " v)))
	    (m:create "andsoon")
	    (letm (v (m:nth 0))
		  (m:inline! (msg! "got II: " v))))))
 > ((k3) '())
 ("andsoon" "welt" "hallo")
 > msgs
 ("got II: andsoon" "got I: welt")
 > (set! msgs '())

 > (define (k4)
     (in-monad
      transaction-multival
      (m:begin (m:create "hallo")
	    (m:create "welt")
	    (m:let ((hal (m:begin
			  (m:create "neue")
			  (return "Hellou")))
		    (wel (m:nth 0)))
		   (m:inline! (msg! "k4: " hal wel))))))
 > ((k4) '())
 ("neue" "welt" "hallo")
 > msgs
 ("k4: Hellouneue")

 ;; monad rules:

 ;; (stupid tests, but a start)
 > (define (test . args)
     (define (run m)
       (m '(X)))
     (apply values-equal? (map run args)))

 ;; return a >>= k  ==  k a
 > (define a "A")
 > (define k (lambda (v)
	       (in-monad transaction-multival
			 (return (cons "k got: " v)))))
 > (in-monad transaction-multival
	     (test (>>= (return a) k)
		   (k a)))
 #t
 
 ;; m >>= return  ==  m
 ;; m must return a value, e.g.:
 > (define m (m:nth 0))
 > (in-monad transaction-multival
	     (test (>>= m return)
		   m))
 #t

 ;; m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h
 > (define h (lambda (v)
 	       (in-monad transaction-multival
 			 (return (cons "k got: " v)))))
 > (in-monad transaction-multival
	     (test (>>= m
			(lambda (x)
			  (>>= (k x)
			       h)))
		   (>>= (>>= m k)
			h)))
 #t
 )

