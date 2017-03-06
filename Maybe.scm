;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Also see Status.scm

(require easy
	 more-oo
	 test
	 (if-let if-let*-expand
		 if-let-expand))

(export (class Maybe
	       (class Nothing)
	       (class Just))
	(macro Maybe:if)
	(macro Maybe:cond)
	(macro Maybe:if-let*)
	(macro Maybe:if-let)
	Maybe)



(class Maybe
       (subclass Nothing
		 (struct constructor-name: _Nothing)
		 (method (maybe-value s)
			 #f))

       (subclass Just
		 (struct value)
		 (method maybe-value Just.value)))

;; optimization:
(def __Nothing (_Nothing))
(def (Nothing)
     __Nothing)

(TEST
 > (eq? (Nothing) (Nothing))
 #t
 > (map (lambda (v)
	  (map (C _ v) (list Maybe? Nothing? Just?
			     (lambda (v)
			       (if (Just? v)
				   (Just.value v)
				   'n)))))
	(list #f
	      (values)
	      (Nothing)
	      (Just 1)
	      (Just #f)
	      (Just (Nothing))
	      (Just (Just 13))))
 ((#f #f #f n)
  (#f #f #f n)
  (#t #t #f n)
  (#t #f #t 1)
  (#t #f #t #f)
  (#t #f #t #((Nothing)))
  (#t #f #t #((Just) 13)))
 > (Just.value (.value (Just (Just 13))))
 13)


(def-inline (if-Maybe #(Maybe? v) then else)
  (if (Just? v)
      (then (Just.value v))
      (else)))

(TEST
 > (%try-error (if-Maybe 'foo 1 2))
 #(error "v does not match Maybe?:" foo))


(defmacro (Maybe:if t
		    then
		    #!optional
		    else)
  `(if-Maybe ,t
	     (lambda (it)
	       ,then)
	     (lambda ()
	       ,(or else `(void)))))

(defmacro (Maybe:cond t+then #!optional else)
  (mcase t+then
	 (`(`t => `then)
	  `(if-Maybe ,t
		     ,then
		     (lambda ()
		       ,(if else
			    (mcase else
				   (`(else `else)
				    else))
			    `(void)))))))

(TEST
 > (def (psqrt x)
	(if (positive? x)
	    (Just (sqrt x))
	    (Nothing)))
 > (def (f x)
	(Maybe:if (psqrt x)
		  (inc it)
		  'n))
 > (def (f* x)
	(Maybe:if (psqrt x)
		  (inc it)))
 > (def (g x)
	(Maybe:cond ((psqrt x) => inc)
		    (else 'n)))
 > (def (g* x)
	(Maybe:cond ((psqrt x) => inc)))
 > (map (lambda (x)
	  (list (f x)
		(g x)
		(f* x)
		(g* x)))
	(list 4 9 -4))
 ((3 3 3 3)
  (4 4 4 4)
  (n n #!void #!void))
 > (%try-error (Maybe:cond ((sqrt 4) => inc)))
 #(error "v does not match Maybe?:" 2))


(def (Maybe pred)
     (lambda (v)
       (or (Nothing? v)
	   (and (Just? v)
		(pred (Just.value v))))))

(TEST
 > (def Maybe-integer? (Maybe integer?))
 > (map Maybe-integer? (list (Nothing) 10 (Just 10)))
 (#t #f #t))


(defmacro (Maybe:if-let* assignments yes #!optional no)
  ;; return (Nothing) in "void" case? Doesn't make sense over #f. Just void?
  (if-let*-expand `Maybe:cond assignments yes (or no `(void))))

(defmacro (Maybe:if-let assignments yes #!optional no)
  (if-let-expand `Maybe:cond assignments yes (or no `(void))))

(TEST
 > (%try (Maybe:if-let ((a 2)) 3))
 (exception text: "v does not match Maybe?: 2\n")
 > (%try (Maybe:if-let ((a #f)) 3 4))
 (exception text: "v does not match Maybe?: #f\n")
 > (Maybe:if-let ((a (Just 2))) a)
 2
 > (Maybe:if-let ((a (Just 2))) a 4)
 2
 > (Maybe:if-let ((a (Nothing))) a 4)
 4
 > (Maybe:if-let ((a (Nothing))) a)
 #!void
 > (Maybe:if-let ((a (Just 2))
		  (b (Just 3)))
		 (list a b)
		 4)
 (2 3)
 > (Maybe:if-let ((a (Just 2))
		  (b (Nothing)))
		 (list a b)
		 5)
 5
 > (Maybe:if-let ((a (Nothing))
		  (b (Just 3)))
		 (list a b)
		 5)
 5
 > (%try (Maybe:if-let ((z8wm5y6dp9 (Just 1))
			(b z8wm5y6dp9))
		       (list a b)
		       5))
 (exception text: "Unbound variable: z8wm5y6dp9\n")
 > (%try (Maybe:if-let* ((z8wm5y6dp9 (Just 1))
			 (b z8wm5y6dp9))
			(list a b)
			5))
 (exception text: "v does not match Maybe?: 1\n")
 >  (Maybe:if-let* ((z8wm5y6dp9 (Just 1))
		    (b (Just (inc z8wm5y6dp9))))
		   (list z8wm5y6dp9 b)
		   5)
 (1 2)
 )

