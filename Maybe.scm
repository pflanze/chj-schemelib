;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Also see Status.scm

(require easy
	 more-oo
	 test)

(class Maybe
       (subclass Nothing
		 (struct constructor-name: _Nothing))

       (subclass Just
		 (struct value)))

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

