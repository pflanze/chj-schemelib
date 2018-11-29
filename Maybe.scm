;;; Copyright 2016-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Also see Result.scm

(require easy
	 jclass
	 test
	 (if-let if-let*-expand
		 if-let-expand))

(export (jclass Maybe
		(jclass Nothing)
		(jclass Just))
	(macro Maybe:if)
	(macro Maybe:cond)
	(macro Maybe:if-let*)
	(macro Maybe:if-let)
	Maybe)



(jclass Maybe

	(jclass ((Nothing _Nothing))
		(def-method- (maybe-value s)
		  #f)
		(def-method- (show s)
		  `(Nothing)))

	(jclass (Just value)
		(def-method- maybe-value Just.value))

	;; name if-present instead?
	(def-method- (if-Just v then els)
	  (if (Just? v)
	      (then (@Just.value v))
	      (els))))


;; optimization:
(def _Nothing_ (_Nothing))
(def (Nothing)
     _Nothing_)

(TEST
 > (eq? (Nothing) (Nothing))
 #t
 > (map (lambda (v)
	  (map (C _ v) (list Maybe? Nothing? Just?
			     (lambda (v)
			       (with-exception-catcher
				error-exception-message
				(& (.if-Just v
					     identity
					     (& 'n))))))))
	(list #f
	      (values)
	      (Nothing)
	      (Just 1)
	      (Just #f)
	      (Just (Nothing))
	      (Just (Just 13))))
 ((#f #f #f "no method found for generic .if-Just for value:")
  (#f #f #f "no method found for generic .if-Just for value:")
  (#t #t #f n)
  (#t #f #t 1)
  (#t #f #t #f)
  (#t #f #t #((Nothing)))
  (#t #f #t #((Just) 13)))
 > (Just.value (.value (Just (Just 13))))
 13)


(TEST
 > (%try-error (.if-Just 'foo (lambda_ 1) (lambda_ 2)))
 #(error "no method found for generic .if-Just for value:" foo))


(def (Maybe:error v)
     (error "not a Maybe:" v))

;; XX rename to if-Just (for consistency with Result.scm)? (or if-present ?)
(defmacro (Maybe:if t
		    then
		    #!optional
		    else)
  `(let ((it-Maybe ,t))
     (cond ((Just? it-Maybe)
	    (let ((it (@Just.value it-Maybe)))
	      ,then))
	   ((Nothing? it-Maybe)
	    ,(or else `(void)))
	   (else
	    (Maybe:error it-Maybe)))))

(TEST
 > (Maybe:if (Just 1) it 'no)
 1
 > (Maybe:if (Nothing) it 'no)
 no
 > (%try-error (Maybe:if 'foo 1 2))
 #(error "not a Maybe:" foo))



;; once again (where did I have something like this?):
;; This never returns `(begin), which might be what you want ('usually
;; always'?)
(def (rest->begin rest)
     (trif-one rest
	       identity
	       (C cons `begin _)
	       (& `(void))))

(TEST
 > (rest->begin '())
 (void)
 > (rest->begin '(a))
 a
 > (rest->begin '(a b))
 (begin a b))


;; Stupid, only allowing one test, why. Should extend, similar to
;; Maybe:or, ah wait, Result:or. Anyway, similar to cond.
(defmacro (Maybe:cond t+then #!optional else)
  (let ((else* (if else
		   (mcase else
			  (`(else . `rest)
			   (rest->begin rest)))
		   `(void))))
    (mcase t+then
	   (`(`t => `then)
	    (with-gensym V
			 `(let ((,V ,t))
			    (cond ((Just? ,V)
				   (,then (@Just.value ,V)))
				  ((Nothing? ,V)
				   ,else*)
				  (else
				   (Maybe:error ,V))))))
	   (`(`t . `rest)
	    ;; actually introduces |it| like, well, Maybe:if
	    `(Maybe:if ,t
		       ,(rest->begin rest)
		       ,else*)))))

(TEST
 > (Maybe:cond ((Nothing) => 'no))
 #!void
 > (Maybe:cond ((Nothing) => 'no) (else 'fail))
 fail
 > (Maybe:cond ((Just 2) it) (else 'fail))
 2
 > (Maybe:cond ((Just 3) => identity) (else 'fail))
 3
 )

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
 > (def counter 0)
 > (def (g x)
	(Maybe:cond ((psqrt x) => inc)
		    (else (inc! counter)
			  'n)))
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
 > counter
 1
 > (%try-error (Maybe:cond ((sqrt 4) => inc)))
 #(error "not a Maybe:" 2))


(def (Maybe pred)
     (lambda (v)
       (or (Nothing? v)
	   (and (Just? v)
		(pred (@Just.value v))))))

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
 (exception text: "not a Maybe: 2\n")
 > (%try (Maybe:if-let ((a #f)) 3 4))
 (exception text: "not a Maybe: #f\n")
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
 (exception text: "not a Maybe: 1\n")
 >  (Maybe:if-let* ((z8wm5y6dp9 (Just 1))
		    (b (Just (inc z8wm5y6dp9))))
		   (list z8wm5y6dp9 b)
		   5)
 (1 2)
 )

