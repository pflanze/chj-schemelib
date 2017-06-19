;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Very similar to Maybe: wrapper with two cases of which one holds a
;; value; but instead of carrying a value in the positive case, this
;; carries a value in the negative case.

;; It could easily be done by using Maybe as meaning maybe an error,
;; but to avoid the possible confusion, define a new type.

;; This also actually has 3 cases now, Success without result, and
;; Result with a result (still representing Success*).

(require easy
	 jclass
	 (dot-oo void/1)
	 test
	 Maybe)

(export Status?
	Success*?
	Success? Success
	Result? Result Result.value
	Failure? Failure Failure.value
	.value ;; XX oh, that one I usually forget. Finally need class
	       ;; export form or so. But, anyway, these methods are
	       ;; not local(izable that way) (right?)
	if-Success
	if-Result
	if-Success* ;; inline
	Status:if ;; macro
	Status:unless ;; macro
	Status:and ;; macro
	Status:or ;; macro
	Status/Success-of
	Status/Result-of
	Result-of
	Failure-of
	
	#!optional
	_Success)

;; how do Perl6 or so call them?
(jinterface Status
	    (jinterface Success*
			(jclass ((Success _Success)))
			(jclass (Result value)))

	    ;; could call .value .reason, but perhaps should stay
	    ;; compatible with Maybe ?
	    (jclass (Failure value)))

;; optimization:
(def __Success (_Success))
(def (Success)
     __Success)


(TEST
 > (eq? (Success) (Success))
 #t
 > (map (lambda (v)
	  (map (C _ v) (list Status? Success*? Success? Failure?
			     (lambda (v)
			       (if (Failure? v)
				   (Failure.value v)
				   (if (Result? v)
				       (Result.value v)
				       'n))))))
	(list #f
	      (values)
	      (Success)
	      (Result 21)
	      (Failure 1)
	      (Failure #f)
	      (Failure (Success)) ;; hihi
	      (Failure (Failure 13))))
 ((#f #f #f #f n)
  (#f #f #f #f n)
  (#t #t #t #f n)
  (#t #t #f #f 21)
  (#t #f #f #t 1)
  (#t #f #f #t #f)
  (#t #f #f #t #((Success)))
  (#t #f #f #t #((Failure) 13)))
 > (Failure.value (.value (Failure (Failure 13))))
 13)



(def (if-Success v yes no)
     (cond ((Success? v)
	    (yes))
	   ((Failure? v)
	    (no (Failure.value v)))
	   (else
	    (error "not a Success or Failure:" v))))

(def (if-Result v yes no)
     (cond ((Result? v)
	    (yes (Result.value v)))
	   ((Failure? v)
	    (no (Failure.value v)))
	   (else
	    (error "not a Result or Failure:" v))))

;; hmm, now depending on Maybe, good or bad idea?
(def-inline (if-Success* v yes no)
  (cond ((Success? v)
	 (yes (Nothing)))
	((Result? v)
	 (yes (Just (Result.value v))))
	((Failure? v)
	 (no (Failure.value v)))
	(else
	 (error "not a Success, Result or Failure:" v))))

(defmacro (Status:if t
		     then
		     #!optional
		     else)
  `(if-Success* ,t
		(lambda (it)
		  ,then)
		(lambda (it)
		  ,(or else
		       `(void)))))

(TEST
 > (Status:if (Success) 'ok 'fail)
 ok
 > (Status:if (Success) it 'fail)
 #((Nothing))
 > (Status:if (Result 12) 'ok 'fail)
 ok
 > (Status:if (Result 12) it 'fail)
 #((Just) 12)
 > (Status:if (Failure 'foo) 'ok 'fail)
 fail
 > (Status:if (Failure 'foo) 'ok it)
 foo
 > (Status:if (Failure 'foo) 'ok)
 #!void)

(defmacro (Status:unless t
			 then)
  `(if-Success* ,t
		void/1
		(lambda (it)
		  ,(or then `(void)))))

(TEST
 > (Status:unless (Success) it)
 #!void
 > (Status:unless (Failure 'foo) it)
 foo)


(defmacro (Status:and . clauses)
  (if (one? clauses)
      (first clauses)
      (let-pair ((a r) (reverse clauses))
		(fold (lambda (clause next)
			(with-gensym
			 V
			 `(let ((,V ,clause))
			    (if (Success*? ,V)
				,next
				,V))))
		      a
		      r))))

(TEST
 ;; > (Status:and)
 ;; (Failure "empty Status:and statement")
 ;; or better simply don't allow syntactically?
 > (Status:and (Failure "foo"))
 #((Failure) "foo")
 > (Status:and (Failure "foo") (Failure "bar"))
 #((Failure) "foo")
 > (Status:and (Success) (Failure "bar"))
 #((Failure) "bar")
 > (Status:and (Result "foo") (Failure "bar"))
 #((Failure) "bar")
 > (Status:and (Result "foo") (Success) (Failure "bar"))
 #((Failure) "bar")
 > (Status:and (Result "foo") (Success) (Result "bar"))
 #((Result) "bar")
 > (Status:and (Result "foo") (Success))
 #((Success)))

(defmacro (Status:or . clauses)
  (if (one? clauses)
      (first clauses)
      (let-pair ((a r) (reverse clauses))
		(fold (lambda (clause next)
			(with-gensym
			 V
			 `(let ((,V ,clause))
			    (if (Success*? ,V)
				,V
				,next))))
		      a
		      r))))

(TEST
 ;; > (Status:or)
 ;; (Failure "empty Status:or statement") or don't allow
 > (Status:or (Failure "foo"))
 #((Failure) "foo")
 > (Status:or (Failure "foo") (Failure "bar"))
 #((Failure) "bar")
 > (Status:or (Success) (Failure "bar"))
 #((Success))
 > (Status:or (Result "foo") (Failure "bar"))
 #((Result) "foo")
 > (Status:or (Failure "bar") (Result "foo"))
 #((Result) "foo")
 > (Status:or (Success) (Result "foo") (Failure "bar"))
 #((Success))
 > (Status:or (Failure "foo") (Failure "bar") (Success))
 #((Success)))


;; Predicates

(def (Status/Success-of pred-failure)
     (lambda (v)
       (or (Success? v)
	   (and (Failure? v)
		(pred-failure (Failure.value v))))))

(def (Status/Result-of pred-result pred-failure)
     (lambda (v)
       (or (and (Result? v)
		(pred-result (Result.value v)))
	   (and (Failure? v)
		(pred-failure (Failure.value v))))))

(def (Result-of pred)
     (lambda (v)
       (and (Result? v)
	    (pred (Result.value v)))))

(def (Failure-of pred)
     (lambda (v)
       (and (Failure? v)
	    (pred (Failure.value v)))))


(TEST
 > (def l
	(list (Success)
	      (Result 5)
	      (Result 'x)
	      10
	      (Failure 10)
	      (Failure 'y)))
 > (map (Status/Success-of integer?) l)
 (#t #f #f  #f #t #f)
 > (map (Status/Result-of symbol? integer?) l)
 (#f #f #t  #f #t #f)
 > (map (Result-of integer?) l)
 (#f #t #f #f #f #f)
 > (map (Failure-of symbol?) l)
 (#f #f #f #f #f #t))

