;;; Copyright 2016 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Very similar to Maybe: wrapper with two cases of which one holds a
;; value; but instead of carrying a value in the positive case, this
;; carries a value in the negative case.

;; It could easily be done by using Maybe as meaning maybe an error,
;; but to avoid the possible confusion, define a new type.

(require easy
	 more-oo
	 test)

;; how do Perl6 or so call them?
(class Status
       (subclass Success
		 (subclass Success/0
			   (struct constructor-name: _Success/0))
		 (subclass Success/1
			   (struct value)))
       (subclass Failure
		 ;; could call this .reason, but perhaps should stay
		 ;; compatible with Maybe ?
		 (struct value)))

;; optimization:
(def __Success/0 (_Success/0))
(def (Success/0)
     __Success/0)

(defmacro (Success . args)
  (if (null? args)
      `__Success/0 ;; `(Success/0) ?
      (if (null? (cdr args))
	  `(Success/1 ,(car args))
	  (source-error stx "too many arguments"))))


(TEST
 > (eq? (Success) (Success))
 #t
 > (map (lambda (v)
	  (map (C _ v) (list Status? Success? Failure?
			     (lambda (v)
			       (if (Failure? v)
				   (Failure.value v)
				   (if (Success/1? v)
				       (.value v)
				       'n))))))
	(list #f
	      (values)
	      (Success)
	      (Success 21)
	      (Failure 1)
	      (Failure #f)
	      (Failure (Success)) ;; hihi
	      (Failure (Failure 13))))
 ((#f #f #f n)
  (#f #f #f n)
  (#t #t #f n)
  (#t #t #f 21)
  (#t #f #t 1)
  (#t #f #t #f)
  (#t #f #t #(Success/0))
  (#t #f #t #(Failure 13)))
 > (Failure.value (.value (Failure (Failure 13))))
 13)



(def-inline (if-Status #(Status? v) success failure)
  (if (Success? v) ;; XXX Success/1 ? and pass value?
      (success)
      (failure (Failure.value v))))


(defmacro (Status:if t
		     then
		     #!optional
		     else)
  `(if-Status ,t
	      (lambda ()
		,then)
	      (lambda (it)
		,(or else
		     `(void)))))

(TEST
 > (Status:if (Success) 'ok 'fail)
 ok
 > (Status:if (Failure 'foo) 'ok 'fail)
 fail
 > (Status:if (Failure 'foo) 'ok)
 #!void)

(defmacro (Status:unless t
			 then)
  `(if-Status ,t
	      void
	      (lambda (it)
		,(or then `(void)))))

(TEST
 > (Status:unless (Success) it)
 #!void
 > (Status:unless (Failure 'foo) it)
 foo)


(def (Status/0 pred-failure)
     (lambda (v)
       (or (Success/0? v)
	   (and (Failure? v)
		(pred-failure (Failure.value v))))))

(def (Status/1 pred-success pred-failure)
     (lambda (v)
       (or (and (Success/1? v)
		(pred-success (Success/1.value v)))
	   (and (Failure? v)
		(pred-failure (Failure.value v))))))

(TEST
 > (def l
	(list (Success)
	      (Success 5)
	      (Success 'x)
	      10
	      (Failure 10)
	      (Failure 'y)))
 > (map (Status/0 integer?) l)
 (#t #f #f  #f #t #f)
 > (map (Status/1 symbol? integer?) l)
 (#f #f #t  #f #t #f))

