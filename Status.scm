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

;; This also actually has 3 cases now, Success without result, and
;; Result with a result (still representing Success*).

(require easy
	 more-oo
	 test)

;; how do Perl6 or so call them?
(class Status
       (subclass Success*
		 (subclass Success
			   (struct constructor-name: _Success))
		 (subclass Result
			   (struct value)))
       (subclass Failure
		 ;; could call this .reason, but perhaps should stay
		 ;; compatible with Maybe ?
		 (struct value)))

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
  (#t #f #f #t #(Success))
  (#t #f #f #t #(Failure 13)))
 > (Failure.value (.value (Failure (Failure 13))))
 13)



(def-inline (if-Status #(Status? v) success failure)
  (if (Success? v) ;; XXX Result ? and pass value?
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


(def (Status/Success pred-failure)
     (lambda (v)
       (or (Success? v)
	   (and (Failure? v)
		(pred-failure (Failure.value v))))))

(def (Status/Result pred-result pred-failure)
     (lambda (v)
       (or (and (Result? v)
		(pred-result (Result.value v)))
	   (and (Failure? v)
		(pred-failure (Failure.value v))))))

(TEST
 > (def l
	(list (Success)
	      (Result 5)
	      (Result 'x)
	      10
	      (Failure 10)
	      (Failure 'y)))
 > (map (Status/Success integer?) l)
 (#t #f #f  #f #t #f)
 > (map (Status/Result symbol? integer?) l)
 (#f #f #t  #f #t #f))

