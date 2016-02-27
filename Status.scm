;; Very similar to Maybe: wrapper with two cases of which one holds a
;; value; but instead of carrying a value in the positive case, this
;; carries a value in the negative case.

;; It could easily be done by using Maybe as meaning maybe an error,
;; but to avoid the possible confusion, define a new type.

;; Also see Result which carries a value in either case. Hm.

(require easy
	 more-oo
	 test)

;; how do Perl6 or so call them?
(class Status
       (subclass Success
		 (struct constructor-name: _Success))
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
	  (map (C _ v) (list Status? Success? Failure?
			     (lambda (v)
			       (if (Failure? v)
				   (Failure.value v)
				   'n)))))
	(list #f
	      (values)
	      (Success)
	      (Failure 1)
	      (Failure #f)
	      (Failure (Success)) ;; hihi
	      (Failure (Failure 13))))
 ((#f #f #f n)
  (#f #f #f n)
  (#t #t #f n)
  (#t #f #t 1)
  (#t #f #t #f)
  (#t #f #t #(Success))
  (#t #f #t #(Failure 13)))
 > (Failure.value (.value (Failure (Failure 13))))
 13)



(def (if-Status #(Status? v) then else)
     (if (Failure? v)
	 (then (Failure.value v))
	 (else)))


(defmacro (Status:if t
		    then
		    #!optional
		    else)
  `(if-Status ,t ,then (lambda () ,(or else `(void)))))

(defmacro (Status:cond t+then #!optional else)
  (mcase t+then
	 (`(`t => `then)
	  `(if-Status ,t
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
	    (Failure (sqrt x))
	    (Success)))
 > (def (f x)
	(Status:if (psqrt x)
		  inc
		  'n))
 > (def (f* x)
	(Status:if (psqrt x)
		  inc))
 > (def (g x)
	(Status:cond ((psqrt x) => inc)
		    (else 'n)))
 > (def (g* x)
	(Status:cond ((psqrt x) => inc)))
 > (map (lambda (x)
	  (list (f x)
		(g x)
		(f* x)
		(g* x)))
	(list 4 9 -4))
 ((3 3 3 3)
  (4 4 4 4)
  (n n #!void #!void))
 > (%try-error (Status:cond ((sqrt 4) => inc)))
 #(error "v does not match Status?:" 2))


(def (Status pred)
     (lambda (v)
       (or (Success? v)
	   (and (Failure? v)
		(pred (Failure.value v))))))

(TEST
 > (def Status-integer? (Status integer?))
 > (map Status-integer? (list (Success) 10 (Failure 10)))
 (#t #f #t))

