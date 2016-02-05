(require easy
	 more-oo
	 test)

(class maybe
       (subclass nothing
		 (struct constructor-name: _nothing))

       (subclass just
		 (struct value)))

;; optimization:
(def __nothing (_nothing))
(def (nothing)
     __nothing)

(TEST
 > (eq? (nothing) (nothing))
 #t
 > (map (lambda (v)
	  (map (C _ v) (list maybe? nothing? just?
			     (lambda (v)
			       (if (just? v)
				   (just.value v)
				   'n)))))
	(list #f
	      (values)
	      (nothing)
	      (just 1)
	      (just #f)
	      (just (nothing))
	      (just (just 13))))
 ((#f #f #f n)
  (#f #f #f n)
  (#t #t #f n)
  (#t #f #t 1)
  (#t #f #t #f)
  (#t #f #t #(nothing))
  (#t #f #t #(just 13)))
 > (just.value (.value (just (just 13))))
 13)



(def (if-maybe #(maybe? v) then else)
     (if (just? v)
	 (then (just.value v))
	 (else)))


(defmacro (maybe:if t
		    then
		    #!optional
		    else)
  `(if-maybe ,t ,then (lambda () ,(or else `(void)))))

(defmacro (maybe:cond t+then #!optional else)
  (mcase t+then
	 (`(`t => `then)
	  `(if-maybe ,t
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
	    (just (sqrt x))
	    (nothing)))
 > (def (f x)
	(maybe:if (psqrt x)
		  inc
		  'n))
 > (def (f* x)
	(maybe:if (psqrt x)
		  inc))
 > (def (g x)
	(maybe:cond ((psqrt x) => inc)
		    (else 'n)))
 > (def (g* x)
	(maybe:cond ((psqrt x) => inc)))
 > (map (lambda (x)
	  (list (f x)
		(g x)
		(f* x)
		(g* x)))
	(list 4 9 -4))
 ((3 3 3 3)
  (4 4 4 4)
  (n n #!void #!void))
 > (%try-error (maybe:cond ((sqrt 4) => inc)))
 #(error "v does not match maybe?:" 2))

