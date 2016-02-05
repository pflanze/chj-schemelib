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

