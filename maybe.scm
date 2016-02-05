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
 > (maybe? (nothing))
 #t
 > (maybe? (just 1))
 #t
 > (maybe? (just #f))
 #t
 > (just? (just #f))
 #t
 > (just? (nothing))
 #f
 > (just.value (just 13))
 13
 > (just.value (just (just 13)))
 #(just 13)
 )

