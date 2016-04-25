;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

(require easy
	 (list-util let-pair)
	 (cj-functional flip)
	 (cj-functional-2 chain) ;; just for fun, in test
	 test)

;; Using the names "first" and "rest" as chosen in functional-perl
;; now, instead of "head" and "tail" like Haskell. Good or bad idea?
;; (See overview of other languages in functional-perl docs.)

(class typed-list

       (subclass typed-list-pair
		 (struct #(procedure? pred)
			 #(natural? length)
			 first
			 rest)

		 (method (list l)
			 (cons (.first l)
			       (.list (.rest l))))

		 (method (filter l f)
			 (let-typed-list-pair
			  ((pred _ v r) l)
			  (let ((r* (.filter r f)))
			    (if (f v)
				;; tail sharing optimization
				(if (eq? r* r)
				    l
				    ;; omit type check since already
				    ;; proven right
				    (typed-list-pair pred
						     (inc (.length r*))
						     v
						     r*))
				r*))))

		 (method (the l)
			 (let-typed-list-pair
			  ((_ len v _) l)
			  (if (= len 1)
			      v
			      (error "more than one element")))))

       (subclass typed-list-null
		 (struct #(procedure? pred))

		 (method (length l)
			 0)

		 (method (list l)
			 '())

		 (method (filter l f)
			 l)

		 (method (the l)
			 (error "fewer than one element")))

       (method (cons rst fst)
	       (let ((pred (.pred rst)))
		 (if (pred fst)
		     (typed-list-pair pred
				      (inc (.length rst))
				      fst
				      rst)
		     (error "typed-list: value does not meed predicate:"
			    fst)))))


(def typed-list-cons (flip typed-list.cons))

(def (list.typed-list pred vals)
     (if (null? vals)
	 (typed-list-null pred)
	 (let-pair ((v vals*) vals)
		   (typed-list-cons v
				    (list.typed-list pred vals*)))))

(def (typed-list pred . vals)
     (list.typed-list pred vals))


(TEST
 > (.length (typed-list number? 1 3 4))
 3
 > (.length (typed-list number?))
 0
 > (.list (typed-list number? 1 3 4))
 (1 3 4)
 > (.list (typed-list number? ))
 ()
 > (.list (.cons (.cons (typed-list number?) 10) 11))
 (11 10)
 > (%try-error (.list (.cons (.cons (typed-list number?) "10") 11)))
 #(error "typed-list: value does not meed predicate:" "10")
 > (%try-error (.list (.cons (.cons (typed-list number?) 10) "11")))
 #(error "typed-list: value does not meed predicate:" "11")
 > (chain (typed-list number?) (.cons 10) (.cons 11) (.list))
 (11 10))

(TEST
 > (.list (.filter (typed-list number? 1 3 4 5 0) even?))
 (4 0)
 > (.length (.filter (typed-list number? 1 3 4 5 0) even?))
 2
 > (def l (typed-list number? 1 3 4 5 0))
 > (eq? (.filter l natural0?) l)
 #t)

(TEST
 > (.the (typed-list number? 19))
 19
 > (%try-error (.the (typed-list number? )))
 #(error "fewer than one element")
 > (%try-error (.the (typed-list number? 19 20)))
 #(error "more than one element"))
