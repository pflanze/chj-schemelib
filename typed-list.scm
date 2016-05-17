;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

(require easy
	 (cj-gambit-sys procedure-name maybe-decompile)
	 (list-util let-pair)
	 (cj-functional flip)
	 (cj-functional-2 chain) ;; just for fun, in test
	 (cj-match mcase) ;; part of easy?
	 (cj-symbol with-gensym) ;; part of easy?
	 test)

(export typed-list? ;; (class typed-list)
	(class typed-list-pair)
	(class typed-list-cons)
	typed-list-cons
	typed-list-of
	list->typed-list
	typed-list
	(macro typed-list:let-pair))


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

		 (method (reverse-list l #!optional (tail '()))
			 (.reverse-list (.rest l)
					(cons (.first l) tail)))

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

		 (method (reverse-list l #!optional (tail '()))
			 tail)

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
			    fst
			    (or (procedure-name pred)
				(maybe-decompile pred)))))))



(def typed-list-cons (flip typed-list.cons))

(def (typed-list-of pred)
     (lambda (v)
       (and (typed-list? v)
	    ;; XX: how to compare type predicates? This is
	    ;; pessimistic.
	    (eq? pred (.pred v)))))

;; don't name list.typed-list, since it can't be dot-oo (with this
;; argument order, and wouldn't make sense (efficiently, and hence at
;; all) otherwise)
(def (list->typed-list pred vals)
     (if (null? vals)
	 (typed-list-null pred)
	 (let-pair ((v vals*) vals)
		   (typed-list-cons v
				    (list->typed-list pred vals*)))))

(def (typed-list pred . vals)
     (list->typed-list pred vals))

(defmacro (typed-list:let-pair bind . body)
  (mcase bind
	 (`(`vars `expr)
	  (mcase vars
		 (`(`a `r)
		  (with-gensym
		   V
		   `(let ((,V ,expr))
		      (let* ((,a (typed-list-pair.first ,V))
			     (,r (@typed-list-pair.rest ,V)))
			,@body))))))))


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
 > (.reverse-list (typed-list number? 1 3 4))
 (4 3 1)
 > (.reverse-list (typed-list number? ))
 ()
 > (%try-error (.list (.cons (.cons (typed-list number?) "10") 11)))
 #(error "typed-list: value does not meed predicate:" "10" number?)
 > (%try-error (.list (.cons (.cons (typed-list number?) 10) "11")))
 #(error "typed-list: value does not meed predicate:" "11" number?)
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

(TEST
 > ((typed-list-of number?) (typed-list number?))
 #t
 ;; currently doesn't allow subtyping...:
 > ((typed-list-of number?) (typed-list integer?))
 #f
 ;; also currently doesn't allow for function redefinitions, bah:
 > ((typed-list-of number?) (typed-list (lambda (v) (number? v))))
 #f)

(TEST
 > (typed-list:let-pair ((a r) (typed-list number? 3 4)) (list a (.list r)))
 (3 (4))
 > (%try-error (typed-list:let-pair ((a r) (cons 3 4)) a))
 #(error "expecting a typed-list-pair, got:" (3 . 4))
 ;; > (%try-error (typed-list:let-pair ((a r) (typed-list-null number?)) a))
 ;; #(error
 ;;   "expecting a typed-list-pair, got:"
 ;;   #((typed-list-null) #<procedure #12 number?>))
 )
