
(require define-macro-star
	 cj-symbol
	 test)

(define-macro* (let-named* ident bindings . body)
  (let* ((vars-flat
	  (map (lambda(v)
		 (assert* pair? v
			  car))
	       (source-code bindings)))
	 
	 (loop
	  (gensym 'let-named*-loop-))
	 
	 (submacro
	  `(##define-macro
	    (,ident . args)
	    ;; build an alist  varname -> syntax
	    (let ((alis
		   (let loop ((l '())
			      (a args))
		     (cond ((pair? a)
			    (or (pair? (cdr a))
				(error "let-named* call: identifyer must be called with an even number of arguments: "
				       ',ident args))
			    (let ((key (car a))
				  (val (cadr a)))
			      (or (keyword? key)
				  (error "let-named* call: expecting keyword in identifyer call: "
					 ',ident key))
			      (let ((varnam (string->symbol
					     (keyword->string key))))
				(or (member varnam ',vars-flat)
				    (error "let-named* call: no such variable:"
					   ',ident varnam))
				(and (assoc varnam l)
				     (error "let-named* call: same keyword given twice: "
					    ',ident varnam))
				(loop (cons (cons varnam val) l)
				      (cddr a)))))
			   (else l)))))
	      (cons ',loop
		    (map (lambda(v)
			   (cond ((assoc v alis)
				  => cdr)
				 (else v))) ',vars-flat))))))
    `(let ,loop ,bindings
	  ,submacro
	  ,@body)))


(TEST
 > (let* ((l '())
	  (res (let-named* fun ((a 3)(b 4))
			   (push! l (list a b))
			   (if (> b 0)
			       (fun b: (- b 1))
			       a))))
     (vector (reverse l) res))
 #(((3 4) (3 3) (3 2) (3 1) (3 0)) 3)
 > (let* ((l '())
	  (res (let-named* fun ((a 3)(b 4))
			   (push! l (list a b))
			   (if (> b 0)
			       (fun b: (- b 1) a: (modulo b 2))
			       a))))
     (vector (reverse l) res))
 #(((3 4) (0 3) (1 2) (0 1) (1 0)) 1))
