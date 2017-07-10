;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;; Parse source into a core Scheme AST.

;; Todo:

;; - DSSSL style arguments

;; - macro expander?

;; - how to handle environments across compilation units? (build in
;;   module system?)

;; - special syntaxes for e.g. function types, or detect inlined
;;   assertment code later?

;; - special syntax for let (through a macro?) to represent Ocaml's
;;   let ?

;; - systematic (property or tracing based?) tests

(require easy
	 jclass
	 typed-list
	 typed-alist
	 Maybe
	 show)

(export corescheme-literal-atom?
	corescheme-literal?
	(jclass cs-var)
	(jclass cs-expr
		(subclasses cs-literal
			    cs-lambda
			    cs-app
			    cs-ref
			    cs-def
			    cs-begin
			    cs-if
			    cs-set!
			    cs-letrec))
	(method source.cs-ast)
	
	#!optional
	cs-id
	cs-next-id!
	new-cs-var!
	cs-ctx?
	default-scheme-env)

;; Core Scheme representation

(def corescheme-literal-atom?
     ;; XX should null? be part here? Not asking because it's a list
     ;; thing, but because, could be user defined? ?
     (either number? string? boolean? symbol? keyword? null?))

;; XX really accept lists and vectors as literals?
(def (corescheme-literal? x)
     ((either corescheme-literal-atom?
	      ;; scheme has improper ones, yes
	      (both pair?
		    (improper-list-of corescheme-literal?))
	      (vector-of corescheme-literal?)) x))

(jclass (cs-var #((perhaps-source-of symbol?) name)
		#(natural0? id)))

(jclass cs-expr
	(jclass (cs-literal #(corescheme-literal? val)))

	(jclass (cs-lambda #((improper-list-of cs-var?) vars)
			   #(cs-expr? expr)))
       
	(jclass (cs-app #(cs-expr? proc)
			#((list-of cs-expr?) args)))
       
	(jclass (cs-ref #(cs-var? var)))

	(jclass (cs-def #(cs-var? var)
			#(cs-expr? val)))

	(jclass (cs-begin #((list-of cs-expr?) body)))
       
	(jclass (cs-if #(cs-expr? test)
		       #(cs-expr? then)
		       ;; should the missing-else case be encoded as
		       ;; explicit (void) ?
		       #((maybe cs-expr?) else)))
       
	(jclass (cs-set! #(cs-var? var)
			 #(cs-expr? val)))

	(jclass (cs-letrec #((list-of cs-var?) vars)
			   #((list-of cs-expr?) exprs))))


(defparameter cs-id #f)

(def (cs-next-id!)
     ;; forever the same  too.
     (let ((newid (inc (cs-id))))
       (cs-id newid)
       newid))

(def (new-cs-var! name)
     (cs-var name (cs-next-id!)))

;; ctx:Maybe-ref
(modimport/prefix ctx: (<typed-alist> symbol?
				      cs-var.name
				      (on source-code eq?)
				      cs-var?))


;; -- XX why typed-list not typed-alist ??? Ah because typed-alist is
;; a naked data structure? (Or at least doesn't currently offer a
;; predicate for it?)
;; > empty-cs-ctx
;; #((typed-list-null) #<procedure #2 cs-var?>)
;; Nah, could do it.
(def cs-ctx? (typed-list-of cs-var?))
(def empty-cs-ctx (typed-list cs-var?))

(def (default-scheme-env) -> cs-ctx?
     (list->typed-list
      cs-var?
      (map new-cs-var!
	   '(+ - * / cons car cdr zero? null?))))


(def (_cs:begin rest
		#(cs-ctx? ctx)
		#(boolean? realmode?))
     -> (if realmode? cs-expr? cs-ctx?)

     (if (one? rest)
	 (_cs (car rest) ctx realmode?)
     
	 ;; ctx changes need to be reflected both forward and
	 ;; backwards. Walk code twice, thus implement dry/real mode
	 ;; in this translation stage (cs) everywhere.
	 (let ((ctx*
		;; protect cs-id from changes, and force
		;; exceptions in case anyone requests them
		;; still
		(parameterize
		 ((cs-id (cs-id)))
		 (fold (lambda (r ctx)
			 (_cs r ctx #f))
		       ctx
		       rest))))
	   (if realmode?
	       (cs-begin
		(map (C _cs _ ctx* realmode?)
		     rest))
	       ctx*))))

(TEST
 > (def (t-_cs:begin v #!optional (get-env (lambda () empty-cs-ctx)))
	(values (parameterize ((cs-id 0))
			      (_cs:begin v (get-env) #t))
		(parameterize ((cs-id 0))
			      (_cs:begin v (get-env) #f))))
 > (.show (t-_cs:begin '((define a 1) (define b 2))))
 (values (cs-begin
	  (list (cs-def (cs-var 'a 1) (cs-literal 1))
		(cs-def (cs-var 'b 2) (cs-literal 2))))
	 (typed-list cs-var? (cs-var 'b 2) (cs-var 'a 1)))
 > (.show (t-_cs:begin '((define a 1) (define b a))))
 (values (cs-begin
	  (list (cs-def (cs-var 'a 1) (cs-literal 1))
		(cs-def (cs-var 'b 2) (cs-ref (cs-var 'a 1)))))
	 (typed-list cs-var? (cs-var 'b 2) (cs-var 'a 1)))
 > (.show (t-_cs:begin '((define a b) (define b a))))
 ;; this might be invalid Scheme, but valid Ocaml; XXX: ah, actually
 ;; ambiguous?, if b was defined earlier, that one is used instead!
 (values (cs-begin
	  (list (cs-def (cs-var 'a 1) (cs-ref (cs-var 'b 2)))
		(cs-def (cs-var 'b 2) (cs-ref (cs-var 'a 1)))))
	 (typed-list cs-var? (cs-var 'b 2) (cs-var 'a 1)))
 > (.show (.take (snd (t-_cs:begin '((define (odd? n)
				       (if (zero? n)
					   #f
					   (even? (- n 1))))
				     (define (even? n)
				       (if (zero? n)
					   #t
					   (odd? (- n 1)))))
				   default-scheme-env)) 3))
 (typed-list cs-var?
	     (cs-var 'even? 11)
	     (cs-var 'odd? 10)
	     (cs-var '+ 1)))


(defmacro (%return-normal e)
  `(return-normal (& ,e)))


(def (_cs expr
	  #(cs-ctx? ctx)
	  #(boolean? realmode?))
     -> (if realmode? cs-expr? cs-ctx?)

     (def (if-ctx-var sym *then *else)
	  (cond
	   ((assq sym ctx) =>
	    (lambda (sym+e)
	      (*then (cdr sym+e))))
	   (else
	    (*else))))

     ;; a normal return, i.e. return a value with unchanged ctx; this
     ;; means introduction of a scope boundary, i.e. not carrying
     ;; changes of the context outside the scope of the s-expression
     (def (return-normal v)
	  (if realmode? (v) ctx))
     (def (return-normal-value v)
	  (if realmode? v ctx))

     (mcase
      expr

      (symbol?
       (Maybe:cond ((ctx:Maybe-ref ctx (source-code expr)) =>
		    (comp return-normal-value cs-ref))
		   (else
		    (source-error expr "undefined variable"))))
      (corescheme-literal-atom?
       (%return-normal (cs-literal expr)))
      (pair?
       (let-pair
	((a r) (source-code expr))
	(Maybe:cond
	 ((ctx:Maybe-ref ctx (source-code a)) =>
	  (lambda (var)
	    (%return-normal
	     (cs-app (cs-ref var)
		     (map (comp fst
				;; dropping ctx changes; XX
				;; interesting: this is where ##begin
				;; would be invalid ~?
				(C _cs _ ctx realmode?))
			  r)))))
	 (else
	  (mcase
	   expr

	   (`(quote `val)
	    (%return-normal
	     (cs-literal val)))
	   (`(define `what . `rest)
	    (mcase what
		   (symbol?
		    (mcase rest
			   (`(`expr)
			    (let* ((what* (new-cs-var! what))
				   (ctx* (.cons ctx what*)))
			      ;; XX really letrec behaviour?
			      (if realmode?
				  (cs-def what*
					  (if realmode?
					      (fst (_cs expr ctx* realmode?))
					      (cs-dummy)))
				  ctx*)))))
		   (pair?
		    (let-pair
		     ((var args) (source-code what))
		     (let* ((var* (new-cs-var! var))
			    (ctx* (.cons ctx var*)))
		       (if realmode?
			   ;; XX copypaste from above, see improper-* usage
			   (let* ((args* (improper-map new-cs-var! args))
				  (ctx** (.improper-prepend ctx* args*)))
			     (cs-def var*
				     (cs-lambda
				      args*
				      (fst ;; dropping ctx changes
				       (_cs:begin rest ctx** realmode?)))))
			   ctx*))))))
	   (`(let `binds . `body)
	    (assert*
	     list? binds
	     (lambda (bs)
	       (let* ((v+e-s
		       (map (lambda (bind)
			      ;; XX dedup?
			      (mcase bind
				     (`(`var `expr)
				      (values var expr))))
			    (source-code binds)))
		      (vs (map (comp new-cs-var! fst) v+e-s))
		      (es (map (comp* fst (C _cs _ ctx realmode?) snd) v+e-s))
		      (ctx* (.improper-prepend ctx vs)))
		 (%return-normal
		  (cs-app
		   (cs-lambda
		    vs
		    (fst ;; dropping ctx changes
		     (_cs:begin body ctx* realmode?)))
		   es))))))
	   (`(let* `binds . `body)
	    ;; XX dedup code with let ?
	    (assert*
	     list? binds
	     (lambda (bs)
	       (let lp ( ;; in
			(bs bs)
			;; out
			(ctx ctx)
			(vars '())
			;; ^ could save this if we
			;; had an n-ary map that
			;; doesn't error out on
			;; uneven list lengths
			(exprs '()))
		 (if (null? bs)
		     ;; process body and wrap
		     ;; preprocessed outer scopes
		     ;; around it
		     (%return-normal
		      (fold (lambda (v+e expr)
			      (cs-app
			       (cs-lambda (list (fst v+e))
					  expr)
			       (list (snd v+e))))
			    (fst ;; dropping ctx changes
			     (_cs:begin body ctx realmode?))
			    (map values vars exprs)))

		     (let-pair
		      ((b bs*) bs)
		      (mcase
		       b
		       (`(`var `expr)
			(let ((var* (new-cs-var! var)))
			  (lp bs*
			      (.cons ctx var*)
			      (cons var* vars)
			      (cons
			       (fst
				;; dropping ctx changes; XX really
				;; interesting: this is "why" (let*
				;; ((y (begin (define x ..))) ..) is
				;; disallowed; "can't" (shouldn't?)
				;; carry over ctx changes.
				(_cs expr ctx realmode?))
			       exprs)))))))))))
	   (`(lambda `vars . `rest)
	    (%return-normal
	     (let* ((vars* (improper-map new-cs-var! vars))
		    (ctx* (.improper-prepend ctx vars*)))
	       (cs-lambda vars* (_cs:begin rest ctx* realmode?)))))
	   (`(begin . `rest)
	    (_cs:begin rest ctx realmode?))
	   (`(if `test `then)
	    (%return-normal
	     ;; dropping ctx changes
	     (cs-if (fst (_cs test ctx realmode?))
		    (fst (_cs then ctx realmode?))
		    #f)))
	   (`(if `test `then `else)
	    (%return-normal
	     (cs-if (fst (_cs test ctx realmode?))
		    (fst (_cs then ctx realmode?))
		    (fst (_cs else ctx realmode?)))))
	   (else
	    (source-error a
			  "undefined variable in function position")))))))))


(TEST
 > (.show (parameterize ((cs-id 0))
			(_cs '(define (even? n) (if (zero? n) #t (odd? (- n 1))))
			     empty-cs-ctx ;; (default-scheme-env)
			     #f)))
 (typed-list cs-var? (cs-var 'even? 1)))

(def. (source.cs-ast expr
		     #!optional
		     (get-ctx default-scheme-env)
		     (realmode? #t))
  -> cs-expr?

  (fst (parameterize ((cs-id 0))
		     (_cs expr (get-ctx) realmode?))))


(TEST
 > (def (catching thunk) ;; XX move something like/from this to lib
	(let ((orig-handler (current-exception-handler)))
	  (call/cc
	   (lambda (return)
	     (with-exception-handler
	      (lambda (e)
		(if (source-error? e)
		    (return (source-error-message e))
		    (orig-handler e)))
	      thunk)))))

 > (def (empty-environment) empty-cs-ctx)
 > (def (cs/empty c)
	(.show (source.cs-ast c empty-environment)))

 > (cs/empty '(define x 2))
 (cs-def (cs-var 'x 1) (cs-literal 2))
 > (cs/empty '(lambda x 2))
 (cs-lambda (cs-var 'x 1) (cs-literal 2))
 > (catching (& (source.cs-ast '(f x))))
 "undefined variable in function position"
 > (catching (& (source.cs-ast '(begin x 2))))
 "undefined variable"
 > (cs/empty '(let ((x 4))
		(begin x 2)))
 (cs-app (cs-lambda
	  (list (cs-var 'x 1))
	  (cs-begin (list (cs-ref (cs-var 'x 1)) (cs-literal 2))))
	 (list (cs-literal 4)))
 > (catching (& (source.cs-ast '(let ((x 4) (y x)) (begin x 2)))))
 "undefined variable"
 > (cs/empty '(let ((x 4) (y 5)) (begin x 2)))
 (cs-app (cs-lambda
	  (list (cs-var 'x 1) (cs-var 'y 2))
	  (cs-begin (list (cs-ref (cs-var 'x 1)) (cs-literal 2))))
	 (list (cs-literal 4) (cs-literal 5)))
 > (cs/empty '(let* ((x 4) (y x)) (begin x 2)))
 (cs-app (cs-lambda
	  (list (cs-var 'x 1))
	  (cs-app (cs-lambda
		   (list (cs-var 'y 2))
		   (cs-begin (list (cs-ref (cs-var 'x 1)) (cs-literal 2))))
		  (list (cs-ref (cs-var 'x 1)))))
	 (list (cs-literal 4)))


 > (def (cs/default c)
	(.show (source.cs-ast c default-scheme-env)))

 > (cs/default '(+ - * /))
 (cs-app (cs-ref (cs-var '+ 1))
	 (list (cs-ref (cs-var '- 2))
	       (cs-ref (cs-var '* 3))
	       (cs-ref (cs-var '/ 4))))
 > (cs/default '(lambda (n) (* n n)))
 (cs-lambda
  (list (cs-var 'n 10))
  (cs-app (cs-ref (cs-var '* 3))
	  (list (cs-ref (cs-var 'n 10)) (cs-ref (cs-var 'n 10))))))

