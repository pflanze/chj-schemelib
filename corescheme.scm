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

(export corescheme:literal-atom?
	corescheme:literal?
	(jclass corescheme-var)
	(jclass corescheme-expr
		(subclasses corescheme-literal
			    corescheme-lambda
			    corescheme-app
			    corescheme-ref
			    corescheme-def
			    corescheme-begin
			    corescheme-if
			    corescheme-set!
			    corescheme-letrec))
	(method source.corescheme)
	
	#!optional
	current-corescheme-id
	corescheme-next-id!
	new-corescheme-var!
	corescheme-ctx?
	default-scheme-env)

;; Core Scheme representation

(def corescheme:literal-atom?
     ;; XX should null? be part here? Not asking because it's a list
     ;; thing, but because, could be user defined? ?
     (either number? string? boolean? symbol? keyword? null?))

;; XX really accept lists and vectors as literals?
(def (corescheme:literal? x)
     ((either corescheme:literal-atom?
	      ;; scheme has improper ones, yes
	      (both pair?
		    (improper-list-of corescheme:literal?))
	      (vector-of corescheme:literal?)) x))

(jclass (corescheme-var #((perhaps-source-of symbol?) name)
		#(natural0? id)))

(jclass corescheme-expr
	(jclass (corescheme-literal #(corescheme:literal? val)))

	(jclass (corescheme-lambda #((improper-list-of corescheme-var?) vars)
			   #(corescheme-expr? expr)))
       
	(jclass (corescheme-app #(corescheme-expr? proc)
			#((list-of corescheme-expr?) args)))
       
	(jclass (corescheme-ref #(corescheme-var? var)))

	(jclass (corescheme-def #(corescheme-var? var)
			#(corescheme-expr? val)))

	(jclass (corescheme-begin #((list-of corescheme-expr?) body)))
       
	(jclass (corescheme-if #(corescheme-expr? test)
		       #(corescheme-expr? then)
		       ;; should the missing-else case be encoded as
		       ;; explicit (void) ?
		       #((maybe corescheme-expr?) else)))
       
	(jclass (corescheme-set! #(corescheme-var? var)
			 #(corescheme-expr? val)))

	(jclass (corescheme-letrec #((list-of corescheme-var?) vars)
			   #((list-of corescheme-expr?) exprs))))


(defparameter current-corescheme-id #f)

(def (corescheme-next-id!)
     ;; forever the same  too.
     (let ((newid (inc (current-corescheme-id))))
       (current-corescheme-id newid)
       newid))

(def (new-corescheme-var! name)
     (corescheme-var name (corescheme-next-id!)))

;; ctx:Maybe-ref
(modimport/prefix ctx: (<typed-alist> symbol?
				      corescheme-var.name
				      (on source-code eq?)
				      corescheme-var?))


;; -- XX why typed-list not typed-alist ??? Ah because typed-alist is
;; a naked data structure? (Or at least doesn't currently offer a
;; predicate for it?)
;; > empty-corescheme-ctx
;; #((typed-list-null) #<procedure #2 corescheme-var?>)
;; Nah, could do it.
(def corescheme-ctx? (typed-list-of corescheme-var?))
(def empty-corescheme-ctx (typed-list corescheme-var?))

(def (default-scheme-env) -> corescheme-ctx?
     (list->typed-list
      corescheme-var?
      (map new-corescheme-var!
	   '(+ - * / cons car cdr zero? null?))))


(def (_source->corescheme:begin rest
		#(corescheme-ctx? ctx)
		#(boolean? realmode?))
     -> (if realmode? corescheme-expr? corescheme-ctx?)

     (if (one? rest)
	 (_source->corescheme (car rest) ctx realmode?)
     
	 ;; ctx changes need to be reflected both forward and
	 ;; backwards. Walk code twice, thus implement dry/real mode
	 ;; in this translation stage (cs) everywhere.
	 (let ((ctx*
		;; protect current-corescheme-id from changes, and force
		;; exceptions in case anyone requests them
		;; still
		(parameterize
		 ((current-corescheme-id (current-corescheme-id)))
		 (fold (lambda (r ctx)
			 (_source->corescheme r ctx #f))
		       ctx
		       rest))))
	   (if realmode?
	       (corescheme-begin
		(map (C _source->corescheme _ ctx* realmode?)
		     rest))
	       ctx*))))

(TEST
 > (def (t-_cs:begin v #!optional (get-env (lambda () empty-corescheme-ctx)))
	(values (parameterize ((current-corescheme-id 0))
			      (_source->corescheme:begin v (get-env) #t))
		(parameterize ((current-corescheme-id 0))
			      (_source->corescheme:begin v (get-env) #f))))
 > (.show (t-_cs:begin '((define a 1) (define b 2))))
 (values (corescheme-begin
	  (list (corescheme-def (corescheme-var 'a 1) (corescheme-literal 1))
		(corescheme-def (corescheme-var 'b 2) (corescheme-literal 2))))
	 (typed-list corescheme-var? (corescheme-var 'b 2) (corescheme-var 'a 1)))
 > (.show (t-_cs:begin '((define a 1) (define b a))))
 (values (corescheme-begin
	  (list (corescheme-def (corescheme-var 'a 1) (corescheme-literal 1))
		(corescheme-def (corescheme-var 'b 2) (corescheme-ref (corescheme-var 'a 1)))))
	 (typed-list corescheme-var? (corescheme-var 'b 2) (corescheme-var 'a 1)))
 > (.show (t-_cs:begin '((define a b) (define b a))))
 ;; this might be invalid Scheme, but valid Ocaml; XXX: ah, actually
 ;; ambiguous?, if b was defined earlier, that one is used instead!
 (values (corescheme-begin
	  (list (corescheme-def (corescheme-var 'a 1) (corescheme-ref (corescheme-var 'b 2)))
		(corescheme-def (corescheme-var 'b 2) (corescheme-ref (corescheme-var 'a 1)))))
	 (typed-list corescheme-var? (corescheme-var 'b 2) (corescheme-var 'a 1)))
 > (.show (.take (snd (t-_cs:begin '((define (odd? n)
				       (if (zero? n)
					   #f
					   (even? (- n 1))))
				     (define (even? n)
				       (if (zero? n)
					   #t
					   (odd? (- n 1)))))
				   default-scheme-env)) 3))
 (typed-list corescheme-var?
	     (corescheme-var 'even? 11)
	     (corescheme-var 'odd? 10)
	     (corescheme-var '+ 1)))


(defmacro (%return-normal e)
  `(return-normal (& ,e)))


(def (_source->corescheme expr
	  #(corescheme-ctx? ctx)
	  #(boolean? realmode?))
     -> (if realmode? corescheme-expr? corescheme-ctx?)

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
		    (comp return-normal-value corescheme-ref))
		   (else
		    (source-error expr "undefined variable"))))
      (corescheme:literal-atom?
       (%return-normal (corescheme-literal expr)))
      (pair?
       (let-pair
	((a r) (source-code expr))
	(Maybe:cond
	 ((ctx:Maybe-ref ctx (source-code a)) =>
	  (lambda (var)
	    (%return-normal
	     (corescheme-app (corescheme-ref var)
		     (map (comp fst
				;; dropping ctx changes; XX
				;; interesting: this is where ##begin
				;; would be invalid ~?
				(C _source->corescheme _ ctx realmode?))
			  r)))))
	 (else
	  (mcase
	   expr

	   (`(quote `val)
	    (%return-normal
	     (corescheme-literal val)))
	   (`(define `what . `rest)
	    (mcase what
		   (symbol?
		    (mcase rest
			   (`(`expr)
			    (let* ((what* (new-corescheme-var! what))
				   (ctx* (.cons ctx what*)))
			      ;; XX really letrec behaviour?
			      (if realmode?
				  (corescheme-def what*
					  (if realmode?
					      (fst (_source->corescheme expr ctx* realmode?))
					      (corescheme-dummy)))
				  ctx*)))))
		   (pair?
		    (let-pair
		     ((var args) (source-code what))
		     (let* ((var* (new-corescheme-var! var))
			    (ctx* (.cons ctx var*)))
		       (if realmode?
			   ;; XX copypaste from above, see improper-* usage
			   (let* ((args* (improper-map new-corescheme-var! args))
				  (ctx** (.improper-prepend ctx* args*)))
			     (corescheme-def var*
				     (corescheme-lambda
				      args*
				      (fst ;; dropping ctx changes
				       (_source->corescheme:begin rest ctx** realmode?)))))
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
		      (vs (map (comp new-corescheme-var! fst) v+e-s))
		      (es (map (comp* fst (C _source->corescheme _ ctx realmode?) snd) v+e-s))
		      (ctx* (.improper-prepend ctx vs)))
		 (%return-normal
		  (corescheme-app
		   (corescheme-lambda
		    vs
		    (fst ;; dropping ctx changes
		     (_source->corescheme:begin body ctx* realmode?)))
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
			      (corescheme-app
			       (corescheme-lambda (list (fst v+e))
					  expr)
			       (list (snd v+e))))
			    (fst ;; dropping ctx changes
			     (_source->corescheme:begin body ctx realmode?))
			    (map values vars exprs)))

		     (let-pair
		      ((b bs*) bs)
		      (mcase
		       b
		       (`(`var `expr)
			(let ((var* (new-corescheme-var! var)))
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
				(_source->corescheme expr ctx realmode?))
			       exprs)))))))))))
	   (`(lambda `vars . `rest)
	    (%return-normal
	     (let* ((vars* (improper-map new-corescheme-var! vars))
		    (ctx* (.improper-prepend ctx vars*)))
	       (corescheme-lambda vars* (_source->corescheme:begin rest ctx* realmode?)))))
	   (`(begin . `rest)
	    (_source->corescheme:begin rest ctx realmode?))
	   (`(if `test `then)
	    (%return-normal
	     ;; dropping ctx changes
	     (corescheme-if (fst (_source->corescheme test ctx realmode?))
		    (fst (_source->corescheme then ctx realmode?))
		    #f)))
	   (`(if `test `then `else)
	    (%return-normal
	     (corescheme-if (fst (_source->corescheme test ctx realmode?))
		    (fst (_source->corescheme then ctx realmode?))
		    (fst (_source->corescheme else ctx realmode?)))))
	   (else
	    (source-error a
			  "undefined variable in function position")))))))))


(TEST
 > (.show (parameterize ((current-corescheme-id 0))
			(_source->corescheme '(define (even? n) (if (zero? n) #t (odd? (- n 1))))
			     empty-corescheme-ctx ;; (default-scheme-env)
			     #f)))
 (typed-list corescheme-var? (corescheme-var 'even? 1)))

(def. (source.corescheme expr
		     #!optional
		     (get-ctx default-scheme-env)
		     (realmode? #t))
  -> corescheme-expr?

  (fst (parameterize ((current-corescheme-id 0))
		     (_source->corescheme expr (get-ctx) realmode?))))


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

 > (def (empty-environment) empty-corescheme-ctx)
 > (def (cs/empty c)
	(.show (source.corescheme c empty-environment)))

 > (cs/empty '(define x 2))
 (corescheme-def (corescheme-var 'x 1) (corescheme-literal 2))
 > (cs/empty '(lambda x 2))
 (corescheme-lambda (corescheme-var 'x 1) (corescheme-literal 2))
 > (catching (& (source.corescheme '(f x))))
 "undefined variable in function position"
 > (catching (& (source.corescheme '(begin x 2))))
 "undefined variable"
 > (cs/empty '(let ((x 4))
		(begin x 2)))
 (corescheme-app (corescheme-lambda
	  (list (corescheme-var 'x 1))
	  (corescheme-begin (list (corescheme-ref (corescheme-var 'x 1)) (corescheme-literal 2))))
	 (list (corescheme-literal 4)))
 > (catching (& (source.corescheme '(let ((x 4) (y x)) (begin x 2)))))
 "undefined variable"
 > (cs/empty '(let ((x 4) (y 5)) (begin x 2)))
 (corescheme-app (corescheme-lambda
	  (list (corescheme-var 'x 1) (corescheme-var 'y 2))
	  (corescheme-begin (list (corescheme-ref (corescheme-var 'x 1)) (corescheme-literal 2))))
	 (list (corescheme-literal 4) (corescheme-literal 5)))
 > (cs/empty '(let* ((x 4) (y x)) (begin x 2)))
 (corescheme-app (corescheme-lambda
	  (list (corescheme-var 'x 1))
	  (corescheme-app (corescheme-lambda
		   (list (corescheme-var 'y 2))
		   (corescheme-begin (list (corescheme-ref (corescheme-var 'x 1)) (corescheme-literal 2))))
		  (list (corescheme-ref (corescheme-var 'x 1)))))
	 (list (corescheme-literal 4)))


 > (def (cs/default c)
	(.show (source.corescheme c default-scheme-env)))

 > (cs/default '(+ - * /))
 (corescheme-app (corescheme-ref (corescheme-var '+ 1))
	 (list (corescheme-ref (corescheme-var '- 2))
	       (corescheme-ref (corescheme-var '* 3))
	       (corescheme-ref (corescheme-var '/ 4))))
 > (cs/default '(lambda (n) (* n n)))
 (corescheme-lambda
  (list (corescheme-var 'n 10))
  (corescheme-app (corescheme-ref (corescheme-var '* 3))
	  (list (corescheme-ref (corescheme-var 'n 10)) (corescheme-ref (corescheme-var 'n 10))))))

