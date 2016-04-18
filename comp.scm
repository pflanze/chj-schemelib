;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;; (Yet Another Stub for a Scheme Compiler)

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
	 more-oo
	 alist
	 Maybe)


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

(class cs-var
       (struct #((perhaps-source-of symbol?) name)
	       #(natural0? id)))

(class cs-expr
       (subclass cs-literal
		 (struct #(corescheme-literal? val))
		 (method (scheme-code v)
			 (let-cs-literal
			  ((val) v)
			  (cond (((either symbol? keyword? null?)
				  (source-code val))
				 `(quote ,val))
				(else
				 val)))))

       (subclass cs-lambda
		 (struct #((improper-list-of cs-var?) vars)
			 #(cs-expr? expr))
		 (method (scheme-code v)
			 (let-cs-lambda ((vars expr) v)
					`(lambda ,(map .name vars)
					   ,@(map .scheme-code
						  (if (cs-begin? expr)
						      (.body expr)
						      (list expr)))))))
       (subclass cs-app
		 (struct #(cs-expr? proc)
			 #((list-of cs-expr?) args))
		 (method (scheme-code v)
			 (let-cs-app ((proc args) v)
				     `(,(.scheme-code proc)
				       ,@(map .scheme-code args)))))
       (subclass cs-ref
		 (struct #(cs-var? var))
		 (method scheme-code (comp .name .var)))

       (subclass cs-def
		 (struct #(cs-var? var)
			 #(cs-expr? val))
		 (method (scheme-code v)
			 (let-cs-def ((var val) v)
				     `(define ,(.name var)
					,(.scheme-code val)))))

       (subclass cs-begin
		 (struct #((list-of cs-expr?) body))
		 (method (scheme-code v)
			 `(begin ,@(map .scheme-code (.body v)))))

       (subclass cs-if
		 (struct #(cs-expr? test)
			 #(cs-expr? then)
			 ;; should the missing-else case be encoded as
			 ;; explicit (void) ?
			 #((maybe cs-expr?) else))
		 (method (scheme-code v)
			 (let-cs-if ((test then else) v)
				    `(if ,(.scheme-code test)
					 ,(.scheme-code then)
					 ,@(if else
					       (list (.scheme-code else))
					       '())))))
       
       (subclass cs-set!
		 (struct #(cs-var? var)
			 #(cs-expr? val))
		 (method (scheme-code v)
			 `(set! ,(.name var)
				,(.scheme-code val))))

       ;; XX?
       (subclass cs-letrec
		 (struct #((list-of cs-var?) vars)
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
(modimport/prefix ctx: (<alist> symbol?
				cs-var.name
				(on source-code eq?)))


;; XX efficiency, want typed lists.
(def cs-ctx? (list-of cs-var?))

(def (default-scheme-env) -> (list-of cs-var?)
     (map new-cs-var!
	  '(+ - * / cons car cdr zero? null?)))


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
;;			 (step)
			 (_cs r ctx #f))
		       ctx
		       rest))))
	   (if realmode?
	       (cs-begin
		(map (C _cs _ ctx* realmode?)
		     rest))
	       ctx*))))

(TEST
 > (def (t-_cs:begin v #!optional (get-env (lambda () '())))
	(vector (parameterize ((cs-id 0))
			      (_cs:begin v (get-env) #t))
		(parameterize ((cs-id 0))
			      (_cs:begin v (get-env) #f))))
 > (t-_cs:begin '((define a 1) (define b 2)))
 #(#(cs-begin
     (#(cs-def #(cs-var a 1) #(cs-literal 1))
       #(cs-def #(cs-var b 2) #(cs-literal 2))))
    (#(cs-var b 2) #(cs-var a 1)))
 > (t-_cs:begin '((define a 1) (define b a)))
 #(#(cs-begin
     (#(cs-def #(cs-var a 1) #(cs-literal 1))
       #(cs-def #(cs-var b 2) #(cs-ref #(cs-var a 1)))))
    (#(cs-var b 2) #(cs-var a 1)))
 > (t-_cs:begin '((define a b) (define b a)))
 ;; this might be invalid Scheme, but valid Ocaml; XXX: ah, actually
 ;; ambiguous?, if b was defined earlier, that one is used instead!
 #(#(cs-begin
     (#(cs-def #(cs-var a 1) #(cs-ref #(cs-var b 2)))
       #(cs-def #(cs-var b 2) #(cs-ref #(cs-var a 1)))))
    (#(cs-var b 2) #(cs-var a 1)))
 > (take (vector-ref
	  (t-_cs:begin '((define (odd? n)
			   (if (zero? n)
			       #f
			       (even? (- n 1))))
			 (define (even? n)
			   (if (zero? n)
			       #t
			       (odd? (- n 1)))))
		       default-scheme-env)
	  1) 3)
 (#(cs-var even? 11)
   #(cs-var odd? 10)
   #(cs-var + 1))
 )


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
				   (ctx* (cons what* ctx)))
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
			    (ctx* (cons var* ctx)))
		       (if realmode?
			   ;; XX copypaste from above, see improper-* usage
			   (let* ((args* (improper-map new-cs-var! args))
				  (ctx** (improper-append args* ctx*)))
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
		      (ctx* (append vs ctx)))
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
			      (cons var* ctx)
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
		    (ctx* (improper-append vars* ctx)))
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
 > (parameterize ((cs-id 0))
		 (_cs '(define (even? n) (if (zero? n) #t (odd? (- n 1))))
		      '() ;; (default-scheme-env)
		      #f))
 (#(cs-var even? 1)))

(def (cs expr
	 #!optional
	 (get-ctx (lambda () '()))
	 (realmode? #t)) -> cs-expr?
     (fst (parameterize ((cs-id 0))
			(_cs expr (get-ctx) realmode?))))


(TEST
 > (def (catching thunk)
	(with-exception-catcher source-error-message thunk))
 > (cs '(define x 2))
 #(cs-def #(cs-var x 1) #(cs-literal 2))
 > (cs '(lambda x 2))
 #(cs-lambda #(cs-var x 1) #(cs-literal 2))
 > (catching (& (cs '(f x))))
 "undefined variable in function position"
 > (catching (& (cs '(begin x 2))))
 "undefined variable"
 > (cs '(let ((x 4))
	  (begin x 2)))
 #(cs-app
   #(cs-lambda
     (#(cs-var x 1))
     #(cs-begin (#(cs-ref #(cs-var x 1)) #(cs-literal 2))))
   (#(cs-literal 4)))
 > (catching (& (cs '(let ((x 4) (y x)) (begin x 2)))))
 "undefined variable"
 > (cs '(let ((x 4) (y 5)) (begin x 2)))
 #(cs-app
   #(cs-lambda
     (#(cs-var x 1) #(cs-var y 2))
     #(cs-begin (#(cs-ref #(cs-var x 1)) #(cs-literal 2))))
   (#(cs-literal 4) #(cs-literal 5)))
 > (cs '(let* ((x 4) (y x)) (begin x 2)))
 #(cs-app
   #(cs-lambda
     (#(cs-var x 1))
     #(cs-app
       #(cs-lambda
	 (#(cs-var y 2))
	 #(cs-begin (#(cs-ref #(cs-var x 1)) #(cs-literal 2))))
       (#(cs-ref #(cs-var x 1)))))
   (#(cs-literal 4)))
 > (cs '(+ - * /) default-scheme-env)
 #(cs-app
   #(cs-ref #(cs-var + 1))
   (#(cs-ref #(cs-var - 2)) #(cs-ref #(cs-var * 3)) #(cs-ref #(cs-var / 4))))
 > (cs '(lambda (n) (* n n)) default-scheme-env)
 #(cs-lambda
   (#(cs-var n 10))
   #(cs-app
     #(cs-ref #(cs-var * 3))
     (#(cs-ref #(cs-var n 10)) #(cs-ref #(cs-var n 10)))))
 )


;; change to <failing-on> for debugging
(modimport/prefix failing: <failing-off>)

(TEST
 > (def (cs-back source)
	(.scheme-code
	 (cs source default-scheme-env)))
 > (def t-scheme-code
	(lambda (source result)
	  (and
	   ;; are the given code fragments even evaluating to the same
	   ;; value?
	   (failing:equal? (eval source)
			   (eval result))
	   ;; and does the compiler actually give the given result?
	   (failing:source-equal? (cs-back source)
				  result))))
 > (def t-scheme-code*
	(lambda (v)
	  (let-pair ((a b) (source-code v))
		    (t-scheme-code a b))))
 > (def failures
	(qcheck
	 (source-code
	  (quote-source
	   (;; source and expected result from cs-back in pairs
	    ((let* ((x 4) (y x))
	       (begin 2 x))
	     .
	     ((lambda (x) ((lambda (y) 2 x) x)) 4))

	    ((let ((x 4) (y 5)) (begin 2 x))
	     .
	     ((lambda (x y) 2 x) 4 5))

	    ((let ((x 4) (y 5)) 2 x)
	     .
	     ((lambda (x y) 2 x) 4 5))

	    ((let ((x '()))
	       x)
	     .
	     ((lambda (x) x) '()))

	    ((define (square n)
	       (* n n))
	     .
	     (define square (lambda (n) (* n n))))

	    ((define (fact n)
	       (if (zero? n)
		   1
		   (* n (fact (- n 1)))))
	     .
	     (define fact
	       (lambda (n)
		 (if (zero? n)
		     1
		     (* n (fact (- n 1)))))))
	    
	    ((begin (define a 1) (define b a))
	     .
	     (begin (define a 1) (define b a)))

	    ((begin
	       (define (odd? n)
	    	 (if (zero? n)
	    	     #f
	    	     (even? (- n 1))))
	       (define (even? n)
	    	 (if (zero? n)
	    	     #t
	    	     (odd? (- n 1)))))
	     .
	     (begin
	       (define odd?
		 (lambda (n)
		   (if (zero? n)
		       #f
		       (even? (- n 1)))))
	       (define even?
		 (lambda (n)
		   (if (zero? n)
		       #t
		       (odd? (- n 1)))))))
	    )))
	 t-scheme-code*))
 > failures
 ())

