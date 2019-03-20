;; Copyright 2016-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Parse source into a core Scheme AST.

;; This is lossy for program representation: let and let* are
;; translated to lambda, for example. Also, quoted self-quoting values
;; (`'10) will lose the quote (become equivalent to `10).

;; Todo:

;; - DSSSL style arguments

;; - macro expander? Currently needs macro-expanded source (doesn't
;;   maintain lexical information other than variable bindings).

;; - how to handle environments across compilation units? (build in
;;   module system?)

;; - special syntaxes for e.g. function types, or detect inlined
;;   assertment code later?

;; - special syntax for let (through a macro?) to represent Ocaml's
;;   let ?

;; - systematic (property or tracing based?) tests

(require easy
         typed-list
	 typed-alist
	 Maybe
	 show)

(export (class corescheme-var)
	(class corescheme
               (classes corescheme-literal
                        corescheme-lambda
                        corescheme-app
                        corescheme-ref
                        corescheme-def
                        corescheme-begin
                        corescheme-if
                        corescheme-set!
                        corescheme-letrec))
	(method source.corescheme)
	make-scheme-env
        run-corescheme (macro RUN-CORESCHEME)
        
	#!optional
        corescheme:literal-atom?
	corescheme:literal?
	current-corescheme-id
	corescheme-next-id!
	new-corescheme-var!
	corescheme-ctx?
	default-scheme-env)

"Core Scheme representation"


;; move to some more general scheme lib?

(def (scheme:and-expand r)
     (fold-right (lambda (v r)
                   ;; heh, only way to do the cross-over with fold
                   ;; (without needing fold-right/last): "but could
                   ;; nobody give #t?", no, but even if could,
                   ;; interestingly it would still be valid? Boolean
                   ;; valued, yes. OK otherwise not?
                   (if (eq? r #t)
                       v
                       `(##if ,v
                              ,r
                              #f)))
                 #t
                 r))

(TEST
 > (define TEST:equal? syntax-equal?)
 > (scheme:and-expand '())
 #t
 > (scheme:and-expand '(a))
 a
 > (scheme:and-expand '(a b))
 (##if a
       b
       #f)
 > (scheme:and-expand '(a b c))
 (##if a
       (##if b
             c
             #f)
       #f))


(def (scheme:or-expand r)
     (fold-right (lambda (v r)
                   (if (eq? r #f)
                       v
                       (with-gensym
                        V
                        `(##let ((,V ,v))
                                (##if ,V
                                      ,V
                                      ,r)))))
                 #f
                 r))

(TEST
 > (define TEST:equal? syntax-equal?)
 > (scheme:or-expand '())
 #f
 > (scheme:or-expand '(a))
 a
 > (scheme:or-expand '(a b))
 (##let ((GEN:-1 a))
        (##if GEN:-1
              GEN:-1
              b))
 > (scheme:or-expand '(a b c))
 (##let ((GEN:-1 a))
        (##if GEN:-1
              GEN:-1
              (##let ((GEN:-2 b))
                     (##if GEN:-2
                           GEN:-2
                           c)))))


;; / general scheme lib


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


(defclass (corescheme-var [(possibly-source-of symbol?) name]
                          [natural0? id])

  (defmethod (equal? a b)
    (and (= id (corescheme-var.id b))
         (begin
           (assert (eq? name (corescheme-var.name b)))
           #t))))


(defparameter current-corescheme-id #f)
(defparameter current-optimizing? 'current-optimizing-is-unset)

;; "Monad runner" 
(def (run-corescheme thunk #!key optimizing? (corescheme-id 0))
     (parameterize ((current-corescheme-id corescheme-id)
                    (current-optimizing? optimizing?))
                   (thunk)))
(defmacro (RUN-CORESCHEME . body)
  `(run-corescheme (lambda () ,@body)))



(definterface corescheme-interface
  (method (references? s [(list-of corescheme-var?) vars]) -> boolean?
          "whether s contains any references to any of the vars")
  (method (num-references s [corescheme-var? var]) -> natural0?
          "how many references to var s contains")
  (method (interpolate s
                       [(list-of corescheme-var?) vars]
                       [(list-of corescheme?) exprs])
          -> corescheme?
          (assert (lengths-= vars exprs))
          "replace references to the given vars with the corresponding exprs."))


(defclass (corescheme [boolean? optimized?])
  implements: corescheme-interface

  (defclass ((corescheme-literal _corescheme-literal)
             [corescheme:literal? val])

    (def (corescheme-literal val)
         (let ((opt (current-optimizing?)))
           (_corescheme-literal opt val)))

    (defmethod (references? s vars) #f)
    (defmethod (num-references s var) 0)
    (defmethod (interpolate s vars exprs) s))

  (defclass ((corescheme-ref _corescheme-ref)
             [corescheme-var? var])

    (def (corescheme-ref var)
         (let ((opt (current-optimizing?)))
           (_corescheme-ref opt var)))

    (defmethod (references? s vars)
      (any (C corescheme-var.equal? _ var) vars))
    (defmethod (num-references s var*)
      (if (corescheme-var.equal? var var*) 1 0))
    (defmethod (interpolate s vars exprs)
      (let lp ((vars vars)
               (exprs exprs))
        (if-let-pair ((v vars*) vars)
                     (if (corescheme-var.equal? v var)
                         (first exprs)
                         (lp vars* (rest exprs)))
                     ;; hmm *was* s already optimized? good Question; ?
                     ;;(corescheme-ref var)
                     s))))

  (defclass ((corescheme-lambda _corescheme-lambda)
             [(improper-list-of corescheme-var?) vars]
             [corescheme? expr])

    (def (corescheme-lambda vars expr)
         (let ((opt (current-optimizing?)))
           (If opt (assert (corescheme.optimized? expr)))
           (_corescheme-lambda opt vars expr)))

    (defmethod (references? s vars)
      (.references? expr vars))
    (defmethod (num-references S var)
      ;; possible optimization: stop analyzing if var's name is in one
      ;; of vars' name?
      (.num-references expr var))
    (defmethod (interpolate s vars* exprs*)
      (corescheme-lambda vars
                         (.interpolate expr vars* exprs*))))
       
  (defclass ((corescheme-app _corescheme-app)
             [corescheme? proc]
             [(list-of corescheme?) args])

    (def (corescheme-app proc args)
         (let ((opt (current-optimizing?)))
           (If opt (assert (and (corescheme.optimized? proc)
                                (every corescheme.optimized? args))))
           (_corescheme-app opt proc args)))

    (defmethod (proc&args s)
      (cons proc args))

    (defmethod (references? s vars)
      (or (.references? proc vars)
          (any (C .references? _ vars) args)))
    (defmethod (num-references s var)
      (fold (lambda (arg tot)
              (+ tot (.num-references arg var)))
            (.num-references proc var)
            args))
    (defmethod (interpolate s vars* exprs*)
      (corescheme-app (.interpolate proc vars* exprs*)
                      (map (C .interpolate _ vars* exprs*)
                           args))))
       
  (defclass ((corescheme-def _corescheme-def)
             [corescheme-var? var]
             [corescheme? val])

    (def (corescheme-def var val)
         (let ((opt (current-optimizing?)))
           (If opt (assert (corescheme.optimized? val)))
           (_corescheme-def opt var val)))

    (defmethod (references? s vars)
      (.references? val vars))
    (defmethod (num-references s var*)
      (.num-references val var*))
    (defmethod (interpolate s vars* exprs*)
      (corescheme-def var (.interpolate val vars* exprs*))))

  (defclass ((corescheme-set! _corescheme-set!)
             [corescheme-var? var]
             [corescheme? val])

    (def (corescheme-set! var val)
         (let ((opt (current-optimizing?)))
           (If opt (assert (corescheme.optimized? val)))
           (_corescheme-set! opt var val)))

    (defmethod (references? s vars)
      (.references? val vars))
    (defmethod (num-references s var*)
      (.num-references val var*))
    (defmethod (interpolate s vars* exprs*)
      (corescheme-set! var (.interpolate val vars* exprs*))))

  (defclass ((corescheme-begin _corescheme-begin)
             [(list-of corescheme?) body])

    (def (corescheme-begin body)
         (let ((opt (current-optimizing?)))
           (If opt (assert (every corescheme.optimized? body)))
           (_corescheme-begin opt body)))

    (defmethod (references? s vars)
      (any (C .references? _ vars) body))
    (defmethod (num-references s var)
      (fold (lambda (expr tot)
              (+ tot (.num-references expr var)))
            0
            body))
    (defmethod (interpolate s vars* exprs*)
      (corescheme-begin (map (C .interpolate _ vars* exprs*)
                             body))))
       
  (defclass ((corescheme-if _corescheme-if)
             [corescheme? test]
             [corescheme? then]
             ;; should the missing-else case be encoded as
             ;; explicit (void) ?
             [(maybe corescheme?) else])

    (def (corescheme-if test then else)
         (let ((opt (current-optimizing?)))
           (If opt (assert (and (corescheme.optimized? test)
                                (corescheme.optimized? then)
                                (if else (corescheme.optimized? else) #t))))
           (_corescheme-if opt test then else)))

    (defmethod (references? s vars)
      (or (.references? test vars)
          (.references? then vars)
          (and else (.references? else vars))))
    (defmethod (num-references s var)
      (+ (.num-references test var)
         (.num-references then var)
         (if else (.num-references else var) 0)))
    (defmethod (interpolate s vars* exprs*)
      (corescheme-if (.interpolate test vars* exprs*)
                     (.interpolate then vars* exprs*)
                     (and else (.interpolate else vars* exprs*)))))
       
  (defclass ((corescheme-letrec _corescheme-letrec)
             [(list-of corescheme-var?) vars]
             [(list-of corescheme?) exprs]
             [corescheme? body-expr])

    (def (corescheme-letrec vars exprs body-expr)
         (let ((opt (current-optimizing?)))
           (If opt (assert (and (every corescheme.optimized? exprs)
                                (corescheme.optimized? body-expr))))
           (_corescheme-letrec opt vars exprs body-expr)))

    (defmethod (references? s vars)
      (or (any (C .references? _ vars) exprs)
          (.references? body-expr vars)))
    (defmethod (num-references s var)
      (fold (lambda (expr tot)
              (+ tot (.num-references expr var)))
            (.num-references body-expr var)
            exprs))
    (defmethod (interpolate s vars* exprs*)
      (corescheme-letrec vars
                         (map (C .interpolate _ vars* exprs*) exprs)
                         (.interpolate body-expr vars* exprs*)))))


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
;; [(typed-list-null) #<procedure #2 corescheme-var?>]
;; Nah, could do it.
(def corescheme-ctx? (typed-list-of corescheme-var?))
(def empty-corescheme-ctx (typed-list corescheme-var?))

(def (make-scheme-env [(list-of symbol?) ss]) -> corescheme-ctx?
     (list->typed-list
      corescheme-var?
      (map new-corescheme-var! ss)))

(def (default-scheme-env) -> corescheme-ctx?
     (make-scheme-env '(+ - * / cons car cdr zero? null?)))


(def (_source->corescheme:begin rest
				[corescheme-ctx? ctx]
				[boolean? realmode?])
     -> (if realmode? corescheme? corescheme-ctx?)

     (if (one-item? rest)
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
	(values (parameterize ((current-corescheme-id 0)
                               (current-optimizing? #f))
			      (_source->corescheme:begin v (get-env) #t))
		(parameterize ((current-corescheme-id 0))
			      (_source->corescheme:begin v (get-env) #f))))
 > (.show (t-_cs:begin '((define a 1) (define b 2))))
 (values (_corescheme-begin
          #f
	  (list (_corescheme-def #f
                                (corescheme-var 'a 1)
                                (_corescheme-literal #f 1))
		(_corescheme-def #f
                                (corescheme-var 'b 2)
                                (_corescheme-literal #f 2))))
	 (typed-list corescheme-var?
                     (corescheme-var 'b 2)
                     (corescheme-var 'a 1)))
 > (.show (t-_cs:begin '((define a 1) (define b a))))
 (values (_corescheme-begin
          #f
	  (list (_corescheme-def #f
                                (corescheme-var 'a 1)
                                (_corescheme-literal #f 1))
		(_corescheme-def #f
                                (corescheme-var 'b 2)
                                (_corescheme-ref #f (corescheme-var 'a 1)))))
	 (typed-list corescheme-var?
                     (corescheme-var 'b 2)
                     (corescheme-var 'a 1)))
 > (.show (t-_cs:begin '((define a b) (define b a))))
 ;; this might be invalid Scheme, but valid Ocaml; XXX: ah, actually
 ;; ambiguous?, if b was defined earlier, that one is used instead!
 (values (_corescheme-begin
          #f
          (list (_corescheme-def #f
                                (corescheme-var 'a 1)
                                (_corescheme-ref #f (corescheme-var 'b 2)))
                (_corescheme-def #f
                                (corescheme-var 'b 2)
                                (_corescheme-ref #f (corescheme-var 'a 1)))))
         (typed-list corescheme-var?
                     (corescheme-var 'b 2)
                     (corescheme-var 'a 1)))
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


(def (ctx-add-args [corescheme-ctx? ctx]
                   [(possibly-source-of any?) args])
     -> (values-of any? ;; args passed through new-corescheme-var!
                   corescheme-ctx?)
     ;; args can be a list of variables, or a single variable (rest
     ;; argument), or in the future DSSSL style declaration. |any?|
     ;; for now.
     (let* ((args* 
             (improper-map new-corescheme-var!
                           ;; if it's a pair, location info has to be
                           ;; stripped so that improper-map works.
                           (let ((args* (source-code args)))
                             (if (pair? args*) args* args))))
            (ctx* (.improper-prepend ctx args*)))
       (values args* ctx*)))


(def (_source->corescheme expr
			  [corescheme-ctx? ctx]
			  [boolean? realmode?])
     -> (if realmode? corescheme? corescheme-ctx?)

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
       (if (self-quoting? (source-code expr))
           (%return-normal (corescheme-literal expr))
           (source-error
            expr "unquoted empty list treated as invalid function application")))

      (pair?
       (let-pair
	((a r) (source-code expr))

        (let ((a* (source-code a)))
          (if
           (symbol? a*)

           (Maybe:if-let
            ((var (ctx:Maybe-ref ctx a*)))
            (%return-normal
             (corescheme-app
              (corescheme-ref var)
              ;; XX copy pasted further down
              (map (comp fst
                         ;; dropping ctx changes; XX
                         ;; interesting: this is where ##begin
                         ;; would be invalid ~?
                         (C _source->corescheme _ ctx realmode?))
                   r)))

            ;; else:
            (case a*
              ((##and and)
               (_source->corescheme (scheme:and-expand r)
                                    ctx
                                    ;; oh, realmode thing leads to
                                    ;; generating the above code
                                    ;; twice. XXX since it's not pure
                                    ;; (gensym) is this leading to
                                    ;; problems? (Make gensym take a
                                    ;; parameter and 'bind' that?)
                                    realmode?))
              ((##or or)
               (_source->corescheme (scheme:or-expand r)
                                    ctx
                                    ;; ditto, see above
                                    realmode?))

              ((##quote quote)
               (if (one-item? r)
                   (%return-normal (corescheme-literal (first r)))
                   (source-error expr "quote form needs 1 argument")))

              ((##define define)
               (if-let-pair
                ((what rest) r)
                (mcase what
                       (symbol?
                        (mcase rest
                               (`(`expr)
                                (let* ((what* (new-corescheme-var! what))
                                       (ctx* (.cons ctx what*)))
                                  ;; XX really letrec behaviour?
                                  (if realmode?
                                      (corescheme-def
                                       what*
                                       (if realmode?
                                           (fst (_source->corescheme
                                                 expr
                                                 ctx*
                                                 realmode?))
                                           (corescheme-dummy)))
                                      ctx*)))))
                       (pair?
                        (let-pair
                         ((var args) (source-code what))
                         (let* ((var* (new-corescheme-var! var))
                                (ctx* (.cons ctx var*)))
                           (if realmode?
                               (letv ((args* ctx**) (ctx-add-args ctx* args))
                                     (corescheme-def
                                      var*
                                      (corescheme-lambda
                                       args*
                                       (fst ;; dropping ctx changes
                                        (_source->corescheme:begin
                                         rest
                                         ctx**
                                         realmode?)))))
                               ctx*)))))
                (source-error expr "define form needs at least 1 argument")))

              ((##let let)
               `(let `binds . `body)
               (if-let-pair
                ((binds body) r)
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
                          (es (map (comp*
                                    fst
                                    (C _source->corescheme _ ctx realmode?)
                                    snd)
                                   v+e-s))
                          (ctx* (.improper-prepend ctx vs)))
                     (%return-normal
                      (corescheme-app
                       (corescheme-lambda
                        vs
                        (fst ;; dropping ctx changes
                         (_source->corescheme:begin body ctx* realmode?)))
                       es)))))
                (source-error expr "let form needs at least 1 argument")))
              
              ((##let* let*)
               (if-let-pair
                ((binds body) r)
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
                                   exprs))))))))))
                (source-error expr "let* form needs at least 1 argument")))

              ((##lambda lambda)
               (if-let-pair
                ((vars rest) r)
                (%return-normal
                 (letv ((vars* ctx*) (ctx-add-args ctx vars))
                       (corescheme-lambda
                        vars*
                        (_source->corescheme:begin rest ctx* realmode?))))
                (source-error expr "lambda form needs at least 1 argument")))

              ((##begin begin)
               (_source->corescheme:begin r ctx realmode?))

              ((##if if)
               (cond ((length-= r 2)
                      (%return-normal
                       ;; dropping ctx changes
                       (corescheme-if
                        (fst (_source->corescheme (first r) ctx realmode?))
                        (fst (_source->corescheme (second r) ctx realmode?))
                        #f)))
                     ((length-= r 3)
                      (%return-normal
                       (corescheme-if
                        (fst (_source->corescheme (first r) ctx realmode?))
                        (fst (_source->corescheme (second r) ctx realmode?))
                        (fst (_source->corescheme (third r) ctx realmode?)))))
                     (else
                      (source-error expr "if form needs 2 or 3 arguments"))))
              
              (else
               (source-error a
                             "undefined variable in function position"))))

           ;; head is not a symbol: function application
           (if (list? r)
               (corescheme-app
                (_source->corescheme a ctx realmode?)
                ;; XX copy paste
                (map
                 (comp fst
                       ;; dropping ctx changes; XX
                       ;; interesting: this is where ##begin
                       ;; would be invalid ~?
                       (C _source->corescheme _ ctx realmode?))
                 r))
               (source-error
                expr "improper list can't be function application"))))))))


(TEST
 > (.show (parameterize
           ((current-corescheme-id 0))
           (_source->corescheme
            '(define (even? n) (if (zero? n) #t (odd? (- n 1))))
            empty-corescheme-ctx ;; (default-scheme-env)
            #f)))
 (typed-list corescheme-var? (corescheme-var 'even? 1)))


(def. (source.corescheme expr
			 #!key
                         ;; globals and get-ctx are doing the same,
                         ;; hence redundant, globals is simply
                         ;; easier.
                         globals
			 (get-ctx default-scheme-env)
			 (realmode? #t))
  -> corescheme?

  (let ((actual-get-ctx (if globals
                            (C make-scheme-env globals)
                            get-ctx)))
    (fst (RUN-CORESCHEME (_source->corescheme expr (actual-get-ctx) realmode?)))))


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
	(.show (source.corescheme c get-ctx: empty-environment)))

 > (cs/empty '(define x 2))
 (_corescheme-def #f (corescheme-var 'x 1) (_corescheme-literal #f 2))
 > (cs/empty '(lambda x 2))
 (_corescheme-lambda #f (corescheme-var 'x 1) (_corescheme-literal #f 2))
 > (catching (& (source.corescheme '(f x))))
 "undefined variable in function position"
 > (catching (& (source.corescheme '(begin x 2))))
 "undefined variable"
 > (cs/empty '(let ((x 4))
		(begin x 2)))
 (_corescheme-app #f
                 (_corescheme-lambda
                  #f
		  (list (corescheme-var 'x 1))
		  (_corescheme-begin
                   #f
                   (list (_corescheme-ref #f (corescheme-var 'x 1))
                         (_corescheme-literal #f 2))))
		 (list (_corescheme-literal #f 4)))
 > (catching (& (source.corescheme '(let ((x 4) (y x)) (begin x 2)))))
 "undefined variable"
 > (cs/empty '(let ((x 4) (y 5)) (begin x 2)))
 (_corescheme-app
  #f (_corescheme-lambda
      #f
      (list (corescheme-var 'x 1) (corescheme-var 'y 2))
      (_corescheme-begin #f
                        (list (_corescheme-ref #f (corescheme-var 'x 1))
                              (_corescheme-literal #f 2))))
  (list (_corescheme-literal #f 4) (_corescheme-literal #f 5)))
 > (cs/empty '(let* ((x 4) (y x)) (begin x 2)))
 (_corescheme-app
  #f (_corescheme-lambda
      #f
      (list (corescheme-var 'x 1))
      (_corescheme-app
       #f (_corescheme-lambda
           #f
           (list (corescheme-var 'y 2))
           (_corescheme-begin
            #f
            (list (_corescheme-ref #f (corescheme-var 'x 1))
                  (_corescheme-literal #f 2))))
       (list (_corescheme-ref #f (corescheme-var 'x 1)))))
  (list (_corescheme-literal #f 4)))


 > (def (cs/default c)
	(.show (source.corescheme c get-ctx: default-scheme-env)))

 > (cs/default '(+ - * /))
 (_corescheme-app #f
                 (_corescheme-ref #f (corescheme-var '+ 1))
		 (list (_corescheme-ref #f (corescheme-var '- 2))
		       (_corescheme-ref #f (corescheme-var '* 3))
		       (_corescheme-ref #f (corescheme-var '/ 4))))
 > (cs/default '(lambda (n) (* n n)))
 (_corescheme-lambda
  #f
  (list (corescheme-var 'n 10))
  (_corescheme-app #f
                  (_corescheme-ref #f (corescheme-var '* 3))
		  (list (_corescheme-ref #f (corescheme-var 'n 10))
                        (_corescheme-ref #f (corescheme-var 'n 10))))))



(TEST
 > (parameterize ((current-optimizing? #f))
                 (.interpolate (corescheme-ref (corescheme-var 'x 1))
                               (list (corescheme-var 'y 2)
                                     (corescheme-var 'x 1))
                               (list (corescheme-literal 99)
                                     (corescheme-literal 77))))
 ;; #f since it's interpolated in non-optimizing context
 [(corescheme-literal) #f 77])
