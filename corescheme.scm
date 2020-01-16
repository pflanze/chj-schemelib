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
	 show
         joo-introspection
         (cj-typed perhaps-typed.var
                   perhaps-typed.maybe-predicate)
         test
         test-random)

(export (class corescheme-var)
        (interface corescheme-interface
                   (interface corescheme-core)
                   (interface corescheme-extension))
        (class corescheme-extended
               (classes corescheme-literal
                        corescheme-ref
                        corescheme-lambda
                        corescheme-app
                        corescheme-def
                        corescheme-set!
                        (class corescheme-beginlike
                               (classes corescheme-begin
                                        corescheme-and
                                        corescheme-or))
                        corescheme-if
                        (class corescheme-letlike
                               (classes
                                corescheme-letrec
                                corescheme-let
                                corescheme-let*))))
	(method source.corescheme)
        (method corescheme.optimized?)
	make-scheme-env
        run-corescheme (macro RUN-CORESCHEME)
        
	#!optional
        corescheme:literal-atom?
	corescheme:literal?
        corescheme:flatten<T>
	current-corescheme-id
	corescheme-next-id!
	new-corescheme-var!
	corescheme-ctx?
	default-scheme-env)

"Core Scheme AST representation"


;; COPY from unmerged monad library:

(defmacro (mdo . args)
  `(RA >> ,@args))

(defmacro (>> a b)
  `(.>> ,a (lambda () ,b)))


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

  (defmethod (equal? a [corescheme-var? b])
    (and (= id (@corescheme-var.id b))
         (begin
           (assert (eq? name (corescheme-var.name b)))
           #t))))


(defparameter current-corescheme-id #f)
(defparameter current-optimizing? 'current-optimizing-is-unset)
;; dynamic typing, hey, :^) -- I don't know how else right now (I
;; still can't combine dot-oo/joo with defmodule yet.)
(defparameter current-corescheme? #f)
(def (corescheme? v)
     ((current-corescheme?) v))


;; "Monad runner" 
(def (run-corescheme thunk
                     #!key
                     optimizing?
                     (corescheme-id 0)
                     extended?)
     (parameterize ((current-corescheme-id corescheme-id)
                    (current-optimizing? optimizing?)
                    (current-corescheme? (if extended?
                                             corescheme-extended?
                                             corescheme-core?)))
                   (thunk)))
(defmacro (RUN-CORESCHEME expr . args)
  `(run-corescheme (lambda () ,expr) ,@args))



(definterface corescheme-interface
  (method (references? s [(list-of corescheme-var?) vars]) -> boolean?
          "whether s contains any references to any of the vars")
  (method (num-references s [corescheme-var? var]) -> natural0?
          "how many references to var s contains")

  (def corescheme:path? (pair-of corescheme-ref?
                                 (ilist-of corescheme?)))
  (def corescheme:path-stream? (istream-of corescheme:path?))
  (method (references s
                      [corescheme-var? var]
                      [corescheme:path? path]
                      [corescheme:path-stream? tail])
          -> corescheme:path-stream?
          "stream of 'paths' (list of nodes) where references to the
given var are found; the first item of the path is the found node")
  (method (interpolate s
                       [(list-of corescheme-var?) vars]
                       [(list-of corescheme?) exprs])
          -> corescheme?
          (assert (lengths-= vars exprs))
          "replace references to the given vars with the corresponding exprs.")

  (definterface corescheme-core
    "core elements; only these are used by source.corescheme")
  
  (definterface corescheme-extension
    "elements probably only for pretty-printing (but some 'normal'
optimizers can be useful on those, too, to optimize after
reconstruction work)"))


(compile-time

 (def (corescheme:def-constructor-expand*
       classname
       cs  ;; list of fields that contain an expr
       lcs ;; list of fields that contain lists of exprs
       mcs ;; list of fields that contain a maybe expr
       fieldnames)
      (let ((l (append (map (lambda (fn) `(corescheme.optimized? ,fn))
                            cs)
                       (map (lambda (fn) `(every corescheme.optimized? ,fn))
                            lcs)
                       (map (lambda (fn) `(if ,fn (corescheme.optimized? ,fn) #t))
                            mcs))))
        `(def (,classname ,@fieldnames)
              (let ((opt (current-optimizing?)))
                ,@(if* l
                       `((If opt (assert ,(if (length-> l 1)
                                              `(and ,@l)
                                              (first l)))))
                       '())
                (,(source.symbol-append '_ classname) opt ,@fieldnames)))))

 (def (corescheme:joo-type.var+pred-s t)
      -> (ilist-of (values-of (possibly-source-of symbol?)
                              (possibly-source-of any?)))
      (=> t
          .all-field-decls
          ((lambda-pair ((a decls))
                   (assert (source-equal? a `[boolean? optimized?]))
                   decls))
          (.map (lambda (decl)
                  ((on cj-desourcify values)
                   (perhaps-typed.var decl)
                   (or (perhaps-typed.maybe-predicate decl)
                       (error "need predicate")))))))

 (def corescheme:classname.var+pred-s
      (=>* class-name.joo-type
             corescheme:joo-type.var+pred-s))
 
 (def (corescheme:def-constructor-expand classname)
      (let* ((var+pred-s (corescheme:classname.var+pred-s classname))
             (cs (map fst (filter (lambda-values
                                   ((var pred))
                                   (equal? pred 'corescheme?))
                                  var+pred-s)))
             (lcs (map fst (filter (lambda-values
                                    ((var pred))
                                    ;; XXXX update, and  report if none matches!
                                    ;; (list-of/length corescheme? (length vars))
                                    (equal? pred '(list-of corescheme?)))
                                   var+pred-s)))
             (mcs (map fst (filter (lambda-values
                                    ((var pred))
                                    (equal? pred '(maybe corescheme?)))
                                   var+pred-s)))
             (fieldnames (map fst var+pred-s)))
        (corescheme:def-constructor-expand* classname
                                            cs
                                            lcs
                                            mcs
                                            fieldnames))))

(TEST
 > (corescheme:def-constructor-expand 'corescheme-letrec)
 (def (corescheme-letrec vars exprs body-expr)
      (let ((opt (current-optimizing?)))
        (If opt
            (assert (and (corescheme.optimized? body-expr)
                         (every corescheme.optimized? exprs))))
        (_corescheme-letrec opt vars exprs body-expr)))
 > (corescheme:def-constructor-expand 'corescheme-if)
 (def (corescheme-if test then else)
      (let ((opt (current-optimizing?)))
        (If opt
            (assert (and (corescheme.optimized? test)
                         (corescheme.optimized? then)
                         (if else (corescheme.optimized? else) #t))))
        (_corescheme-if opt test then else))))


(defmacro (corescheme:def-constructor classname)
  (corescheme:def-constructor-expand classname))



(defclass (corescheme-extended [boolean? optimized?])

  (defclass ((corescheme-literal _corescheme-literal)
             [corescheme:literal? val])
    implements: corescheme-core

    (defmethod (references? s vars) #f)
    (defmethod (num-references s var) 0)
    (defmethod (references s var path tail) tail)
    (defmethod (interpolate s vars exprs) s))

  (defclass ((corescheme-ref _corescheme-ref)
             [corescheme-var? var])
    implements: corescheme-core

    (defmethod (references? s vars)
      (any (C corescheme-var.equal? _ var) vars))
    (defmethod (num-references s var*)
      (if (corescheme-var.equal? var var*) 1 0))
    (defmethod (references s var* path tail)
      (delay (if (corescheme-var.equal? var var*)
                 (cons (cons s path) tail)
                 tail)))
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
    implements: corescheme-core

    (defmethod (references? s vars)
      (.references? expr vars))
    (defmethod (num-references S var)
      ;; possible optimization: stop analyzing if var's name is in one
      ;; of vars' name?
      (.num-references expr var))
    (defmethod (references s var path tail)
      (.references expr var (cons s path) tail))
    (defmethod (interpolate s vars* exprs*)
      (corescheme-lambda vars
                         (.interpolate expr vars* exprs*))))
       
  (defclass ((corescheme-app _corescheme-app)
             [corescheme? proc]
             [(list-of corescheme?) args])
    implements: corescheme-core

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
    (defmethod (references s var path tail)
      (stream-fold-right (let ((path* (cons s path)))
                           (lambda (expr tail)
                             (.references expr var path* tail)))
                         tail
                         (cons proc args)))
    (defmethod (interpolate s vars* exprs*)
      (corescheme-app (.interpolate proc vars* exprs*)
                      (map (C .interpolate _ vars* exprs*)
                           args))))
       
  (defclass ((corescheme-def _corescheme-def)
             [corescheme-var? var]
             [corescheme? val])
    implements: corescheme-core

    (defmethod (references? s vars)
      (.references? val vars))
    (defmethod (num-references s var*)
      (.num-references val var*))
    (defmethod (references s var path tail)
      (.references val var (cons s path) tail))
    (defmethod (interpolate s vars* exprs*)
      (corescheme-def var (.interpolate val vars* exprs*))))

  ;; XXX unify implementation with corescheme-def, too
  (defclass ((corescheme-set! _corescheme-set!)
             [corescheme-var? var]
             [corescheme? val])
    implements: corescheme-core

    (defmethod (references? s vars)
      (.references? val vars))
    (defmethod (num-references s var*)
      (.num-references val var*))
    (defmethod (references s var path tail)
      (.references val var (cons s path) tail))
    (defmethod (interpolate s vars* exprs*)
      (corescheme-set! var (.interpolate val vars* exprs*))))


  (defclass ((corescheme-beginlike
              ;; #f gives Unbound variable: let-corescheme-beginlike
              ;; XX grr didn't I fix this some time? 
              __corescheme-beginlike)
             [(list-of corescheme?) body])

    (defmethod (references? s vars)
      (any (C .references? _ vars) body))
    (defmethod (num-references s var)
      (fold (lambda (expr tot)
              (+ tot (.num-references expr var)))
            0
            body))
    ;; XX almost copy-paste with -app
    (defmethod (references s var path tail)
      (stream-fold-right (let ((path* (cons s path)))
                           (lambda (expr tail)
                             (.references expr var path* tail)))
                         tail
                         body))
    (defmethod (interpolate s vars* exprs*)
      ((.constructor s) (map (C .interpolate _ vars* exprs*)
                             body)))
     

    (defclass ((corescheme-begin _corescheme-begin))
      implements: corescheme-core
      (defmethod (constructor s) corescheme-begin))

    (defclass ((corescheme-and _corescheme-and))
      implements: corescheme-extension
      (defmethod (constructor s) corescheme-and))

    (defclass ((corescheme-or _corescheme-or))
      implements: corescheme-extension
      (defmethod (constructor s) corescheme-or)))
    
       
  (defclass ((corescheme-if _corescheme-if)
             [corescheme? test]
             [corescheme? then]
             ;; should the missing-else case be encoded as
             ;; explicit (void) ?
             [(maybe corescheme?) else])
    implements: corescheme-core

    (defmethod (references? s vars)
      (or (.references? test vars)
          (.references? then vars)
          (and else (.references? else vars))))
    (defmethod (num-references s var)
      (+ (.num-references test var)
         (.num-references then var)
         (if else (.num-references else var) 0)))
    (defmethod (references s var path tail)
      ;; could use =>> but, the |if| form kinda spoils it. Thus go
      ;; proper monads.
      (def ((>> a b) tail)
           (=> (a tail)
               ((b))))
      (let ((path* (cons s path)))
        (=> tail
            ((mdo (C .references test var path* _)
                  (C .references then var path* _)
                  (if else (C .references else var path* _) id))))))
    (defmethod (interpolate s vars* exprs*)
      (corescheme-if (.interpolate test vars* exprs*)
                     (.interpolate then vars* exprs*)
                     (and else (.interpolate else vars* exprs*)))))


  (defclass ((corescheme-letlike
              ;; #f see above
              __corescheme-letlike)
             [(list-of corescheme-var?) vars]
             [(list-of/length corescheme? (length vars)) exprs]
             ;; the nice part of an alist is that it doesn't require
             ;; this cross check ^. Also, this will be giving a |vars|
             ;; not bound warning from the generated code in the exprs
             ;; setter. (Q: should I make setters macros, or via
             ;; define-inline ? Or, more correctly, not generate
             ;; setters at all for fields which have cross
             ;; dependencies for value correctness.)
             [corescheme? body-expr])

    (defmethod (references? s vars)
      (or (any (C .references? _ vars) exprs)
          (.references? body-expr vars)))
    (defmethod (num-references s var)
      (fold (lambda (expr tot)
              (+ tot (.num-references expr var)))
            (.num-references body-expr var)
            exprs))
    (defmethod (references s var path tail)
      (.fold-right ç))
    (defmethod (interpolate s vars* exprs*)
      ((.constructor s)
       vars
       (map (C .interpolate _ vars* exprs*) exprs)
       (.interpolate body-expr vars* exprs*)))


    (defclass ((corescheme-letrec _corescheme-letrec))
      implements: corescheme-core
      (defmethod (constructor s) corescheme-letrec))

    (defclass ((corescheme-let _corescheme-let))
      implements: corescheme-extension
      (defmethod (constructor s) corescheme-let))

    (defclass ((corescheme-let* _corescheme-let*))
      implements: corescheme-extension
      (defmethod (constructor s) corescheme-let*))))


;; these can't be within the class definitions because of the
;; evaluation ordering
(corescheme:def-constructor corescheme-literal)
(corescheme:def-constructor corescheme-ref)
(corescheme:def-constructor corescheme-lambda)
(corescheme:def-constructor corescheme-app)
(corescheme:def-constructor corescheme-def)
(corescheme:def-constructor corescheme-set!)
(corescheme:def-constructor corescheme-begin)
(corescheme:def-constructor corescheme-if)
(corescheme:def-constructor corescheme-letrec)
(corescheme:def-constructor corescheme-let)
(corescheme:def-constructor corescheme-let*)
(corescheme:def-constructor corescheme-and)
(corescheme:def-constructor corescheme-or)

;; Should this (probably?) be done automatically by dot-oo/joo?  or
;; well now it would be corescheme-core.optimized?; avoid slow
;; dispatch on dynamic variable (current-corescheme?), and make it a
;; plain function now.
(def corescheme.optimized? corescheme-extended.optimized?)


(def (corescheme-next-id!)
     (parameter-inc! current-corescheme-id))

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
	(values (RUN-CORESCHEME (_source->corescheme:begin v (get-env) #t))
		(RUN-CORESCHEME (_source->corescheme:begin v (get-env) #f))))
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
		    (source-error expr "undefined variable"
                                  (source-code expr)))))

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
                             "undefined variable in function position"
                             a*))))

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
  ;; -> corescheme?  can't use outside RUN-CORESCHEME, hence:
  -> corescheme-extended?

  (let ((actual-get-ctx (if globals
                            (C make-scheme-env globals)
                            get-ctx)))
    (fst (RUN-CORESCHEME
          (_source->corescheme expr (actual-get-ctx) realmode?)))))


(def (corescheme:flatten<T> T? T body)
     (let ((body* (fold-right (lambda (e r)
                                (if (T? e)
                                    (append (.body e) r)
                                    (cons e r)))
                              '()
                              body)))
       (if (one-item? body*)
           (first body*)
           (T body*))))


;; ------------------------------------------------------------------
;; Testing

(def list.string list->string);; WHYnot have???XX

(def (random-nonnull-alpha-list)
     (let lp ((res '()))
       (if (and (pair? res) (zero? (random-integer 5)))
           res
           (lp (cons (integer.char (+ (char.integer #\a) (random-integer 26)))
                     res)))))

(def (random-nonnull-alpha-string)
     (=> (random-nonnull-alpha-list) list.string))

(def (random-variable-name)
     (=> (random-nonnull-alpha-string) 
         string.symbol))


;;XX ctx

;;XX where should level be incremented  uniformly ?

;;XX separate into create vs. use
;; corescheme-var is not part of corescheme hierarchy:
(def (random-corescheme-var level)
     (new-corescheme-var! (random-variable-name)))


(def (random-corescheme:literal level)
     ;; for now, simply:
     (random-integer 1000))

;;XX a heh  of same length   thing  for app etc--but then far anyway not working. need static typing for that. type ctx? dynamic as well tho r 
(def (corescheme:random-list-of construct level)
     (let rec ()
       (if (zero? (random-integer 3))
           '()
           (cons (construct level) (rec)))))

(def (corescheme:random-list-of/length construct [natural0? len] level)
     (let rec ((len len))
       (if (zero? len)
           '()
           (cons (construct level) (rec (dec len))))))

;; XX actually make improper, but then have to finish parsing
(def corescheme:random-improper-list-of corescheme:random-list-of)

(def (corescheme:random-maybe construct level)
     (if (zero? (random-integer 2))
         (construct level)
         #f))

(compile-time
 (def (corescheme:random-code)
      (let* ((joo-types (=> 'corescheme-interface
                            class-name.joo-type
                            joo-type.all-subclasses
                            (.map class-name.joo-type)
                            (.filter (complement .interface?))))
             (randomconstructor-name
              (lambda (classname)
                (symbol-append 'random- classname)))
             (randomconstructor-name/?
              (lambda (predicatename)
                (=> (symbol.string predicatename)
                    .butlast
                    string.symbol
                    randomconstructor-name))))
        (quasiquote-source
         (begin
           ,@(.map
              joo-types
              (lambda (t)
                (=>
                 t
                 corescheme:joo-type.var+pred-s
                 ((lambda (var+pred-s)
                    (let ((classname (.class-name t)))
                      (quasiquote-source
                       (def (,(randomconstructor-name classname) level)
                            (let* ((level* (inc level))
                                   ,@(.map
                                      var+pred-s
                                      (lambda-values
                                       ((var pred))
                                       (quasiquote-source
                                        (,var
                                         ,(mcase
                                           pred
                                           (`(`<container>-of `T? . `args)
                                            (quasiquote-source
                                             (,(symbol-append 'corescheme:random-
                                                              <container>-of)
                                              ,(randomconstructor-name/? T?)
                                              ,@args
                                              level*)))
                                           (symbol?
                                            (quasiquote-source
                                             (,(randomconstructor-name/? pred)
                                              level)))))))))
                              (,classname
                               ,@(.map var+pred-s fst)))))))))))
           (def (random-corescheme level)
                (xcase (random-integer ,(length joo-types))
                       ,@(.map/iota joo-types
                                    (lambda (t i)
                                      (quasiquote-source
                                       ((,i)
                                        (,(randomconstructor-name
                                           (.class-name t))
                                         level))))))))))))

(insert-result-of (corescheme:random-code))



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


;; .references
(TEST
 > (.show (F (RUN-CORESCHEME (.references (corescheme-ref
                                           (corescheme-var 'foo 1))
                                          (corescheme-var 'foo 1)
                                          '(a)
                                          '()))))
 (list (list (_corescheme-ref #f (corescheme-var 'foo 1)) 'a))
 > (.show (F (RUN-CORESCHEME (.references (corescheme-begin
                                           (list (corescheme-ref
                                                  (corescheme-var 'foo 1))))
                                          (corescheme-var 'foo 1)
                                          '()
                                          '()))))
 (list (list (_corescheme-ref #f (corescheme-var 'foo 1))
             (_corescheme-begin
              #f (list (_corescheme-ref #f (corescheme-var 'foo 1))))))

 ;; generative tests? Could do if keeping random decisions (e.g.
 ;; generating the code and the expected results from the same
 ;; decisions).
 )
