;;; Copyright 2013-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require cj-env
	 (fixnum inc)
	 define-macro-star
	 cj-typed ;; heh indirectly through define-struct. expansion
	 cj-match
	 cj-warn
	 test
	 (cj-source-wraps source:symbol-append)
	 (string-util strings-join)
	 (string-util-2 string-split-once)
	 (cj-env-2 for..<)
	 (cj-test %try)
	 (cj-struct define-struct-expand)
	 C
	 (list-util-lazy xone)
	 slib-sort ;; for show-method-statistics
	 )

(export (macro method-table-for)
	(macro show-methods)
	(macro define.)
	(macro define-struct.)
	(macro CALL.)
	nothing? ;; really?

	;; XX should move?
	(generic list.ref)

	show-generics-list
	show-method-statistics
	
	#!optional
	define-struct.-expand
	(generic .typecheck!) ;; ?
	(variable *dot-oo:method-trace*)
	)

(include "cj-standarddeclares.scm")

;; include method lookup table implementation
(include "dot-oo--include.scm")
;; Getting:
;; (    dot-oo:method-key-maybe-ref-i
;; 	dot-oo:method-type-maybe-ref-method
;; 	dot-oo:method-table-set!
;; 	dot-oo:new-method-table)



;; (XX lost the string-split that allowed to give me limits on the
;; number of result values. Working around using strings-join.)

(both-times
 (define (dot-oo:split-typename.methodname str #!optional src)
   (let ((typename+maybe-methodname
	  (string-split-once (source-code str) #\. #f)))

     (cond ((snd typename+maybe-methodname)
	    => (lambda (methodname)
		 (if (string=? methodname ".")
		     (source-error src "missing method name after '.' in name")
		     typename+maybe-methodname)))
	   (else
	    (source-error src "missing '.' in name")))))
 
 ;; The full name
 ;; 
 ;;   Vb:Vr.map
 ;;
 ;; yields the generic name
 ;; 
 ;;   Vb:.map
 ;;
 ;; with its own method table (one method table is only to contain the
 ;; type checks and methods for one hole, the first argument; not the
 ;; return type; return type yields a different generic name).

 (define (dot-oo:split-prefix:typename.methodname str #!optional src)
   (define (cont prefix remainder)
     (letv ((typename methodname)
	    (dot-oo:split-typename.methodname remainder src))
	   (values typename
		   (string-append prefix methodname))))
   (let* ((parts (string-split (source-code str) #\:)))
     (if (= (length parts) 1)
	 (cont "" str)
	 (cont (string-append (car parts) ":")
	       (possibly-sourcify
		(strings-join (cdr parts) ":") str))))))

(TEST
 > (values->vector (dot-oo:split-typename.methodname "foo.bar"))
 #("foo" ".bar")
 > (values->vector (dot-oo:split-typename.methodname "foo.bar.baz"))
 #("foo" ".bar.baz")
 > (values->vector (dot-oo:split-typename.methodname "fix:foo.bar.baz"))
 #("fix:foo" ".bar.baz")

 > (values->vector (dot-oo:split-prefix:typename.methodname
		    "fix:foo.bar.baz:boo"))
 #("foo" "fix:.bar.baz:boo")
 > (values->vector (dot-oo:split-prefix:typename.methodname
		    "foo.bar.baz"))
 #("foo" ".bar.baz")
 ;; XX I'm not testing "foo.bar.baz:boo".. what should it do then?...
 > (map (lambda (name)
	  (with-exception-catcher
	   source-error-message
	   (& (snd (dot-oo:split-prefix:typename.methodname name)))))
	'("foo" "foo." "foo.bar"))
 ("missing '.' in name" "missing method name after '.' in name" ".bar"))


(define dot-oo:genericname->method-table (make-table))


(define (dot-oo:generic-error genericname obj)
  (error (string-append "no method found for generic "
			(object->string genericname)
			" for value:")
	 obj))

(define (dot-oo:make-generic genericname method-table)
  (table-set! dot-oo:genericname->method-table
	      genericname method-table)
  (lambda (obj . rest)
    (cond ((dot-oo:method-table-maybe-ref-method method-table obj)
	   => (lambda (method)
		(apply method obj rest)))
	  (else
	   (dot-oo:generic-error genericname obj)))))

;; optimization to avoid allocation of rest arguments (alternatively
;; also see dot-oo-optim.scm):
(define-macro* (CALL. genericname obj . args)
  (assert*
   symbol? genericname
   (lambda (genericname)
     (with-gensyms
      (OBJ METHOD)
      `(let ((,OBJ ,obj))
	 (cond ((dot-oo:method-table-maybe-ref-method
		 ,(generic-name-string.method-table-name
		   (symbol->string genericname))
		 ,OBJ)
		=> (lambda (,METHOD)
		     (,METHOD ,OBJ ,@args)))
	       (else
		(dot-oo:generic-error ',genericname ,OBJ))))))))



(both-times
 (define (generic-name-string.method-table-name str)
   (string->symbol (string-append "dot-oo-method-table#"
				  str)))
 
 (define define.-expand
   (lambda (name expr)
     (let ((namestr (possibly-sourcify (symbol->string (source-code name))
				       name)))
       (letv ((typenamestr genericnamestr)
	      (dot-oo:split-prefix:typename.methodname namestr name))
	     (let* ((genericname (string->symbol genericnamestr))
		    (typename (string->symbol typenamestr))
		    (predicate (source:symbol-append typename '?))
		    ;; But predicate can change through redefinitions
		    ;; on reload, use typename for table updates
		    ;; instead.
		    (fulltypename (string->symbol (string-append typenamestr)))
		    (method-table-name (generic-name-string.method-table-name
					genericnamestr)))
	       `(begin
		  (define ,name ,expr)
		  ;; Update (and possibly create) method table
		  (define ,method-table-name
		    (dot-oo:method-table-set!
		     (macro-symbol-value-or ,method-table-name
					    dot-oo:new-method-table)
		     ',typename
		     ;; wrapper (1) to pick up definitions later in
		     ;; scope (gah, perhaps solvable?), (2) to pick up
		     ;; on redefinitions? Well, not sure whether that
		     ;; should happen. XX
		     (lambda (v) (,predicate v))
		     ,name))

		  ;; don't use |set!| since it leads to "Ill-placed 'define'"s:
		  (define-if-not-defined ,genericname
		    (dot-oo:make-generic ',genericname ,method-table-name)))))))))


(define-macro* (method-table-for generic-name-sym)
  (generic-name-string.method-table-name
   (symbol->string (source-code generic-name-sym))))

(define-macro* (show-methods generic-name-sym)
  `(dot-oo:show-method-table (method-table-for ,generic-name-sym)))


(define (show-generics-list)
  (sort (map car (table->list dot-oo:genericname->method-table))
	(on symbol->string string<?)))


(define (show-method-statistics)
  (define (stat-count l) (list-ref l 3))
  (sort (filter
	 (lambda (entry)
	   (not (zero? (car entry))))
	 (map (lambda (genericname.method-table)
		(let* ((tableshown (dot-oo:show-method-table
				    (cdr genericname.method-table)))
		       (tot (apply + (map stat-count tableshown))))
		  (list tot
			(car genericname.method-table)
			(sort tableshown (on stat-count <)))))
	      (table->list dot-oo:genericname->method-table)))
	(on car <)))


(define-macro* (define. first . rest)
  (mcase first
	 (symbol?
	  (define.-expand first (xone rest)))
	 (pair?
	  (let ((first* (source-code first)))
	    (define.-expand (car first*)
	      `(typed-lambda ,(cdr first*)
		 ,@rest))))))

;; [*] Note: this now resolves the type predicate at run time, so that
;; later redefinitions of other modules are respected[, and so that
;; more-oo can work (without a hack that broke compilation).--Now
;; more-oo is in its own namespace, so, ok? XX look into this some
;; time.]

;; Redefinitions of the same method by using define. in the running
;; system will keep the previous definition chained in the
;; generic. (They are now not retaining the old type predicate
;; anymore, so they'll check for the same updated predicates as the
;; newest method definitions.)  Apart from the type checking cost upon
;; failures or types that aren't reloaded (and memory usage) this
;; won't have any effect though.


;; where should these be moved to?
(define. (list.ref x y) (list-ref x y))

(TEST
 ;; why is this defined but can't find the source code? Aha in
 ;; oo-vector-lib.scm.
 > (define. (string.ref x y) (string-ref x y))

 ;; > (%try-error (.ref "foo" 1))
 ;; #(error "no method found for generic .ref for value:" "foo")
 ;; well, if this test suite is run again, it will find one.
 > (.ref "foo" 1)
 #\o
 > (.ref '(a b c) 1)
 b
 )



;; omit the |make-| prefix for the constructor name; use "." as
;; separator, and use |define.| to define all methods so that they
;; become part of generic super functions. Also, allow field typing.

(define define-struct/types:arg->maybe-fieldname
  ;; XX sigh, almost-copy-paste of define-struct:arg->maybe-fieldname
  (named self
	 (lambda (v*)
	   (let ((v (source-code v*)))
	     (cond ((symbol? v)
		    v*)
		   ((typed? v)
		    (@typed.var v))
		   ((meta-object? v)
		    #f)
		   (((list-of-length 2) v)
		    ;; `(`definition `default-value)
		    (self (car v)))
		   (else
		    (source-error
		     v*
		     "expecting symbol or typed symbol or meta-object")))))))


(both-times
 (define dot.oo:have-no-typecheck
   '(symbol? fixnum? string? boolean? number? integer? complex?)))

(define (define-struct.-expand constructor-stx name defs)
  (with-gensyms
   (V V*)
   `(begin
      ,(apply define-struct-expand
	      'define.
	      'typed-lambda
	      'detyped-lambda
	      define-struct/types:arg->maybe-fieldname
	      (lambda (var field+)
		(let ((field+* (source-code field+)))
		  (if (typed? field+*)
		      (begin
			(assert (= (vector-length field+*) 2))
			(vector (vector-ref field+* 0)
				var))
		      var)))
	      (lambda (FN field+)
		(let ((field+* (source-code field+)))
		  (if (typed? field+*)
		      (begin
			(assert (= (vector-length field+*) 2))
			(with-gensyms
			 (V V*)
			 `(lambda (,V)
			    (let ((,V* (,FN ,V)))
			      ;; (XX btw much code duplication? (of the
			      ;; type check code, in case it is big))
			      (type-check ,(vector-ref field+* 0) ,V*
					  ,V*)))))
		      FN)))
	      name
	      separator: "."
	      constructor-stx: constructor-stx

	      ;; don't override constructor-name or
	      ;; unsafe-constructor-name if provided by user
	      (let ((defs* (map source-code defs)))
		`(,@(if (memq constructor-name: defs*)
			`()
			`(constructor-name: ,name))
		  ,@(if (memq unsafe-constructor-name: defs*)
			`()
			`(unsafe-constructor-name:
			  ,(source:symbol-append name '@)))
		  ,@defs)))
      
      ;; reserve name for just this purpose?
      (define. (,(source:symbol-append name ".typecheck!") ,V)
	,@(filter values
		  (map (lambda (def)
			 (let ((def* (source-code def)))
			   (if (typed? def*)
			       `(let ((,V*
				       ;; use accessors, or direct vector-ref? :
				       (,(source:symbol-append name "." (vector-ref def* 1)) ,V)))
				  (type-check ,(vector-ref def* 0)
					      ,V*
					      ;; stupid (begin) in if not allowed:
					      (void))
				  ;; recursion
				  ,@(if (memq (source-code (vector-ref def* 0))
					      dot.oo:have-no-typecheck)
					`()
					`((.typecheck! ,V*))))
			       ;; NOTE: doen't recurse for fields
			       ;; without type
			       ;; declaration. Hm. (Because I would
			       ;; still need typecheck! methods for
			       ;; basic types then; and more over,
			       ;; maybe some vector in those fields
			       ;; with symbol head is just a vector
			       ;; with symbol head?)
			       #f)))
		       defs))
	,V))))

(define-macro* (define-struct. name . defs)
  (define-struct.-expand stx name defs))


;; need this often enough I guess:
(define nothing? not)
(define. nothing.typecheck! identity)

;; hmm. inefficiency worries except doesn't matter for typecheck; but
;; also ugly? (But wouldn't special-casing list-of in
;; define-struct. be ugler?)
(define. (list.typecheck! l)
  (for-each .typecheck! l)
  l)

;; hm even special-casing doesn't help in all cases, thus:
(define (void/1 x) (void))
(define. boolean.typecheck! identity)
(define. number.typecheck! identity) ;; heh includes fixnum etc. of course
(define. symbol.typecheck! identity)
(define. string.typecheck! identity)


(TEST
 > (define-struct. foo #(fixnum? x))
 > (%try-error (foo 'a))
 #(error "x does not match fixnum?:" a)
 > (foo 10)
 #((foo) 10)
 > (.x #)
 10
 > (define-struct. foo #!key #(integer? x) #(boolean? b))
 > (foo x: 10 b: #t)
 #((foo) 10 #t)
 > (foo b: #t x: 10)
 #((foo) 10 #t)
 > (.x-set # 12)
 #((foo) 12 #t)
 > (%try-error (.x-set # 'n))
 #(error "value does not match integer?:" n)
 > (.x-update (foo x: 10 b: #t) inc-function)
 #((foo) 11 #t)
 > (%try-error (.x-update (foo x: 10 b: #t) true/1))
 #(error "gensym '\"V*\" does not match integer?:" #t)
 > (%try-error (foo b: 11 x: 10))
 #(error "b does not match boolean?:" 11)
 ;; > (define-struct. foo #(integer? x) #!optional (b #t))
 ;; "expecting symbol or typed symbol or meta-object" XX hmm, still not complete.

 ;; typecheck feature
 ;; (Now need to acess cj-struct:tag: variables; otherwise .typecheck!
 ;; would not match. Hacky?)
 > (define-struct. foo #!key x #(boolean? b))
 > (.typecheck! `#(,cj-struct:tag:foo #(,cj-struct:tag:foo 10 #f) #f))
 > (%try-error (.typecheck! `#(,cj-struct:tag:foo #(,cj-struct:tag:foo 10 #f) 12)))
 #(error "gensym '\"V*\" does not match boolean?:" 12)
 ;; > (%try-error (.typecheck! '#(foo #(foo 10 11) #f)))
 ;; was hoping for an error here. But, see NOTE in the source.
 > (define-struct. foo #!key #((either foo? nothing?) x) #(boolean? b))
 > (.typecheck! `#(,cj-struct:tag:foo #(,cj-struct:tag:foo #f #f) #f))
 > (%try-error (.typecheck! `#(,cj-struct:tag:foo #(,cj-struct:tag:foo #f 11) #f)))
 #(error "gensym '\"V*\" does not match boolean?:" 11)
 )

