;;; Copyright 2016-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Java style object orientation: extensible hierarchy,
;; single-inheritance of both methods and fields.

;; Still based on cj-struct (i.e. purely functional).

;; Relies on symbols as unique names for classes (so that symboltable
;; can be used, sensible?), so will be incompatible with defmodule !
;; XX aha, could actually work with symboltable, still, by putting the
;; full struct tag into the value position and comparing it. Wow
;; actually this way I can avoid using the slow Gambit hashtable in
;; cj-struct!! So, actually we can still be
;; safe. Interesting. Probably still won't work with defmodule anyway
;; but let's see. Ah it will break if the same name is reused for a
;; different type and both types are part of the same class hierarchy;
;; "could this ever happen?".  AH, cj-struct predicates don't go
;; through the hashtable actually man. man.

(require easy-1
	 more-oo ;; just for bootstrapping reasons.
	 (srfi-1 cons* take drop)
	 ;; ^ btw so many times, fold etc., should this be with easy?
	 (cj-struct struct-tag.name)
	 (dot-oo define-struct.-expand)
	 symboltable
	 (symboltable symboltable-declare @symboltable-ref-inline)
	 (code-macro-expand macro-expander/symtbl begin-flatten)
	 (cj-source source-quote source-dequote location?)
	 cj-seen
	 (improper-list improper-list->list)
	 (tree-util flatten)
	 (cj-typed args-detype)
	 cj-source-quasiquote ;; all in the name of optimization
	 debuggable-promise
	 cj-functional
	 (cj-gambit-sys-0 @vector-ref)
	 test)

(export (macro joo-class)
	(macro joo-interface)
	(macro def.*) ;; XX should I provide |define.*|, too?
        (macro with.)
	#!optional
	joo:parse-decl
        joo:class-name.joo-type
        joo:class-name.all-field-names
        joo:class-name.all-field-decls
        joo:class-name.field-decls)


(include "cj-standarddeclares.scm")
(possibly-use-debuggable-promise)


;; inline a local copy
(define symboltable? (symboltable?-lambda))



;; We define a new |method| syntax, but it is locally scoped, not
;; exported (here, this time, unlike more-oo which has one definition
;; for method and then takes info from the context, and you see
;; already how that was a bad idea, hm?), I mean, ok?  I.e. its
;; definition is exported with |jooclass|.

;; Look for symbols of the same name as the field names, if found *and
;; not in method argument list* then copy the corresponding field from
;; the object using the let-foo thing. Could be better by properly
;; analyzing the code but defer that to when I have proper
;; macros.... (Although OTOH by then perhaps the compiler is also
;; smarter and will elide unused vector-ref's (and even better move
;; used ones to better places).)

(def (joo:body-symbols body) ;; -> (list-of symbol?)
     (filter symbol? (flatten (cj-desourcify body))))

(TEST
 > (joo:body-symbols '(foo (a bar 1 let) #(pair? x)))
 (foo a bar let))


(def (joo:args->vars args)
     (map (lambda (bind)
	    (let ((bind* (source-code bind)))
	      (if (pair? bind*) (car bind*)
		  bind)))
	  (filter (comp-function (either pair? symbol?) source-code)
		  (improper-list->list (args-detype args)))))

(TEST
 > (joo:args->vars '(a #(vector? b) . #(pair? c)))
 (a b c)
 > (joo:args->vars '(a #(vector? b) #!optional #(pair? c)))
 (a b c)
 > (joo:args->vars '(a #(vector? b) #!key (#(pair? c) (cons 1 2))))
 (a b c)
 > (joo:args->vars '(row-stream
		     s
		     #((list-of symbol?) index-names)
		     #(boolean? reverse?)
		     #!optional
		     (tail '())))
 (row-stream s index-names reverse? tail)
 > (cj-desourcify
    (joo:args->vars (quote-source
		     (row-stream
		      s
		      #((list-of symbol?) index-names)
		      #(boolean? reverse?)
		      #!optional
		      (tail '())))))
 (row-stream s index-names reverse? tail))



;; for |def-method|, |def.*| and |with.|: make fields visible in the
;; body
(def (joo:body* stx
                [symbol? class-name]
		[(list-of (source-of symbol?)) fields]
		bind
                ;; ^ raw method arguments; maybe should separate first
                ;; argument (used in `(car bindvars) below) so that it
                ;; doesn't have to be a symbol, for |with.|
		rest
		;; ^ body plus possibly -> from method definition or lambda
		)
     ;; -> list? -- why? so it is compatible with body itself, right
     (let* ((used (list->symbolcollection (joo:body-symbols rest)))
	    (bindvars (joo:args->vars bind))
	    (bind-used (list->symbolcollection (map source-code bindvars))))

       (if (null? bindvars)
           (source-error
            stx "missing method arguments (need at least self argument)")
           (letv ((pre body)
                  ;; Also have to parse rest for "->"
                  ;; syntax *UH*, that's bad? Well the ->
                  ;; is part of the binds thing,
                  ;; basically, but still, XX should
                  ;; really provide proper parsers!
                  ;; (S-expr *is* just a layer in the
                  ;; syntax parsing. Really.)
                  (if (and (pair? rest)
                           (eq? (source-code (car rest)) '->)
                           (pair? (cdr rest)))
                      (values (take rest 2)
                              (drop rest 2))
                      (values '()
                              rest)))
                 `(,@pre
                   (,(symbol-append 'let- class-name)
                    (,(map (lambda (nam)
                             (let ((nam* (source-code nam)))
                               (if (and (symboltable-ref used nam* #f)
                                        (not
                                         (symboltable-ref bind-used nam* #f)))
                                   nam
                                   '_)))
                           fields)
                     ,(car bindvars))
                    ,@body))))))


(def (joo:class-name.joo-type class-name)
     ;; XX avoid need for eval?; should still be safe though as
     ;; joo:joo-type-symbol prefixes it (although not Gambit
     ;; namespaces, todo change?)
     (eval (joo:joo-type-symbol class-name)))
(def (joo:class-name.all-field-names class-name)
     (joo-type.all-field-names (joo:class-name.joo-type class-name)))
(def (joo:class-name.all-field-decls class-name)
     (joo-type.all-field-decls (joo:class-name.joo-type class-name)))
(def (joo:class-name.field-decls class-name)
     (joo-type.field-decls (joo:class-name.joo-type class-name)))


;; XX now that def-method* has been renamed to def-method, should
;; probably rename this, too, but then it needs safe detection if
;; class-name is a class or not (just see whether the eval fails?
;; Sigh.)

(def (joo:source-error-len-1 stx)
     (source-error stx (string-append
			"bare symbol given, but remainder "
			" after it is not of length 1")))

(defmacro (def.* bind . rest)

  (def (expand class-name.method binds body)
       (letv ((class-name-str _method)
	      (dot-oo:split-prefix:typename.methodname
	       (symbol.string (source-code class-name.method))))

	     (let ((class-name (string.symbol class-name-str)))
	       `(def. (,class-name.method ,@binds)
		  ,@(joo:body* stx
                               class-name
			       (joo:class-name.all-field-names class-name)
			       binds
			       body)))))
  
  (mcase bind
	 (pair?
	  (let-pair ((class-name.method binds) (source-code bind))
		    (expand class-name.method binds rest)))
	 (symbol?
	  ;; partial COPY PASTE from the lambda expansion code in
	  ;; joo:implementation-method-expander-for
	  (if (length-= rest 1)
	      ;; XX should macro-expand that form before checking for
	      ;; lambda, ditto for the copy in
	      ;; joo:implementation-method-expander-for
	      (mcase (car rest)
		     (`(lambda `binds . `body)
		      (expand bind binds body)))
	      (joo:source-error-len-1 stx)))))


;; a macro to get at joo:body* in other contexts
(defmacro (with. class-name val . body)
  (assert* symbol? class-name
           (lambda (class-name)
             (early-bind-expressions
              ;; needed for no good reason except to make joo:body*
              ;; happy
              (val)
              (the (joo:body* stx
                              class-name
                              (joo:class-name.all-field-names class-name)
                              (list val)
                              body))))))


;; def-method- and def-method
(def (joo:implementation-method-expander-for
      class-name
      maybe-fields
      ;; ^ #f if no visibility of fields is desired
      ;; (i.e. |def-method-|)
      abstract?)

     ;; maybe-fields is true (and the list of all fields (including
     ;; those of parent classes) in order) if object fields should be
     ;; visible as same-named variables (they are currently copied,
     ;; though, not aliased).
     (def class-name. (symbol-append class-name "."))

     (lambda (stx)
       (cj-sourcify-deep
	(mcase
	 stx
	 (`(`METHOD `bind . `rest)

	  (def (expand-with-bindings name args body)
	       `(def. (,(source.symbol-append class-name. name)
		       ,@args)
		  ,@(if (and maybe-fields
			     ;; only non-abstract classes have let-
			     ;; forms
			     (not abstract?))
			(joo:body* stx
                                   class-name
                                   maybe-fields
                                   args
                                   body)
			body)))

	  (mcase
	   bind
	   (pair?
	    (let-pair ((name args) (source-code bind))
		      (expand-with-bindings name args rest)))
	   (symbol?

	    (def (expand-without-bindings)
		 `(def. ,(source.symbol-append class-name. bind)
		    ,@rest))

	    (if maybe-fields
		;; check if it's a lambda form, if so do the transformation
		;; anyway, OK?  XX should eliminate COPY PASTE in
		;; def.*, including adding the missing macro-expand
		;; step.
		(if (length-= rest 1)
		    (mcase (car rest)
			   (`(lambda `binds . `body)
			    (expand-with-bindings bind binds body))
			   (else
			    (expand-without-bindings)))
		    (joo:source-error-len-1 stx))

		;; no fields given, i.e. |def-method-|
		(expand-without-bindings))))))
	stx)))

(def (joo:abstract-method-expander-for class-name)
     ;; (Future: could instead parse from joo-interface form, store
     ;; and use for checking in joo-class forms, whatever.)
     (lambda (stx)
       (cj-possibly-sourcify-deep
	`(begin)
	stx)))

(def joo:abstract-method-expander-forbidden
     (lambda (stx)
       (source-error stx "abstract method not allowed in non-abstract class")))

(def joo:implementation-method-expander-forbidden
     (lambda (stx)
       (source-error stx "method implementation not allowed in interface")))


;; How to check for (eq? and, nah, actually only, although including
;; eq? at the same time) is-a membership: query a symboltable for the
;; type name (symbol), then check the value to be identical to the
;; full struct tag. (Future: have a list of all types with the same
;; symbolname at the top? But now those symboltable bindings are
;; let-scoped (i.e. define-module compatible?), perhaps don't do that
;; here? or, linked-list-link from the previous one on the toplevel ?
;; (Wat xactly?))


(symboltable-declare)

(def (joo:struct-tag.member-of? t members)
     (declare (not safe)) ;; <-- XX safe?
     (let ((tag-name (@maybe-struct-tag-name t)))
       (and tag-name
	    ;; XX did I go to the dark side by using unsafe op here?
	    ;; -- also, assumes symboltable-1.scm is compiled.
	    (let ((tag (@symboltable-ref-inline members
						tag-name
						#f)))
	      (and tag
		   (eq? t tag))))))


;; using more-oo here as infrastructure for our coding, but not for
;; building the actual oo system on top of it

(##namespace ("more-oo#" class subclass struct method))

(class
 joo-type
 ;; With metadata; still call it joo-metadata instead?  Those
 ;; are mutable, and singletons, please!
 (struct [symbol? class-name]
	 ;;^ XX should rename that to name ? (is for interfaces, too)
	 [(maybe location?) maybe-location]
	 ;; location of the class definition form (if any)
	 [(maybe symbol?) maybe-constructor-name]
	 ;; #f means no constructor
	 [(maybe struct-tag?) maybe-struct-tag]
	 ;; #f means no cj-struct (i.e. same as above)
	 [boolean? interface?]
	 [ ;;(if interface? false? (maybe joo-type?))  sigh, I
	  ;; remembered right afterwards: doesn't work in the
	  ;; code used for setters; fix this in cj-struct?
	  ;; (~ugh, and heh, C++/Java style object field
	  ;; accesses, you know, heh)
	  (maybe joo-type?) maybe-parent]
	 ;; #f means this is joo-object
	 [(list-of joo-interface-type?) implements]
	 [list? field-decls]
	 ;; of source-code (never contains constructor-name:)
	 [symboltable? members]
	 ;; mutable; includes self
	 )

 ;; XX keep in sync with above and also joo_type_members
 (method (members-set! s [symboltable? members])
	 (vector-set! s 9 members))

 (def (joo-interface-type? s)
      (and (joo-type? s)
	   (joo-type.interface? s)))

 (def (joo-class-type? s)
      (and (joo-type? s)
	   (not (joo-type.interface? s))))

 ;; both `extends` and `implements` parents
 (method (all-immediate-parents s)
	 (let ((i (joo-type.implements s)))
	   (cond ((joo-type.maybe-parent s)
		  => (lambda (p)
		       (cons p i)))
		 (else i))))

 ;; excluding self
 (method (all-parents s)
	 (let ((seen?! (make-seen?! test: eq?)))
	   (let rec ((s s)
		     (res '()))
	     (if (seen?! s)
		 res
		 (cons s (fold-right rec
				     res
				     (joo-type.all-immediate-parents s)))))))

 (method (struct-tag s)
	 (or (joo-type.maybe-struct-tag s)
	     (error "this joo class does not have instances:"
		    (.class-name s))))

 (method (members-perhaps-add! s t)
	 ;; but only if it's possible that this class has
	 ;; instances
	 (cond ((joo-type.maybe-struct-tag t)
		=> (lambda (tag)
		     (joo-type.members-set!
		      s
		      ;; (Why not symboltable-add here: calling repeatedly
		      ;; when already inserted, right? ~Why is it all so
		      ;; unnice.) XX aha, optimization: could stop adding
		      ;; once already there
		      (symboltable-set (joo-type.members s)
				       (struct-tag.name tag)
				       tag))))))

 (method (update-parents! s) -> void? ;; just with itself
	 (for-each (lambda (p)
		     (joo-type.members-perhaps-add! p s))
		   (joo-type.all-parents s)))

 ;; build one decl that encompasses all the field declarations
 ;; of parents and ourselves; take care of #!key etc. so that
 ;; the new decl is valid DSSSL syntax. Ah, actually don't
 ;; especially take care of it, just have the user understand
 ;; that DSSSL syntax *continues across subclassing* ? !
 (method (all-field-decls s)
	 (append (cond ((joo-type.maybe-parent s)
			=> (lambda (p)
			     (joo-type.all-field-decls p)))
		       (else
			'()))
		 ;; XX ah oh btw, no rest arguments and
		 ;; similar allowed! or how to deal with those?
		 ;; ! Oh, not even key arguments, messes
		 ;; everything up. For now. (Would have to
		 ;; write a proper abstraction, parser.)
		 (joo-type.field-decls s)))

 (method (all-field-names s)
	 (joo:args->vars (joo-type.all-field-decls s)))

 ;; is `s` a `t` ?
 (method (is-a? s [joo-type? t])
	 (joo:struct-tag.member-of? (joo-type.struct-tag s)
				    (joo-type.members t)))

 ;; but, usually have to check an instance; hmm, provide method
 ;; in joo-object class, that everybody inherits? but circular,
 ;; no? Thus instead revert order of arguments:
 ;; XX better name? has-instance, no, rev-is-a, gah
 (method (covers-instance? s v) ;; flip of joo:instance.member-of? 
	 (declare (block)
		  (standard-bindings)
		  (extended-bindings)
		  (not safe))
	 (and (##vector? v)
	      ;; do *not* restrict length
	      (fx>= (##vector-length v) 1)
	      (joo:struct-tag.member-of? (@vector-ref v 0)
					 (@joo-type.members s)))))

;; end of more-oo.
(##namespace ("" class subclass struct method))


;; a constructor that also updates the parent's member tables (XX
;; *could* also ensure singletons here (keyed on tag), should I ?)
(def (make-joo-type! [symbol? class-name]
		     [(maybe location?) maybe-location]
		     [(maybe symbol?) constructor-name]
		     [(maybe struct-tag?) tag]
		     [boolean? interface?]
		     [(maybe joo-type?) parent]
		     [(list-of joo-type?) implements]
		     [list? field-decls])
     (when tag
           (assert (eq? (struct-tag.name tag) class-name)))
     (let ((t (joo-type class-name
			maybe-location
			constructor-name
			tag
			interface?
			parent
			implements
			field-decls
			(list->symboltable (list (cons class-name tag))))))
       (joo-type.update-parents! t)
       t))


(TEST
 > (def (t-joo-type! class-name parent . implements)
	(make-joo-type! class-name
			#f
			class-name
			(struct-tag-allocate!
			 class-name
			 (struct-metadata 'joo-test-fake-constructor))
			#f ;; XX should test interfaces, too
			parent
			implements
			'()))
 > (def ta (t-joo-type! 'a #f))
 > (def tb (t-joo-type! 'b #f))
 > (.is-a? ta tb)
 #f
 > (.is-a? tb ta)
 #f
 > (.is-a? ta ta)
 #t
 > (.is-a? tb tb)
 #t
 > (.members-perhaps-add! tb ta)
 > (.is-a? ta tb)
 #t
 > (.is-a? tb ta)
 #f
 > (.is-a? ta ta)
 #t
 > (.is-a? tb tb)
 #t
 > (def tc (t-joo-type! 'c ta))
 > (.is-a? tc ta)
 #t
 > (.is-a? ta tc)
 #f
 > (.is-a? tc tb)
 ;; by way of the relationship added above--ah that was a fake one,
 ;; hence not followed through the parent-ship chain
 #f
 > (def td (t-joo-type! 'd tb))
 > (.is-a? td ta)
 #f
 > (.is-a? td tb) ;; by way of direct relationship
 #t
 > (def te (t-joo-type! 'e td))
 > (.is-a? te td)
 #t
 > (.is-a? te ta)
 #f
 > (.is-a? te tb) ;; indirectly through td
 #t
 > (.is-a? te tc)
 #f
 )

;; Meta(?)-information pieces:

;; - tag-binding from cj-struct
;; - XX table with struct tag entries for that class' predicate
;; - joo-field-decls:<class-name> : field defs for that class
;;                                   (including parent fields right)
;; (- what about methods, I said no right? see later,
;;     hope doesnt depend on order)


(def (joo:joo-type-symbol class-name) -> symbol?
     (assert* symbol? class-name
	      (lambda (v)
		(symbol-append "joo-type:" v))))


;; (def (joo:classes)
;;      ;; XX  except  does this need flushing, too? ? ?
;;      ;; instead need a registry separate? But what   ?
;;      (symboltable->list joo:members:joo-object))

;; (def (joo:invalidate-caches!)
;;      (for-each (lambda (class)
;; 		 XX)
;; 	       (joo:classes)))
;; BUT then  we do the KISS approach first of just re-using dot-oo right?


(def (joo:decl.maybe-ref-keyword decl [keyword? k])
     (let lp ((l (source-code decl)))
       (if (pair? l)
	   (let-pair ((a l*) l)
		     (if (eq? a k)
			 (car l*) ;; Just
			 (lp l*)))
	   ;; Nothing
	   #f)))

(TEST
 > (def d '(foo #(fee? f) constructor-name: _foo a b #!key x . rest))
 > (joo:decl.maybe-ref-keyword d constructor-name:)
 _foo
 > (joo:decl.maybe-ref-keyword d constructor:)
 #f)


;; the base class

(def joo:joo-object-tag
     (struct-tag-allocate!
      'joo-object
      (struct-metadata 'joo-object-does-not-have-a-constructor)))
;; XX actually don't really need a type tag for this??
;;    Instead add a class-name field to joo-type ?

(def joo-type:joo-object
     (make-joo-type! 'joo-object
		     #f ;; maybe-location
		     #f ;; constructor-name
		     joo:joo-object-tag
		     #f ;; not an interface
		     #f ;; no parent
		     '() ;; does not implement anything
		     '() ;; no field decls
		     ))

;; this:
(def (joo:make-predicate type)
     (lambda (v)
       (joo-type.covers-instance? type v)))

;; shall now be optimized as:

(defmacro (%joo-declare)
  (if (mod:compiled?)
      `(c-declare "
#ifndef __JOO___ /* necessary? */
#define __JOO___
___SCMOBJ joo__joo_type_covers_instanceP(___SCMOBJ s, ___SCMOBJ v);
#endif
")
      `(begin)))


(IF (mod:compiled?)
    (c-declare "
// now hand-rewrite and -inline everything here. XX super evil.

// This one should be moved back to cj-struct if I wanted to do more
// such hacking.  XX also evil since also needs to be kept in sync of course.
static
___SCMOBJ maybe_struct_tag_name(___SCMOBJ v) {
    ___SCMOBJ ___temp;
    // (and (pair? v) (null? (cdr v)) (let ((t (car v))) (and (symbol? t) t)))
    if (___PAIRP(v) && ___NULLP(___CDR(v))) {
        ___SCMOBJ t= ___CAR(v);
        return ___SYMBOLP(t) ? t : ___FAL;
    } else {
        return ___FAL;
    }
}

static
___SCMOBJ struct_tag_member_ofP(___SCMOBJ t, ___SCMOBJ members) {
    ___SCMOBJ tag_name= maybe_struct_tag_name(t);
    if (tag_name == ___FAL) {
        return ___FAL;
    } else {
        ___SCMOBJ tag= symboltable_1__symboltable_ref(members,
                                                      tag_name,
                                                      ___FAL);
        return (tag == ___FAL) ? ___FAL : ((t == tag) ? ___TRU : ___FAL);
    }
}

static
___SCMOBJ joo_type_members(___SCMOBJ v) {
    return ___VECTORREF(v, ___FIX(9));
       // XX evil, keep in sync with @joo-type.members-set!
}

___SCMOBJ joo__joo_type_covers_instanceP(___SCMOBJ s, ___SCMOBJ v) {
    ___SCMOBJ ___temp;
    return (___VECTORP(v)
            && (___VECTORLENGTH(v) >= 1)) ?
            struct_tag_member_ofP(___VECTORREF(v,___FIX(0)),
                                  joo_type_members(s)) : ___FAL;
}
"))

(defmacro (%joo:make-predicate type-symbol)
  (IF (mod:compiled?)
      (with-gensym
       V
       (if (mod:compiled?)
	   (quasiquote-source
	    (lambda (,V)
	      (##c-code "___RESULT=  joo__joo_type_covers_instanceP(___ARG1,
                                                                    ___ARG2);"
			,type-symbol ,V)))
	   (quasiquote-source
	    (lambda (,V)
	      (joo-type.covers-instance? ,type-symbol ,V)))))
      `(joo:make-predicate ,type-symbol)))

;; / optimization


(def joo-object?
     (joo:make-predicate joo-type:joo-object))


(def (joo:parse-decl decl
		     ;; conts all receive:
		     ;; (class-name, maybe-constructor-name, field-decls)
		     #!key
		     cont-renamedconstructor
		     cont-samename
		     cont-nofields)
     (def constructor-stx decl) ;; for location info of the constructor call
     (mcase decl
	    (`(`class-name+perhaps-constructor-name* . `field-decls)
	     (mcase class-name+perhaps-constructor-name*

		    ;; separate constructor (XX hm, allow #f, too?,
		    ;; i.e. fields but no constructor)
		    (`(`class-name `maybe-constructor-name)
		     (cont-renamedconstructor constructor-stx
					      class-name
					      maybe-constructor-name
					      field-decls))

		    ;; constructor has the same name as the class
		    (symbol?
		     (let ((n (source-code
			       class-name+perhaps-constructor-name*)))
		       (cont-samename constructor-stx
				      class-name+perhaps-constructor-name*
				      class-name+perhaps-constructor-name*
				      field-decls)))))
	    (symbol?
	     ;; no constructor at all, and no fields either
	     (cont-nofields constructor-stx
			    decl
			    #f
			    `()))))


(def (joo:joo-expand interface?
		     decl
		     extends
		     ;; (maybe class-name), for fields and method
		     ;; implementations
		     implements
		     ;; (improper-list-of class-name), for type
		     ;; checking (i.e. predicates)
		     defs
		     /keywords?)
     (let
	 ((cc
	   (lambda (nofields?)
	     ;; parsed decl:
	     (lambda (constructor-stx
		 class-name*
		 maybe-constructor-name*
		 field-decls)
	       ;; If maybe-constructor-name is #f, that means, no
	       ;; constructor at all (abstract class or interface).

	       ;; (Whereas if nofields? is #f then no fields are
	       ;; being defined (and of course no constructor, since
	       ;; even if a parent class has fields, there's no point
	       ;; defining a new constructor with the same fields as
	       ;; the parent class, or is there? XX)

	       ;; ** Summary on the different cases: **
		
	       ;; interface: only declarations, no definitions
	       ;; (neither method nor field)

	       ;; abstract class: allows method definitions (but no
	       ;; declarations), and fields (i.e. there are two
	       ;; variants, the nofields? case, and the (not
	       ;; maybe-constructor-name) case)

	       ;; normal class: method definitions, fields, and
	       ;; constructor (but the constructor can have a
	       ;; different name than the class name).

	       ;; (NOTE: we don't implement "static fields" in
	       ;; interfaces like Java does. Do we need them? We
	       ;; don't have fields anyway, only methods, so would be
	       ;; difficult. Just use abstract class instead, OK?)

	       (if
		(and interface? maybe-constructor-name*)
		;; Note: an idea could be to allow field
		;; *declarations* in interface, and treat them as
		;; accessor method requirements (including for
		;; setters), but not actually prepending the fields
		;; to classes implementing the interface,
		;; i.e. requiring the jclass forms to re-specify
		;; those fields (perhaps in slices).
		(source-error
		 decl "field definitions not allowed in interface")

		(let*
		    ((class-name (source-code class-name*))
		     (maybe-location (maybe-source-location decl))
		     (maybe-constructor-name
		      (and maybe-constructor-name*
			   (source-code maybe-constructor-name*)))
		     ;; XX HACKy, especially since now we depend on
		     ;; internals of cj-struct. Also, not the same as
		     ;; allocated at runtime, so....?!
		     (fake-tag
		      (struct-tag-allocate!
		       class-name
		       (struct-metadata maybe-constructor-name)))
		     ;; Why are we doing the hack? Because need, at
		     ;; compile time, the list of fields, right? Need
		     ;; to append parent classes' field lists
		     ;; (all-field-decls), before generating code. So
		     ;; yes. And then we've got 1 data structure for
		     ;; everything. Also in the future should access
		     ;; parent class storage from lexical scope, to
		     ;; make it work with define-module (good luck
		     ;; implementing the module system /
		     ;; meta-language expander).

		     (maybe-parent-type-symbol
		      (and extends (joo:joo-type-symbol extends)))
	   
		     (type-symbol
		      (joo:joo-type-symbol class-name))

		     (implements-type-symbols
		      (map joo:joo-type-symbol
			   (improper-list->list (source-code implements))))

		     (type
		      (make-joo-type! class-name
				      maybe-location
				      maybe-constructor-name
				      fake-tag
				      interface?
				      (and maybe-parent-type-symbol
					   (eval maybe-parent-type-symbol))
				      ;; ^ XX as mentioned above
				      ;; should really pass context
				      ;; to eval
				      (map eval implements-type-symbols)
				      field-decls))

		     (saved-values (values #f #f)))

		  ;; store in the symbol in case another joo-class is being
		  ;; expanded in the same compilation run (sigh, the old
		  ;; schizophrenia is back) (ah and crazy, in two-step process
		  ;; here, for storing run time value):
		  (eval `(define ,type-symbol #f))
		  ((eval `(lambda (v) (set! ,type-symbol v))) type)

		  `(begin
		     (%joo-declare)
		     ;; XX TODO: in the case of a class definition
		     ;; with fields but no constructor, we would
		     ;; still like to be able to use def-method, but
		     ;; currently can't as let-classname is defined
		     ;; by define-struct.-expand. Split that out so
		     ;; that we can.
		     ,(if maybe-constructor-name
			  (define-struct.-expand
			    constructor-stx ;; for location info only
			    class-name
			    (cons* predicate-code:
				   (lambda (predicate-symbol
				       tag-symbol
				       add-offset
				       numfields)
				     ;; ugh ugly but so, to break up
				     ;; circular dependency between
				     ;; cj-struct and make-joo-type!:
				     (set! saved-values
					   (values predicate-symbol
						   tag-symbol))
				     ;; output no predicate code yet,
				     ;; will that work?
				     `(begin))
				   constructor-name:
				   maybe-constructor-name
				   /keywords?:
				   /keywords?
				   (joo-type.all-field-decls type)))
			  `(begin))

		     ,(letv ((predicate-symbol tag-symbol) saved-values)
			    `(begin
			       (define ,type-symbol
				 ;; XX and as mentioned gah, since
				 ;; here ge create another version of
				 ;; the joo-type object, when it
				 ;; should be a singleton. Hope the
				 ;; kind of mutations done don't
				 ;; matter. And can't even use the
				 ;; same code since eval with *our*
				 ;; lexical context (macro expander)
				 ;; wouldn't work either. \SCHEME is
				 ;; so unfinished! \CL?
				 (make-joo-type!
				  ',class-name
				  ',maybe-location
				  ',maybe-constructor-name
				  ,(if maybe-constructor-name
				       tag-symbol
				       #f)
				  ,interface?
				  ,maybe-parent-type-symbol
				  ;; ^ heh, otherwise #f
				  ;; which is also valid
				  ;; source for the
				  ;; purpose
				  (list ,@implements-type-symbols)
				  ,(source-quote* field-decls)))

			       (define ,(let ((predicate-symbol*
					       (symbol-append class-name "?")))
					  (when predicate-symbol
                                                (assert (eq? predicate-symbol*
                                                             predicate-symbol)))
					  predicate-symbol*)
				 (%joo:make-predicate ,type-symbol))))


		     ;; Can't use ##define-syntax, as it leaves the
		     ;; scope of the |begin| forms, and hence the
		     ;; original joo-* forms (would need
		     ;; ##let-syntax). Use macro-expand/symtbl
		     ;; instead.

		     ,@(map
			(macro-expander/symtbl 
			 (let ((m-
				(if interface?
				    joo:implementation-method-expander-forbidden
				    (joo:implementation-method-expander-for
				     class-name
				     #f
				     nofields?)))
			       (m
				(if interface?
				    joo:implementation-method-expander-forbidden
				    (joo:implementation-method-expander-for
				     class-name
				     (joo-type.all-field-names type)
				     nofields?))))
			   (symboltable*
			    ;; abstract methods
			    method:
			    (if (or interface? nofields?)
				(joo:abstract-method-expander-for class-name)
				joo:abstract-method-expander-forbidden)
			    ;; implementations
			    def-method-: m-
			    defmethod-: m-
			    ;; with fields bound to variables
			    def-method: m
			    defmethod: m)))
			(begin-flatten (cons 'begin defs)
				       '())))))))))

       (joo:parse-decl decl
		       cont-renamedconstructor: (cc #f)
		       cont-samename: (cc #f)
		       cont-nofields: (cc #t))))

(defmacro (joo-class decl
		     #!key
		     (extends 'joo-object)
		     (implements '())
		     /keywords?
		     keywords
		     . defs)
  (joo:joo-expand #f decl extends implements defs
		  (or /keywords? keywords)))

(defmacro (joo-interface decl
			 #!key
			 (extends '())
			 ;;(implements '()) -- XX[1]
			 . defs)
  (joo:joo-expand #t decl #f extends defs
		  #f))
;; [1] is `implements` allowed for interfaces in Java? Seems that yes?
;; http://docs.oracle.com/javase/specs/jls/se8/html/jls-9.html but
;; then tests show that not.


(def. (joo-object.instance-of? s #(joo-type? t))
  (joo-type.covers-instance? t s))

;; this would need mapping from tags to types sigh. Not just from tag
;; names.
'(def. (joo-object.is-a? s #(joo-object? t))
  (joo-type.is-a? XX XX))


(TEST
 > (joo-class (joo_fooo a b)
	      (def-method- (haha s)
		(.a s))
	      (def-method- id identity))
 > (joo-object? (joo_fooo 1 2))
 #t
 > (.haha (joo_fooo 1 2))
 1
 > (.show (.id (joo_fooo 1 2)))
 (joo_fooo 1 2))

;; test checks:
(TEST
 > (with-exception-handler source-error-message
			   (& (eval `(joo-class (joo_fooo a b)
						(method (haha s))))))
 "abstract method not allowed in non-abstract class"
 > (eval `(joo-class joo_fooo
		     (method (haha s))))
 #!void
 > (with-exception-handler source-error-message
			   (& (eval `(joo-interface joo_fooo
						    (def-method- (haha s))))))
 "method implementation not allowed in interface"
 > (with-exception-handler source-error-message
			   (& (eval `(joo-interface (joo_fooo a b)
						    (method (haha s))))))
 "field definitions not allowed in interface")


;; without interfaces:
(TEST
 > (joo-class (joo_foo-number))
 > (joo-class (joo_foo-complex #(boolean? exact?))
	      extends: joo_foo-number)
 > (joo-class (joo_foo-real) extends: joo_foo-complex)
 > (joo-class ((joo_foo-integer _joo_foo-integer))
	      extends: joo_foo-real
	      (def (joo_foo-integer)
		   (_joo_foo-integer #t)))
 > (joo-class (joo_foo-natural0)
	      extends: joo_foo-integer)

 > (joo_foo-real? (joo_foo-integer))
 #t
 ;; breaking down here, foo-natural0 still needs the argument:
 > (.exact? (joo_foo-natural0 #f))
 #f)

;; with interfaces:
(TEST
 > (joo-interface bar-number-interface
		  (method (exact? s)))
 > (joo-interface bar-complex-interface
		  extends: bar-number-interface)
 > (joo-interface bar-real-interface
		  extends: bar-complex-interface)
 > (joo-interface bar-integer-interface
		  extends: bar-real-interface)
 > (joo-interface bar-natural0-interface
		  extends: bar-integer-interface)
 > (joo-interface bar-natural-interface
		  extends: bar-natural0-interface)

 > (joo-class (bar-complex #(boolean? exact?))
	      implements: (bar-complex-interface))
 > (joo-class (bar-real)
	      extends: bar-complex
	      implements: (bar-real-interface))

 > (joo-class (bar-integer)
	      implements: (bar-integer-interface)
	      (def-method- (exact? _)
		#t)
	      ;; to see that rest args work:
	      (def-method- (fifi a . b)
		(vector a b)))
 > (joo-class (bar-natural0)
	      extends: bar-integer
	      implements: (bar-natural0-interface))
 > (joo-class (bar-natural)
	      extends: bar-natural0
	      implements: (bar-natural-interface))

 > (bar-real? (bar-integer))
 #f
 > (bar-number-interface? (bar-integer))
 #t
 > (bar-real-interface? (bar-integer))
 #t
 > (bar-natural-interface? (bar-integer))
 #f
 
 > (.exact? (bar-complex #t))
 #t
 > (.exact? (bar-complex #f))
 #f
 > (.exact? (bar-natural0))
 #t
 )


;; def-method
(TEST
 > (def z 'outside)
 > (joo-class (fooagain x #(pair? y) z)
	      (def-method- (bar x y)
		(list x y z)))
 > (.bar (fooagain 10 (cons 1 2) 'f) 11)
 (#((fooagain) 10 (1 . 2) f) 11 outside)

 > (joo-class (fooagain2 x #(pair? y) z)
	      (def-method (bar x y)
		(list x y z))
	      (def-method (baz x y) -> pair?
		(list x y z))
	      (def-method foo (lambda (s) x)))
 
 > (def myfooagain2 (fooagain2 10 (cons 1 2) 'f))
 > (.bar myfooagain2 11)
 (#((fooagain2) 10 (1 . 2) f) 11 f)
 > (.baz myfooagain2 11)
 (#((fooagain2) 10 (1 . 2) f) 11 f)
 > (.foo myfooagain2)
 10)

;; def.*
(TEST
 > (def.* (fooagain2.testdefstar _)
     y)
 > (.testdefstar myfooagain2)
 (1 . 2)
 ;; and now also supporting immediate lambda forms:
 > (def.* fooagain2.testdefstar2
     ;; ah lambda_ not supported lol. XX should macro-expand first.
     (lambda (_) y))
 > (.testdefstar2 myfooagain2)
 (1 . 2))

;; with.
(TEST
 > (define TEST:equal? syntax-equal?)
 > (expansion#with. fooagain2 p (list z x a n))
 (let-fooagain2 ((x _ z) p) (list z x a n))
 > (expansion#with. fooagain2 (a bc) (list y z a n))
 (##let ((GEN:-23362 (a bc)))
        (let-fooagain2 ((_ y z) GEN:-23362)
                       (list y z a n))))



;; def-method and inheritance, or, test the let-<classname> feature
;; with inheritance for the first tiem:
(TEST
 > (joo-class (baa a b))
 > (joo-class (boo c) extends: baa
	      (def-method (foo1 s) (list c))
	      (def-method (foo2 s) (list a b c))
	      (def-method (foo3 s) -> pair? (list b c)))
 > (.foo1 (boo 10 11 12))
 (12)
 > (.foo2 (boo 10 11 12))
 (10 11 12)
 > (.foo3 (boo 10 11 12))
 (11 12))

;; Test that sub-forms are expanded
(TEST
 > (defmacro (my-def-method . args) `(def-method ,@args))
 ;; note that this fails:
 ;; > (defmacro (my-def-method . args) `(begin (void)
 ;; 					    (def-method ,@args)))
 > (joo-class (boo c)
	      (my-def-method (foo1 s) (list c)))
 > (.foo1 (boo 10))
 (10))

;; Test /keywords feature:
(TEST
 > (defclass (foom b c) /keywords?: #t)
 > (foom 10 30)
 [(foom) 10 30]
 > (foom/keywords c: 10 b: 20)
 [(foom) 20 10])



;; Whether a class or interface "extends" or "implements" a particular
;; 'parent' (not used within joo.scm).
(def (joo-extends-or-implements stx super-is-class? is-class?)
     (if super-is-class?
	 (if is-class?
	     `extends:
	     (source-error stx "an interface cannot extend a class"))
	 (if is-class?
	     `implements:
	     `extends:)))

