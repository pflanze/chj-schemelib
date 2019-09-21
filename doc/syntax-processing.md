# Syntax processing modules

This is an attempt at listing the various modules which process Scheme
(and related) syntax (s-expressions); these can be used for building
macros or other (compiler level) infrastructure.

cj-source.scm
    source
    location
    position
    read-all-source
	 show-location-location
	 show-source-location
	 source-warn
	 location-warn
    ..
	 source-quote ;; deprecated, use source-quote* instead?
	 source-dequote ;; ditto
	 source-quote*
     #hmm??

define-macro-star.scm
    macro-star-expand-1
    macro-star-expand
    define-macro*
    require
    export
    options

cj-source-util--include.scm 
  "XX move to cj-source-lambda.scm?"
  ";; tests see cj-source-util-test.scm"
  '(export schemedefinition-arity:template->checker
	schemedefinition-arity:pattern->template
	schemedefinition-arity-checker
	improper-length ;;via (include "improper-length--source.scm")
	safer-apply)

cj-source-lambda.scm
  ";; XXX shouldn't this be merged with dsssl.scm ?"
  (export source.bindings->app
	#!optional
	source.list-of-length-2?)
  
dsssl.scm
   ";; XXX shouldn't this be merged with cj-source-lambda.scm ?"
   (export sequential-pairs
        sequentialpairs->pairs ;; older, obsolete ?
        dsssl->alist
        alist->dsssl
        dsssl-maybe-ref ;; should move to Maybe ?
        dsssl-ref
        dsssl-delete
        dsssl-defaults
        dsssl-apply
        #!optional
        dsssl-delete-1)




cj-expansion.scm
  (export (macro expansion)
	(macro macro-expand-all)
	(macro macro-expand))

cj-phasing.scm
  (export (macro insert-result-of)
	(macro compile-time)
	(macro both-times))
    
simple-match-1.scm
  (export (macro warn*)
        (macro match*)
        (macro match-list*)
        (macro assert-desourcified*)
        (macro assert*)
        (macro assert**))

simple-match.scm
  # aha just the tests "?"! rename!

scheme-meta.scm
  (export void?
	optional?
	key?
	rest?
	
	self-quoting?
	constant-expr?
	perhaps-quote
	uvector?
	svector?
	fvector?
	homogenous-vector?
	sexpr-object?
	sexpr?

	#!optional
	pair-of-sexpr?
	vector-of-sexpr?
	void?)

cj-source-util-2.scm
  (export (macros assert
		assert-privately
		assert-and
		V)
	pp-through-source
	no-pp-through-source
	source-map
	(macros with-source
		with-source*))

cj-typed-1.scm
  (export (mutable cj-typed-1:error?)
	(mutable cj-typed-1:.string)
	cj-typed#type-check-error
	type-failure-handling?
	current-type-failure-handling
	cj-typed#type-check-warn)

cj-typed.scm
  (export (macro type-check)
        (macro source-type-check)
        perhaps-typed.var
        perhaps-typed.maybe-predicate
        typed?
        @typed.var
        typed.var
        typed.predicate
        args-detype
        (macro typed-lambda)
        (macro define-typed)
        (macro ->)
        (macro @->)
        ;; indirectly: ->-error
        (macros cj-typed-disable
                cj-typed-enable)
        
        #!optional
        typed-body-parse)
        
cj-source-2.scm
  (export vector-equal?
	source-equal?
	(macro template:quote)
	(macro force-source-code))

cj-match.scm
  (export (macro match)
        (macro matchl)
        (macro mcase)
        (macro mcase-lambda))

code-macro-expand.scm
  (export macro-expand/symtbl
	macro-expander/symtbl
	begin-flatten)
  Makeshift "manual" ##let-syntax. Only top-level exprs are expanded!
  #xx hmm ?

tree-util.scm
  (export flatten/
        flatten
        flatten*)
  #xx move to some list-util ? ah tree, well


cj-source-test.scm
  # tests only
  
cj-source-util-test.scm
  # tests only

