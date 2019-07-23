;;; Copyright 2010-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 (fixnum inc dec)
	 cj-struct-tag
	 test
	 simple-match
	 (cj-symbol with-gensyms)
	 named
	 (cj-env symbol-append define-if-not-defined list-max) ;; in macro expansion
	 (predicates-1 list-of-length)
	 (cj-inline-1 define-inline.1) ;; cj-inline would give cycle
	 (cj-gambit-sys-0 @vector-ref @vector-length @vector-set!)
	 (srfi-1 filter cons*))

(export (macro define-struct)
	(macro define-struct*)
	struct?
	struct-type
	struct-type-name
	struct-type->constructor-name
	struct-constructor-name
	struct-of-type
	;; odd one?:
	struct-of

	struct-values

        vector-like?

	#!optional
	define-struct-expand)


;; (Copy of explanation in cj-struct-tag:
;; A struct is a vector with an object in slot 0 that unambiguously
;; determines the struct type, implemented by using allocated,
;; non-interned objects ("tags", but they are not visual, but
;; invisibly unique; for now, sadly (no type language yet)).
;; )

(include "cj-standarddeclares.scm")


(define define-struct:arg->maybe-fieldname
  (named self
	 (lambda (v*)
	   (let ((v (source-code v*)))
	     (cond ((symbol? v)
		    v*)
		   ((dsssl-meta-object? v)
		    #f)
		   (((list-of-length 2) v)
		    ;; `(`definition `default-value)
		    (self (car v)))
		   (else
		    (source-error
		     v*
		     "expecting symbol or dsssl-meta-object")))))))

(define (define-struct-expand
	  DEFINE	;; what definition forms to use
	  LAMBDA		;; what lambda form to use; useful for typed-lambda
	  UNSAFE-LAMBDA	;; what lambda form to use when
	  ;; unsafe-constructor-name is given
	  arg->maybe-fieldname ;; fn to extract the field name symbols
	  wrap-var ;; (var, field+) -> e.g. field+ with variable replaced by var
	  wrap-fn-for ;; (fn, field+) -> e.g. fn that uses check from field+ to type check
	  ;; then the actual user-servicable parts:
	  name*
	  #!key
	  ;; leaving away the star at those (because it'd be "uservisible")
	  (prefix "")
	  (accessor-prefix "")
	  (unsafe-accessor-prefix "@")
	  (separator "-")
	  ;; the prefix for the accessors when falling back to generic access:
	  generic-accessor-prefix ;; including name if wished, and even the separator !
	  constructor-name
	  constructor-stx ;; for location info for constructor call (e.g. on type failures)
	  unsafe-constructor-name
	  predicate-name
	  predicate-code ;; mostly for joo.scm
	  let*-name
	  let-name
	  let-fallback?
	  tag
	  tag-prefix
	  ;; whether to generate a purely positional constructor with
	  ;; constructor-name/positional as name:
	  /positional?
	  ;; whether to generate a constructor-name/keywords macro:
	  /keywords?
	  #!rest args*)

  (let* ((name (source-code name*))
	 (tag-code
	  (if tag
	      (if tag-prefix
		  `(symbol-append ,tag-prefix ,tag)
		  tag)
	      (if tag-prefix
		  `(symbol-append ,tag-prefix (##quote ,name*))
		  `(##quote ,name*))))
	 (tag-binding (symbol-append "cj-struct:tag:" name))
	 (fields* (filter identity (map arg->maybe-fieldname args*)))
	 ;; XX isn't fields* already dsssl-filtered, i.e. fields+ the same?
	 (fields+ (filter (lambda (arg) (not (dsssl-meta-object? (source-code arg))))
			  ;; ^ assuming that DEFINE/LAMBDA won't ever need anything else
			  args*))
	 (_ (or (= (length fields*) (length fields+)) (error "assertion failure")))
	 (fields (map source-code fields*))
	 ;; keyed arguments:
	 (prefix (source-code prefix))
	 (accessor-prefix (source-code accessor-prefix))
	 (unsafe-accessor-prefix (source-code unsafe-accessor-prefix))
	 (separator (source-code separator))
	 (constructor-name (or constructor-name
			       (symbol-append prefix "make-" name)))
	 (positional-constructor-name
	  (symbol-append (source-code constructor-name)
			 '/positional))
	 (predicate-name
	  (if predicate-name
	      (source-code predicate-name)
	      (symbol-append prefix name "?")))
	 (generic-accessor-prefix
	  (if generic-accessor-prefix
	      (source-code generic-accessor-prefix)
	      (symbol-append prefix name separator)))
	 (let*-name
	  (or let*-name
	      ;; ah yeh, why did I want to strip source info above? not used for append
	      (symbol-append ;; prefix  no, do not use the prefix for this. It's a macro. Ok?
	       "let*-" name)))
	 (let-name
	  (or let-name
	      (symbol-append ;; prefix no
	       "let-" name)))
	 (error-name
	  (symbol-append prefix name "-type-error"))
	 (genericsetter-name
	  ;; (XX: even more risk for name conflicts, thus uppercase it, k?)
	  (symbol-append prefix name "-GENERIC-SET"))
	 (genericupdater-name
	  ;; (XX: even more risk for name conflicts, thus uppercase it, k?)
	  (symbol-append prefix name "-GENERIC-UPDATE"))
	 ;; other stuff
	 (numfields (length fields))
	 (offset 1) ;; Change to 0 if no type head is used (future unsafe mode) !
	 (add-offset
	  (lambda (i)
	    (+ offset i)))
	 (safe-accessor-for-field
	  (lambda (field)
	    (symbol-append prefix
			   accessor-prefix
			   name
			   separator
			   (source-code field))))
	 (safe-setter-for-field
	  (lambda (field)
	    ;; always just "-set"?
	    (symbol-append (safe-accessor-for-field field) "-set")))
	 (safe-updater-for-field
	  (lambda (field)
	    ;; always just "-update"?
	    (symbol-append (safe-accessor-for-field field) "-update")))
	 )
    `(begin
       ;; Use define-if-not-defined here or not? Yes because otherwise
       ;; reloading the file with a struct definition breaks handling
       ;; of previously allocated objects. (But then isn't this
       ;; breaking define-module concerns? Hey why should it?)
       (define-if-not-defined ,tag-binding
	 (struct-tag-allocate! ,tag-code
			       (struct-metadata ',constructor-name)))

       ,@(let ((construct
		(lambda (LAMBDA constructor-name)
		  `(define ,constructor-name
		     ,(possibly-sourcify `(,LAMBDA ,args*
					      (##vector ,tag-binding
							,@fields*))
					 constructor-stx)))))
	   `(,(construct LAMBDA
			 constructor-name)
	     ,@(if unsafe-constructor-name
		   `(,(construct UNSAFE-LAMBDA
				 unsafe-constructor-name))
		   '())))

       ;; always purely positional constructor, even when keyword or
       ;; rest arguments were given (only safe variant for now, OK?):
       ,@(if (or /positional? /keywords?)
	     (if 
	      `((define ,positional-constructor-name ,constructor-name))
	      `((define (,positional-constructor-name
			 ,@fields)
		  (##vector ,tag-binding
			    ,@fields)))))
       ,@(if /keywords?
	     `((define-macro* (,(symbol-append constructor-name
					       '/keywords)
			       #!key
			       ,@fields)
		 ,(cons* 'list
			 (list 'quote constructor-name)
			 fields)))
	     '())
       
       ,(if predicate-code
	    (predicate-code predicate-name tag-binding
			    add-offset numfields)
	    `(define ,predicate-name
	       (lambda (v)
		 (and (##vector? v)
		      (= (@vector-length v) ,(add-offset numfields))
		      (eq? (@vector-ref v 0)
			   ,tag-binding)))))
       (define ,error-name
	 (lambda (v)
	   (error ,(string-append "expecting a "
				  (symbol->string name)
				  ", got:")
		  v)))
       (define ,genericsetter-name
	 (lambda (v offset value)
	   ;; ^ it's safe to use static variable names here as
	   ;; predicate-name can't conflict with them, *correct*? (at
	   ;; least if a "-" or "." separator is used)
	   (if (,predicate-name v)
	       (let ((v* (##vector-copy v)))
		 (vector-set! v* offset value) ;; XXX use @vector-set!, safe?
		 v*)
	       (,error-name v))))
       (define ,genericupdater-name
	 (lambda (v offset fn)
	   (if (,predicate-name v)
	       (let ((v* (##vector-copy v)))
		 (@vector-set! v* offset ;; XXX is use of @vector-set! safe?
			       (fn (@vector-ref v offset)))
		 v*)
	       (,error-name v))))
       ,@(map (lambda (field field+ i)
		(with-gensyms
		 (FN)
		 `(begin
		    (,DEFINE ,(safe-accessor-for-field field)
		      (lambda (v)
			(if (,predicate-name v)
			    (vector-ref v ,(add-offset i))
			    (,error-name v))))
		    ;; unsafe version:
		    (define-macro* (,(symbol-append prefix
						    unsafe-accessor-prefix
						    name
						    separator
						    field) v)
		      ,(list 'quasiquote
			     `(@vector-ref ,(list 'unquote 'v) ,(add-offset i))))
		    ;; functional setter:
		    (,DEFINE ,(safe-setter-for-field field)
		      (,LAMBDA (v ,(wrap-var 'value field+))
			  ;; use shared code
			  (,genericsetter-name v ,(add-offset i) value)))
		    ;; functional updater:
		    (,DEFINE ,(safe-updater-for-field field)
		      (lambda (v ,FN)
			;; use shared code
			(,genericupdater-name v ,(add-offset i) ,(wrap-fn-for FN field+)))))))
	      fields
	      fields+
	      (iota numfields))
       (define-macro* (,let*-name vars+inp-s . body)
	 (let* (
		;; copies of the accessed parts of the outer expander's context:
		(offset ',offset)
		(prefix ',prefix)
		(accessor-prefix ',accessor-prefix)
		(generic-accessor-prefix ',generic-accessor-prefix)
		;; copies of procedures from the outer expander
		(add-offset
		 (lambda (i)
		   (+ offset i)))
		(generic-accessor-for-field
		 (lambda (field)
		   (symbol-append generic-accessor-prefix (source-code field))))
		;; /copies
		)
	   (let rec ((vars+inp-s (source-code vars+inp-s)))
	     (if (null? vars+inp-s)
		 `(begin ,@body)
		 (match-list*
		  (car vars+inp-s)
		  ((vars* inp)
		   (match-list*	;; to remove source info and to check for proper list
		    vars*
		    (vars
		     (let ((numvars (length vars)))
		       ;; allow smaller num of vars for extensibility
		       (if (<= numvars ,numfields)
			   (let ((real-v+f+i-s (filter (lambda (v+f+i)
							 (not (eq? (source-code (vector-ref v+f+i 0))
								   '_)))
						       (map vector
							    vars
							    (take ',fields numvars)
							    (iota numvars))))
				 (V (gensym))
				 (C (gensym)))
			     `(let ((,V ,inp)
				    (,C (lambda ,(map (lambda (v+f+i)
						   (vector-ref v+f+i 0))
						 real-v+f+i-s)
					  ,(rec (cdr vars+inp-s)))))
				(if
				 (and (,',predicate-name ,V)
				      ;; XX is the following check
				      ;; really needed? (does the
				      ;; predicate not already
				      ;; always include it? But
				      ;; better be safe: reloaded
				      ;; code!, i.e. changing
				      ;; definitions); don't even
				      ;; count on it being a
				      ;; vector?
				      ,(if (null? real-v+f+i-s)
					   ;; no variable accesses,
					   ;; thus no need for any
					   ;; size check
					   `#t
					   `(fx> (vector-length ,V)
						 ,(list-max
						   (map (lambda (v+f+i)
							  (add-offset
							   (vector-ref v+f+i 2)))
							real-v+f+i-s)))))
				 (,C
				  ,@(map (lambda (v+f+i)
					   `(@vector-ref ,V ,(add-offset (vector-ref v+f+i 2))))
					 real-v+f+i-s))
				 ,(if ,let-fallback?
				      ;; use type-name.field-name
				      ;; style accessors for cases
				      ;; when those work even if
				      ;; the type check fails
				      `(,C
					,@(map (lambda (v+f+i)
						 `(,(generic-accessor-for-field (vector-ref v+f+i 1))
						   ,V))
					       real-v+f+i-s))
				      ;; by default, it's just a
				      ;; type error (e.g. joo's
				      ;; type predicates already
				      ;; allow for subtyping and
				      ;; the vector-ref approach
				      ;; works in that case, too)
				      `(,',error-name ,V)))))
			   (source-error vars*
					 "invalid number of variables")))))))))))
       (define-macro* (,let-name vars+inp . body)
	 `(,',let*-name (,vars+inp) ,@body)))))


(define-macro* (define-struct . args)
  ;; (sigh, had safer-apply right? What for again?)
  (apply define-struct-expand
	 'define
	 'lambda
	 'lambda
	 define-struct:arg->maybe-fieldname
	 (lambda (var field+) var)
	 (lambda (fn field+) fn)
	 args))

(TEST
 > (define-struct foo a b )
 > (struct? (make-foo 1 2))
 #t
 > (struct-constructor-name (make-foo 1 2))
 make-foo
 > (make-foo 10 11)
 [(foo) 10 11]
 > (foo? #)
 #t
 > (foo? #)
 #f
 > (foo? '[(foo) 10 11])
 ;; unlike TEST comparisons which are based on equal?, this is
 ;; comparing more strictly
 #f
 > (define x (make-foo 10 11))
 > (foo-a x)
 10
 > (foo-b x)
 11
 ;; > (foo-b #t)
 ;; *** ERROR IN (console)@16.1 -- expecting a foo, got: #t
 ;; 1> 
 > (let-foo ((a b) x) a)
 10

 ;; tag feature:
 > (define-struct foo1 tag: 'myvery:foo a b)
 > (define v1 (make-foo1 10 11))
 > v1
 [(myvery:foo) 10 11]
 > (define-struct foo2 tag: 'myvery:foo a b)
 > (define v2 (make-foo2 10 11))
 > v2
 [(myvery:foo) 10 11]
 > (foo1? v1)
 #t
 > (foo1? v2)
 #f
 > (foo2? v1)
 #f
 > (foo2? v2)
 #t

 ;; updaters
 > (define-struct foo a b)
 > (make-foo 1 2)
 [(foo) 1 2]
 > (foo-b-update # inc-function)
 [(foo) 1 3]
 > (foo-b-update # inc-function)
 [(foo) 1 4]
 > (foo-a-update # inc-function)
 [(foo) 2 4]
 > (define-struct foo #!key quote x y z)
 > (make-foo quote: 10)
 [(foo) 10 #f #f #f]

 ;; keywords feature:
 > (define-struct foon /keywords?: #t b c)
 > (make-foon c: 3)
 [(foon) c: 3]
 > (make-foon/keywords c: 3)
 [(foon) #f 3])


;; omit the |make-| prefix for the constructor name
(define-macro* (define-struct* name . defs)
  `(define-struct ,name constructor-name: ,name ,@defs))


;; see also |define-struct.| in dot-oo


;; Generic struct ops:

;; alternative names: "object?", "instance?"
(define (struct? v)
  ;; XX also check that the length of the vector matches the
  ;; corresponding class? or not since that would fail with
  ;; multiversioning?
  (and (##vector? v)
       (fx>= (@vector-length v) 1)
       (struct-tag? (@vector-ref v 0))))

(define (struct-of pred)
  (lambda (v)
    (and (struct? v)
	 (let ((len (vector-length v)))
	   (let lp ((i 1))
	     (if (< i len)
		 (and (pred (vector-ref v i))
		      (lp (inc i)))
		 #t))))))

(TEST
 > (define vals `(foo
		  ,cj-struct:tag:foo
		  (foo)
		  (,cj-struct:tag:foo)
		  [foo]
		  ,(list->vector cj-struct:tag:foo)
		  [(foo)]
		  [,cj-struct:tag:foo]
		  [(foo) 1]
		  [,cj-struct:tag:foo 1]
		  [1 foo]
		  [,cj-struct:tag:foo 1 2]
		  [,cj-struct:tag:foo 1 -2]
		  [,cj-struct:tag:foo 1 "-2"]))
 > (map (struct-of natural0?) vals)
 (#f #f #f #f #f #f #f #t #f #t #f #t #f #f))


(define (struct-type v)
  (if (struct? v)
      (vector-ref v 0)
      (error "not a struct:" v)))

(define (struct-type-name v)
  ;; struct-type already guarantees the type is valid, thus use unsafe
  ;; name getter
  (@maybe-struct-tag-name (struct-type v)))


(define (struct-type->constructor-name t)
  (struct-metadata.constructor-name
   (struct-tag->metadata t)))

(define (struct-constructor-name v)
  (struct-type->constructor-name (struct-type v)))


;; Does not check for parent types! (This is not an is-a check.) Also,
;; does not verify the number of fields, just the type tag!

;; Also, currently requires a type value, it doesn't look them up by
;; name, currently can't: the concept is eq values ("invisible"
;; ones). You need to pass them by scope etc., by value, not
;; "stored-name". The storage place is in the runtime code. Again,
;; currently, might change this "to type language" (then passing the
;; "term" works here, and equal terms will be the same type).  OF
;; COURSE this is quite pointless really now since we have predicate
;; functions already! Those are even better since they work with
;; subtyping (sort of). Hum.

(define (struct-of-type tag)
  (if (struct-tag? tag)
      (lambda (v)
	(and (struct? v)
	     (eq? (struct-type v) tag)))
      (error "struct-of-type: not a struct tag:" tag)))

(TEST
 > (define f? (struct-of-type cj-struct:tag:foo))
 > (map f? vals)
 ;; (0 0 0 0 0 0 0 1 0 1 0 1 1 1)
 (#f #f #f #f #f #f #f #t #f #t #f #t #t #t))


;; there used to be a copy called struct->values
(define (struct-values s)
  (if (struct? s)
      (cdr (vector->list s))
      (error "not a struct:" s)))


(define (vector-like? v)
  (and (cj-gambit-sys:vector-like? v)
       (or (not (##vector? v))
           (cj-struct#vector? v))))

(TEST
 > (vector-like? (vector 1))
 #t
 > (vector-like? (cons 1 2))
 #f
 > (vector-like? (u8vector))
 #t
 > (vector-like? (vector))
 #t
 > (defclass (foo x y))
 > (vector-like? (foo 1 2))
 #f
 > (vector-like? (values 1 2))
 #f)

