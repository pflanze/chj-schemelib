;;; Copyright 2010-2014 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (lib.define-macro-star)
	 (lib.test)
	 (lib.simple-match)
	 (lib.cj-env);; symbol-append
	 )

;; XX move to predicates.scm ? dependency order?
(define (list-of-length n)
  (lambda (v)
    (let lp ((v v)
	     (len 0))
      (cond ((pair? v)
	     (if (< len n)
		 (lp (cdr v) (inc len))
		 #f))
	    ((null? v)
	     (= len n))
	    (else
	     ;; not a list
	     #f)))))

(TEST
 > (def vals '(() (a) (a b) (a b c) (a b . c) a))
 > (map (list-of-length 2) vals)
 (#f #f #t #f #f #f)
 > (map (list-of-length 0) vals)
 (#t #f #f #f #f #f)
 > (map (list-of-length 4) vals)
 (#f #f #f #f #f #f))


(define define-struct:arg->maybe-fieldname
  (named self
	 (lambda (v*)
	   (let ((v (source-code v*)))
	     (cond ((symbol? v)
		    v*)
		   ((meta-object? v)
		    #f)
		   (((list-of-length 2) v)
		    ;; `(`definition `default-value)
		    (self (car v)))
		   (else
		    (source-error
		     v*
		     "expecting symbol or meta-object")))))))

(define (define-struct-expand
	  DEFINE ;; what definition forms to use
	  LAMBDA ;; what lambda form to use; useful for typed-lambda
	  UNSAFE-LAMBDA	       ;; what lambda form to use when
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
	  unsafe-constructor-name
	  predicate-name
	  let*-name
	  let-name
	  tag
	  tag-prefix
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
	 (fields* (filter identity (map arg->maybe-fieldname args*)))
	 (fields+ (filter (lambda (arg) (not (meta-object? (source-code arg))))
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
       ,@(let ((construct
		(lambda (LAMBDA constructor-name)
		  `(define ,constructor-name
		     (,LAMBDA ,args*
			      (##vector ,tag-code
					,@fields*))))))
	   `(,(construct LAMBDA
			 constructor-name)
	     ,@(if unsafe-constructor-name
		   `(,(construct UNSAFE-LAMBDA
				 unsafe-constructor-name))
		   '())))
       
       (define ,predicate-name
	 (lambda (v)
	   (and (vector? v)
		(= (vector-length v) ,(add-offset numfields))
		(eq? (vector-ref v 0)
		     ,tag-code))))
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
		 (vector-set! v* offset value)
		 v*)
	       (,error-name v))))
       (define ,genericupdater-name
	 (lambda (v offset fn)
	   (if (,predicate-name v)
	       (let ((v* (##vector-copy v)))
		 (##vector-set! v* offset
				(fn (##vector-ref v offset)))
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
			     `(##vector-ref ,(list 'unquote 'v) ,(add-offset i))))
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
		(name ,tag-code)
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
		       (if (<= numvars ,numfields) ;; allow smaller num of vars for extensibility, ok?
			   (let ((real-v+f+i-s (filter (lambda (v+f+i)
							 (not (eq? (source-code (vector-ref v+f+i 0))
								   '_)))
						       (map vector
							    vars
							    ',fields
							    (iota numvars))))
				 (V (gensym))
				 (C (gensym)))
			     `(let ((,V ,inp)
				    (,C (lambda ,(map (lambda (v+f+i)
							(vector-ref v+f+i 0))
						      real-v+f+i-s)
					  ,(rec (cdr vars+inp-s)))))
				(if (,',predicate-name ,V)
				    (,C
				     ,@(map (lambda (v+f+i)
					      `(vector-ref ,V ,(add-offset (vector-ref v+f+i 2))))
					    real-v+f+i-s))
				    ;; (,',error-name ,V)
				    ;; or, in a setting with generics, fall back to the dynamic dispatches:
				    (,C
				     ,@(map (lambda (v+f+i)
					      `(,(generic-accessor-for-field (vector-ref v+f+i 1))
						,V))
					    real-v+f+i-s)))))
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
 > (make-foo 10 11)
 #(foo 10 11)
 > (foo? #)
 #t
 > (foo? #)
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
 > (define-struct foo tag: 'myvery:foo a b)
 > (make-foo 10 11)
 #(myvery:foo 10 11)
 > (foo? #)
 #t
 > (foo? #)
 #f
 ;; updaters
 > (define-struct foo a b)
 > (make-foo 1 2)
 #(foo 1 2)
 > (foo-b-update # inc)
 #(foo 1 3)
 > (foo-b-update # inc)
 #(foo 1 4)
 > (foo-a-update # inc)
 #(foo 2 4)
 > (define-struct foo #!key quote x y z)
 > (make-foo quote: 10)
 #(foo 10 #f #f #f)
 )

;; omit the |make-| prefix for the constructor name
(define-macro* (define-struct* name . defs)
  `(define-struct ,name constructor-name: ,name ,@defs))


;; see also |define-struct.| in dot-oo


;; Generic struct ops:

(define (struct? v)
  ;; XX also check that the length of the vector matches the
  ;; corresponding class? or not since that would fail with
  ;; multiversioning?
  (and (vector? v)
       (>= (vector-length v) 1)
       (symbol? (vector-ref v 0))))

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
 > (map (struct-of natural0?)
	'(foo
	  (foo)
	  #(foo)
	  #(foo 1)
	  #(1 foo)
	  #(foo 1 2)
	  #(foo 1 -2)
	  #(foo 1 "-2")))
 (#f #f #t #t #f #t #f #f))
