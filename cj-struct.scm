;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (lib.define-macro-star)
	 (lib.test)
	 (lib.simple-match)
	 (lib.cj-env);; symbol-append
	 )


(define-macro* (define-struct
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
		 predicate-name
		 let*-name
		 let-name
		 tag
		 #!rest args*)
  (let* ((name (source-code name*))
	 (tag (if (source-code tag) tag name*))
	 (fields* (filter (lambda (v*)
			    (let ((v (source-code v*)))
			      (cond ((symbol? v)
				     #t)
				    ((meta-object? v)
				     #f)
				    (else
				     (source-error
				      v*
				      "expecting symbol or meta-object")))))
			  args*))
	 (fields (map source-code fields*))
	 (varsargs (map (lambda (v)
			  (let ((v (source-code v)))
			    (if (symbol? v)
				(symbol-append "var-" v)
				v)))
			args*))
	 (vars (filter symbol? varsargs))
	 ;; keyed arguments:
	 (prefix (source-code prefix))
	 (accessor-prefix (source-code accessor-prefix))
	 (unsafe-accessor-prefix (source-code unsafe-accessor-prefix))
	 (separator (source-code separator))
	 (constructor-name
	  (if constructor-name
	      (source-code constructor-name)
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
       (define ,constructor-name
	 (lambda ,varsargs
	   (vector ',tag
		   ,@vars)))
       (define ,predicate-name
	 (lambda (v)
	   (and (vector? v)
		(= (vector-length v) ,(add-offset numfields))
		(eq? (vector-ref v 0)
		     ',tag))))
       (define ,error-name
	 (lambda (v)
	   (error ,(string-append "expecting a "
				  (symbol->string name)
				  ", got:")
		  v)))
       (define ,genericsetter-name
	 (lambda (v offset val)
	   (if (,predicate-name v)
	       (let ((v* (##vector-copy v)))
		 (vector-set! v* offset val)
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
       ,@(map (lambda (field i)
		`(begin
		   (define ,(safe-accessor-for-field field)
		     (lambda (v)
		       (if (,predicate-name v)
			   (vector-ref v ,(add-offset i))
			   (,error-name v))))
		   ;; unsafe version:
		   (define ,(symbol-append prefix
					   unsafe-accessor-prefix
					   name
					   separator
					   field)
		     (lambda (v)
		       (vector-ref v ,(add-offset i))))
		   ;; functional setter:
		   (define ,(safe-setter-for-field field)
		     (lambda (v val)
		       ;; use shared code
		       (,genericsetter-name v ,(add-offset i) val)))
		   ;; functional updater:
		   (define ,(safe-updater-for-field field)
		     (lambda (v fn)
		       ;; use shared code
		       (,genericupdater-name v ,(add-offset i) fn)))))
	      fields
	      (iota numfields))
       (define-macro* (,let*-name vars+inp-s . body)
	 (let* (
		;; copies of the accessed parts of the outer expander's context:
		(offset ',offset)
		(prefix ',prefix)
		(accessor-prefix ',accessor-prefix)
		(name ',tag)
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
 > (define-struct foo tag: myvery:foo a b)
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
 )

;; omit the |make-| prefix for the constructor name
(define-macro* (define-struct* name . defs)
  `(define-struct ,name constructor-name: ,name ,@defs))


;; omit the |make-| prefix for the constructor name; use "." as
;; separator.
(define-macro* (define-struct. name . defs)
  `(define-struct ,name
     ;; don't override constructor-name if provided by user
     ,@(if (memq constructor-name: (map source-code defs))
	   `()
	   `(constructor-name: ,name))
     separator: "."
     ,@defs))

