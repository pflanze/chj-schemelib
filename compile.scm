;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; BUGS:

;; * recompiles the same code again once (or more?) until caching
;;   comes in, because the same code yields different hashes (because
;;   presumably the serialization is different, because presumably the
;;   sharing of the source pointers is, while equal?-same,
;;   eq?-different). Not sure what to do about this.

;; * somehow the compiled code, instead of (options: (debug
;;   track-scheme) cc-options: "-g"), does not seem to remember source
;;   location, wtf?

;; * perhaps md5 should be replaced with a (randomly, per-user
;;   installation) salted sha-256 (as the name is showing up in error
;;   messages)

(require cj-source
	 (cj-env define-if-not-defined)
	 (cj-io-util xbacktick)
	 fluid-let
	 ;;(test orig-load) or:
	 (cj-env gambit:load)
	 cj-source-quasiquote
	 jclass
	 md5
	 srfi-11
	 (cj-u8vector-util write-u8vector)
	 (predicates-1 string-of-length))

(export (jclass compiled-source
		compiled-expression)
	compile-source
	compile-expression
	(macro compiled-expression)
	#!optional)


(def md5-digest-string? (string-of-length 32))

(jclass (compiled-source [md5-digest-string? hash]
			 [path-string? basedir])

	(def-method (path s)
	  (path-append basedir hash))
	(def-method (object-path s)
	  ;; XX blindly assume .o1
	  (string-append (.path s) ".o1"))
	(def-method (u8vector-path s)
	  (string-append (.path s) ".u8vector1"))
	(def-method (scm-path s)
	  (string-append (.path s) ".scm"))
	
	(def-method (load! s)
	  (gambit:load (.object-path s)))

	(def-method (module-symbol s)
	  (string->symbol (string-append " " bn ".o1")))

	;; Run the module's body; load! already does that implicitly
	(def-method (re-init! s)
	  ((eval (.module-symbol s))))

	(def-method (source s)
	  (u8vector->object
	   (read-u8vector-from-file
	    (.u8vector-path s))))
	
	(def-method (compiled-expression s namesym)
	  (compiled-expression hash basedir namesym))

	
	(jclass (compiled-expression [symbol? namesym])

		(def-method (eval s)
		  ((symbol-value-or namesym
				    (lambda ()
				      (.load! s)
				      (eval namesym)))))))



(define-if-not-defined compile:basedir
  (##delay
   (let ((path (path-expand "~/.cj-gambit-compile-cache")))
     (if (not (file-exists? path))
	 (xbacktick "mkdir" "-m" "0700" path))
     path)))


(define (compile:source.expr*+bin+hash expr already-sourcified?)
  (let* ((expr* (if already-sourcified?
		    expr
		    ;; ensure that the compiler won't segfault
		    (cj-sourcify-deep expr expr)))
	 (exprbin (object->u8vector expr*)))
    (values expr*
	    exprbin
	    (md5:digest exprbin))))


(define compile:cache-counter 0)
(define compile:compilation-counter 0)

(define _compile-source-u8vector

  (lambda-values
   ((expr*
     exprbin
     hash)
    compile-options)

   (let ((c (compiled-source hash (force compile:basedir))))
     (let. ((object-path u8vector-path scm-path) c)

	   (if (file-exists? object-path)
	       (begin
		 (inc! compile:cache-counter)
		 c)
	       (begin
		 ;; create the .scm file path as compile-file fails
		 ;; otherwise (it reads what's in it, so leave it empty for
		 ;; speed)
		 (close-port (open-output-file scm-path))
		 ;; save the u8vector just in case it needs to be retrieved
		 ;; later
		 (let ((p (open-output-file u8vector-path)))
		   (write-u8vector exprbin p)
		   (close-port p))
    
		 (if (fluid-let ((c#expand-source (lambda (_expr)
						    expr*)))
		       ;; XXX should get a mutex!
		       (compile-file scm-path compile-options))
		     (begin
		       (inc! compile:compilation-counter)
		       (with-exception-catcher
			values
			(lambda () (delete-file scm-path)))
		       c)
		     (error "compiler error"))))))))


;; Compile an arbitrary expression. This is totally unsafe with
;; regards to spamming the global name space! Does not load the object
;; file.

(define (compile-source expr
			#!key
			already-sourcified? ;; #t is unsafe
			(compile-options
			 ;; reuses global compile-options
			 compile-options))

  (_compile-source-u8vector (compile:source.expr*+bin+hash expr
							   already-sourcified?)
			    compile-options
			    compiled-source))


;; Does not spam the global name space (except for the random-name
;; symbol). Does not load or run the code, run |compilation-value|
;; afterwards.

(define (compile-expression expr
			    #!key
			    already-sourcified? ;; #t is unsafe
			    (compile-options
			     ;; reuses global compile-options
			     compile-options))

  (letv ((expr* _bin hash) (compile:source.expr*+bin+hash
			    expr
			    already-sourcified?))
	(let ((namesym (string->symbol hash))) ;; XX prefix it?
	
	  (.compiled-expression
	   (_compile-source-u8vector
	    (compile:source.expr*+bin+hash (quasiquote-source
					    (define ,namesym (lambda () ,expr*)))
					   #t)
	    compile-options)
	   namesym))))



;; careful, needs to be an expression, i.e. not everything will work
;; due to stupid Scheme version with regards to local defines. Could
;; use define-module but that's something to use explicitly, OK?
(define-macro* (compiled expr)
  `(.eval (compile-expression (source-dequote ',(source-quote expr)))))
;;XX rename source-dequote to dequote-source ?

