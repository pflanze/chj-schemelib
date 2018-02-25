;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require cj-source
	 (cj-env define-if-not-defined)
	 (cj-io-util xbacktick)
	 fluid-let
	 ;;(test orig-load) or:
	 (cj-env gambit:load)
	 cj-source-quasiquote
	 jclass)

(export (jclass compiled-source
		compiled-expression)
	compile-source
	compile-expression
	(macro compiled-expression)
	#!optional)


(jclass (compiled-source [string? basename]
			 [string? object-path])

	(def-method (load! s)
	  (gambit:load object-path))

	(def-method (module-symbol s)
	  ;; XX safer would be to strip the .oX suffix and directory
	  ;; part from object-path
	  (string->symbol (string-append " " bn ".o1")))

	;; Run the module's body; load! already does that implicitly
	(def-method (re-init! s)
	  ((eval (.module-symbol s))))
	
	(def-method (compiled-expression s namesym)
	  (compiled-expression basename object-path namesym))

	
	(jclass (compiled-expression [symbol? namesym])

		(def-method (eval s)
		  ((symbol-value-or namesym
				    (lambda ()
				      (.load! s)
				      (eval namesym)))))))



(define (random-filename)
  ;; XX not fork safe
  (number->string (random-integer #xffffffffffffffff) 16))

(define (compile:random-name)
  (string-append "compile-" (random-filename)))

(define-if-not-defined compile:basedir
  (##delay
   (let ((path (path-expand "~/.cj-gambit-compile-cache")))
     (if (not (file-exists? path))
	 (xbacktick "mkdir" "-m" "0700" path))
     path)))


;; Compile an arbitrary expression. This is totally unsafe with
;; regards to spamming the global name space! Does not load the object
;; file.

(define (compile-source expr
			#!key
			(name (compile:random-name))
			already-sourcified? ;; #t is unsafe
			(compile-options
			 ;; reuses global compile-options
			 compile-options))
  (let* ((expr* (if already-sourcified?
		    expr
		    ;; by default ensure that the compiler won't
		    ;; segfault
		    (cj-sourcify-deep expr expr)))
	 (path (string-append (force compile:basedir)
			      "/"
			      name))
	 ;; ah then should return the full path anyway? XX race issue
	 ;; (well not even checking)
	 (path.o1 (string-append path ".o1"))
	 (c (compiled-source name path.o1))
	 (path.scm (string-append path ".scm")))

    ;; create the file as compile-file fails otherwise
    (close-port (open-output-file path.scm))
    
    (if (fluid-let ((c#expand-source (lambda (_expr)
				       expr*)))
	  ;; XXX should get a mutex!
	  (compile-file path.scm compile-options))
	(begin
	  (with-exception-catcher values (lambda () (delete-file path.scm)))
	  c)
	(error "compiler error"))))


;; Does not spam the global name space (except for the random-name
;; symbol). Does not load or run the code, run |compilation-value|
;; afterwards.

(define (compile-expression expr
			    #!key
			    compile-options)

  (let* ((name (compile:random-name))
	 (namesym (string->symbol name))
	 (c
	  (compile-source (quasiquote-source
			   (define ,namesym (lambda () ,expr)))
			  compile-options: compile-options
			  name: name)))
    (.compiled-expression c namesym)))


(define-macro* (compiled-expression expr)
  `(.eval (compile-expression (source-dequote ',(source-quote expr)))))

