
(require cj-source
	 (cj-env define-if-not-defined)
	 (cj-io-util xbacktick)
	 fluid-let
	 ;;(test orig-load) or:
	 (cj-env gambit:load))


(define (random-filename)
  ;; XX not fork safe
  (number->string (random-integer #xffffffffffffffff) 16))


(define-if-not-defined compile-expr:basedir (##delay (xbacktick "mktemp" "-d")))


;; Compile an arbitrary expression. This is totally unsafe with
;; regards to spamming the global name space!

(define (compile-expr expr
		      #!key
		      already-sourcified?
		      (compile-options
		       ;; reuses global compile-options
		       compile-options))
  (let* ((expr* (if already-sourcified?
		    expr
		    ;; by default ensure that the compiler won't
		    ;; segfault
		    (cj-sourcify-deep expr expr)))
	 (path (string-append (force compile-expr:basedir)
			      "/"
			      (random-filename)))
	 (path.scm (string-append path ".scm")))

    ;; create the file as compile-file fails otherwise
    (close-port (open-output-file path.scm))
    
    (fluid-let ((c#expand-source (lambda (_expr)
				   expr*)))
      ;; XXX should get a mutex!
      (if (compile-file path.scm compile-options)
	  (gambit:load path)
	  (error "compiler error")))))



