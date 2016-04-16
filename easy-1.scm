;; short identifiers

(require define-macro-star
	 cj-env ;; identity ?, define-if-not-defined
	 cj-functional ;; compose
	 cj-struct
	 cj-typed
	 dot-oo ;; incl. define.
	 srfi-11
	 define-module
	 cj-match
	 simple-match ;; provided by cj-match ?
	 (cj-source-wraps source:symbol-append))

(define-macro* (& . args)
  ;; `(thunk ,@args)
  `(lambda () ,@args))

(define-macro* (defstruct . args)
  `(define-struct. ,@args))

(define-macro* (def first . rest)
  (if (pair? (source-code first))
      `(define-typed ,first ,@rest)
      `(define ,first ,@rest)))

(define-macro* (def. . args)
  `(define. ,@args))

;; adapted from cj-inline.scm to accept typing
(define-macro* (def-inline name+args body0 . body)
  (match-list*
   name+args
   ((name . args)
    (let* ((lambdacode
	    `(typed-lambda ,args
			   ,body0 ,@body))
	   (templatecode
	    `(,lambdacode
	      ,@(map (lambda (arg)
		       (list 'unquote (perhaps-typed.var arg)))
		     args))))
      (quasiquote-source
       (begin
	 (define-macro* (,(source:symbol-append name '-lambda))
	   ,(list 'quasiquote-source lambdacode))
	 (define-macro* (,name ,@(map perhaps-typed.var args))
	   ,(list 'quasiquote-source templatecode))))))))

(define-macro* (defenum name . args)
  `(define-enum ,name ,@args))

(define-macro* (defmacro . args)
  `(define-macro* ,@args))

(define-macro* (defvalues . args)
  `(define-values ,@args))

(define-macro* (defparameter . args)
  `(define-parameter ,@args))

(define-macro* (def-once . args)
  `(define-if-not-defined ,@args))

(def comp compose)
(defmacro (comp* . args)
  `(compose* ,@args))

(def id identity)

(defmacro (defmodule . args)
  `(define-module ,@args))

(def the xxone)

;; --  extension of define-module.scm ---

;; A new import form, that accepts expressions, but also prefixing iff
;; the expression is using a symbol that starts with a #\< as head,
;; and also (a new feature) allows to load pre-evaluated 'modules'
;; (module expressions?) (i.e. what a module exports when called)
;; stored in a symbol that is already available at macro expansion
;; time. (Hm. Good or bad? Could fail if that symbol was defined in
;; another place but re-defined in the current module; but that's evil
;; anyway, and future module system of mine (?) will/should do that
;; better anyway, ookay?)

(def (module-symbol? v)
     (and (symbol? v)
	  (let* ((s (symbol->string v))
		 (len (string-length s)))
	    (and (>= len 3)
		 (char=? (string-ref s 0) #\<)
		 (char=? (string-ref s (dec len)) #\>)
		 (string-any (lambda (v)
			       (not (or (char=? v #\<)
					(char=? v #\>))))
			     s)))))


(TEST
 > (map module-symbol? '(< <= <> <=> <foo>module <foo>))
 (#f #f #f #t #f #t))


;; check for exporter (runtime value)
(def (modimport-expand:exporterholder modsymbol export)
     (*if-symbol-value
      modsymbol
      (lambda (exporter)
	(if (procedure? exporter)
	    (export (exporter #f))
	    (source-error
	     modsymbol
	     (string-append
	      "modimport: expecting an exporter procedure"
	      " in module-holding symbol"))))
      (lambda ()
	(source-error
	 modsymbol
	 (string-append
	  "modimport: module-holding symbol does not"
	  " contain a value (yet?)")))))

;; check for -exports symbol (compiletime value)
(def (modimport-expand:mod maybe-prefix mod expr *else)
     (*if-symbol-value

      (source:symbol-append mod '-exports)

      (lambda (exports)
	(if maybe-prefix
	    `(module:import/prefix ,expr ,maybe-prefix ,@exports)
	    `(module:import ,expr ,@exports)))

      *else))


(def (modimport-expand maybe-prefix expr vars)
     (if (pair? vars)
	 (if maybe-prefix
	     `(module:import/prefix ,expr ,maybe-prefix ,@vars)
	     `(module:import ,expr ,@vars))

	 ;; No vars given, need to find the exports list from the
	 ;; module name with -exports appended, or in case of expr
	 ;; being a bare symbol, checking if it holds a value at
	 ;; expansion time already.
	 (mcase
	  expr

	  (symbol?
	   ;; Always assume that it is the result of a module
	   ;; parametrization (exporter); checking for mod-exports is
	   ;; pointless in this case, and dangerous (conflicts with
	   ;; other actual modules (previously defined ones, although
	   ;; admittedly that's a problem of the current (non-)module
	   ;; system)).
	   (modimport-expand:exporterholder
	    expr
	    (lambda (exports)
	      (if maybe-prefix
		  `(module:import/prefix ,expr ,maybe-prefix ,@exports)
		  `(module:import ,expr ,@exports)))))

	  (`(`mod . `args)
	   (if (module-symbol? (source-code mod))

	       (modimport-expand:mod
		maybe-prefix
		mod
		expr
		(&
		 (source-error
		  mod
		  (string-append
		   "modimport: can't find -exports entry"
		   " for this module symbol"))))

	       (source-error
		mod
		;; XX this was "if no prefix is given", hmm, which is
		;; true really?
		(string-append
		 "modimport requires syntax matching"
		 " `module-symbol?` here if no import list is given")))))))

(defmacro (modimport expr . vars)
  (if (keyword? (source-code expr))
      (source-error (second (source-code stx))
		    ;; expr has been stripped of source information,
		    ;; gah, for macro expander keyword functionality
		    (string-append
		     "modimport: can't take keyword as first argument "
		     "(did you mean to use |modimport/prefix|?)"))
      (modimport-expand #f expr vars)))

(defmacro (modimport/prefix prefix expr . vars)
  (modimport-expand prefix expr vars))

(TEST
 > (expansion#modimport tj8znc94e7fkdsqfm a b c)
 (module:import tj8znc94e7fkdsqfm a b c)
 ;; > (expansion#modimport tj8znc94e7fkdsqfm)
 > (with-exception-catcher source-error-message
			   (& (modimport-expand #f 'tj8znc94e7fkdsqfm '())))
 "modimport: module-holding symbol does not contain a value (yet?)"
 > (with-exception-catcher source-error-message
			   (& (modimport-expand #f '<tj8znc94e7fkdsqfm> '())))
 "modimport: module-holding symbol does not contain a value (yet?)"
 > (with-exception-catcher source-error-message
			   (& (modimport-expand #f '<tj8znc94e7fkdsqfm> '(a))))
 (module:import <tj8znc94e7fkdsqfm> a)
 > (with-exception-catcher source-error-message
			   (& (modimport-expand foo: '<tj8znc94e7fkdsqfm> '(a))))
 (module:import/prefix <tj8znc94e7fkdsqfm> foo: a)
 ;; a shadow definition, defines foo-exports :
 > (defmodule (foo x) (export bad guys) (def bad 1) (def guys 2))
 ;; the definition we want to use:
 > (defmodule (<foo> a b) (export bar baz) (def bar a) (def baz b))

 ;; <foo> is *not* an exporter, it's the module itself, which has to
 ;; be parametrized (XX although, is it really the case that we never
 ;; want to use defmodule for unparametrized modules?), thus this is
 ;; an error:
 > (with-exception-catcher wrong-number-of-arguments-exception?
			   (& (modimport-expand #f '<foo> '())))
 #t
 ;; correct:
 > (modimport-expand #f '(<foo> Y) '())
 (module:import (<foo> Y) bar baz)
 > (modimport-expand foo: '(<foo> Y) '())
 (module:import/prefix (<foo> Y) foo: bar baz)
 ;; defining evaluated module: (software, please do *not* take |foo-exports| !)
 > (def foo (<foo> 10 11))
 > (modimport-expand foo: '(<foo> 13 14) '())
 (module:import/prefix (<foo> 13 14) foo: bar baz)
 > (with-exception-catcher source-error-message
			   (& (modimport-expand foo: '(foo 13 14) '())))
 "modimport requires syntax matching `module-symbol?` here if no import list is given"
 > (modimport-expand foo: 'foo '())
 (module:import/prefix foo foo: bar baz))
