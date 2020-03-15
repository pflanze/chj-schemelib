;;; Copyright 2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy-1
	 test
	 test-lib-1
	 cj-env
	 dot-oo
	 (cj-source-wraps source:symbol-append)
	 (string-util-2 string-contains?)
	 (oo-util string.symbol symbol.string))

(export (macro more-oo#class)
	(macro more-oo#subclass)
	(macro more-oo#struct)
	(macro more-oo#method)
	(macro let.)
	(macro let.-static)
	#!optional
	(struct more-oo-class-ctx)
	current-more-oo-class-ctx
	current-more-oo-class-name)

;; * add class and subclass syntax

(both-times ;; or would compile-time work ?

 (def compile-time:class-ctx '())

 ;; well, always fearing conflicts...
 (defstruct more-oo-class-ctx
   #((compose-function symbol? source-code) class-name)
   #(box? subclasses))
 
 (def (class=> stx cont)
      (if (pair? compile-time:class-ctx)
	  (cont (car compile-time:class-ctx))
	  (raise-source-error stx
                              "not placed within a |more-oo#class| form"))))

(defmacro (more-oo#start-class! name subclass?)
  (when (source-code subclass?)
        (class=> stx
                 (lambda (superclass)
                   (box-push! (.subclasses superclass) name))))
  (push! compile-time:class-ctx
	 (more-oo-class-ctx name (box '())))
  `(begin))


;; for macro expanders:
(def (current-more-oo-class-ctx) ;; -> more-oo-class-ctx
     (car compile-time:class-ctx))

(def (current-more-oo-class-name)
     (.class-name (current-more-oo-class-ctx)))

(defmacro (more-oo#end-class!)
  (let* ((c (pop! compile-time:class-ctx))
	 (subclasses (unbox (.subclasses c))))
    ;; (warn "ending class:" (cj-desourcify c))
    (no-pp-through-source
     (if (null? subclasses)
	 `(begin)
	 `(define ,(source:symbol-append (.class-name c) "?")
	    (%either ,@(map (C source:symbol-append _ "?")
			    subclasses)))))))

(defmacro (more-oo#class name . body)
  `(begin
     (more-oo#start-class! ,name #f)
     ,@body
     (more-oo#end-class!)))

(defmacro (more-oo#subclass name . body)
  ;;XXX + predicate constr
  `(begin
     (more-oo#start-class! ,name #t)
     ,@body
     (more-oo#end-class!)))



(defmacro (more-oo#struct . decls)
  (class=> stx
	   (lambda (subclass)
	     `(defstruct ,(.class-name subclass)
		,@decls))))

(defmacro (more-oo#method bind . rest)
  (class=> stx
	   (lambda (class)
	     (let ((class-name (.class-name class)))
	       (mcase bind
		      (pair?
		       (let-pair ((name args) (source-code bind))
				 `(def. (,(source:symbol-append class-name
								"." name)
					 ,@args)
				    ,@rest)))
		      (symbol?
		       `(def. ,(source:symbol-append class-name "." bind)
			  ,@rest)))))))


;; * add let.

;; In the tradition of |letv|, only accept one binding form.


(def (let-dot:var v)
     (let ((s (symbol.string v)))
       (if (string-contains? s ".")
	   ;; XX what if there are multiple dots? Still don't have a spec.
	   (string.symbol (last (string-split s #\.)))
	   v)))

(def (let-dot:accessor v)
     (if (string-contains? (symbol.string v) ".")
	 v
	 (source:symbol-append "." v)))

(TEST
 > (map (lambda (v) (list (let-dot:var v) (let-dot:accessor v)))
	'(foo
	  .bar
	  baz:.bar))
 ((foo .foo)
  (bar .bar)
  (bar baz:.bar)))


(def (let-dot:accessor/prefix v prefix)
     (let ((vstr (symbol.string v)))
       (if (or (string-contains? vstr ":")
	       (string-contains? vstr "."))
	   v ;; really? ":" as trigger for not prefixing the prefix?
	   (source:symbol-append
	    prefix
	    v))))

(TEST
 > (map (lambda (v)
	  (list (let-dot:var v)
		(let-dot:accessor/prefix v '@mypref.)))
	'(foo
	  .bar
	  baz:.bar))
 ((foo @mypref.foo)
  (bar .bar)
  (bar baz:.bar)))



(defmacro (let. var-or-accessors+e . body)
  ;; heh, does such a great name call for speed-optimized access?
  (mcase var-or-accessors+e
	 (`(`var-or-accessors `e)
	  (with-gensym
	   V
	   `(##let ((,V ,e))
		   (##let ,(source-map
			    (lambda (var-or-accessor)
			      (assert*
			       symbol? var-or-accessor
			       (lambda (sym)
				 `(,(let-dot:var sym)
				   (,(let-dot:accessor sym)
				    ,V)))))
			    var-or-accessors)
			  ,@body))))))

(TEST
 > (let. ((string keyword) 'foo) (list string keyword))
 ("foo" foo:)
 > (let ((foo:.bar .string))
     (let. ((foo:.bar) 'foo) bar))
 "foo")


(defmacro (let.-static prefix+var-or-accessors+e . body)
  (mcase prefix+var-or-accessors+e
	 (`(`prefix `var-or-accessors `e)
	  (assert* symbol? prefix
		   (lambda (prefix)
		     (with-gensym
		      V
		      `(##let ((,V ,e))
			      (##let ,(source-map
				       (lambda (var-or-accessor)
					 (assert*
					  symbol? var-or-accessor
					  (lambda (sym)
					    `(,(let-dot:var sym)
					      (,(let-dot:accessor/prefix sym prefix)
					       ,V)))))
				       var-or-accessors)
				     ,@body))))))))

(TEST
 > (def (t v)
	(let.-static (symbol. (string keyword) v)
		     (list string keyword)))
 > (t 'foo)
 ("foo" foo:)
 > (%try (t "foo"))
 (exception text: "(Argument 1) SYMBOL expected\n(symbol->string \"foo\")\n")
 > (let ((foo:.bar .string))
     (let.-static (balabala (foo:.bar) 'foo) bar))
 "foo")
