;;; Copyright 2014 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy-1
	 cj-env
	 dot-oo
	 (cj-source-wraps source:symbol-append))

(export (macro class)
	(macro subclass)
	(macro struct)
	(macro method)
	(macro let.)
	#!optional
	more-oo:current-class)

;; * add class and subclass syntax

(compile-time

 (def compile-time:class-ctx '())

 ;; well, always fearing conflicts...
 (defstruct more-oo-class-ctx
   #((compose symbol? source-code) class-name)
   #(box? subclasses))
 
 (def (class=> cont)
      (if (pair? compile-time:class-ctx)
	  (cont (car compile-time:class-ctx))
	  (source-error compile-time:class-ctx
			"not placed within a |class| form"))))

(defmacro (compile-time#start-class! name subclass?)
  (if (source-code subclass?)
      (class=> (lambda (superclass)
		 (box-push! (.subclasses superclass) name))))
  (push! compile-time:class-ctx
	 (more-oo-class-ctx name (box '())))
  `(begin))


;; for macro expanders:
(def (more-oo:current-class)
     (car compile-time:class-ctx))


(defmacro (compile-time#end-class!)
  (let* ((c (pop! compile-time:class-ctx))
	 (subclasses (unbox (.subclasses c))))
    ;; (warn "ending class:" (cj-desourcify c))
    (no-pp-through-source
     (if (null? subclasses)
	 `(begin)
	 `(define ,(source:symbol-append (.class-name c) "?")
	    (%either ,@(map (C source:symbol-append _ "?")
			    subclasses)))))))

(defmacro (class name . body)
  `(begin
     (compile-time#start-class! ,name #f)
     ,@body
     (compile-time#end-class!)))

(defmacro (subclass name . body)
  ;;XXX + predicate constr
  `(begin
     (compile-time#start-class! ,name #t)
     ,@body
     (compile-time#end-class!)))



(defmacro (struct . decls)
  (class=> (lambda (subclass)
	     `(defstruct ,(.class-name subclass)
		,@decls))))

(defmacro (method bind . rest)
  (class=> (lambda (class)
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
