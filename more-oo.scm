;;; Copyright 2014 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require dot-oo)

;; * add class and subclass syntax

(compile-time

 ;; lib
 (define (box-push! b val)
   (set-box! b (cons val (unbox b))))
 ;;/lib
 
 (def compile-time:class-ctx '())

 ;; well, always fearing conflicts...
 (defstruct more-oo-class-ctx
   #((compose symbol? source-code) class-name)
   #(box? subclasses))
 
 (def (class=> cont)
      (if (pair? compile-time:class-ctx)
	  (cont (car compile-time:class-ctx))
	  (source-error stx "not placed within a |class| form"))))

(defmacro (compile-time#start-class! name subclass?)
  (if (source-code subclass?)
      (class=> (lambda (superclass)
		 (box-push! (.subclasses superclass) name))))
  (push! compile-time:class-ctx
	 (more-oo-class-ctx name (box '())))
  ;; Indirection to be able to define the predicate after the body of
  ;; the class but still read it at module load time for the dot-oo
  ;; dispatchers. HACK
  (with-gensym
   V
   `(def (,(source.symbol-append name "?") ,V)
	 (,(source.symbol-append "_more-oo_" name "?") ,V))))

(defmacro (compile-time#end-class!)
  (let* ((c (pop! compile-time:class-ctx))
	 (subclasses (unbox (.subclasses c))))
    ;; (warn "ending class:" (cj-desourcify c))
    (no-pp-through-source
     (if (null? subclasses)
	 `(begin)
	 `(def ,(source.symbol-append "_more-oo_" (.class-name c) "?")
	       (either ,@(map (C source.symbol-append _ "?")
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
				 `(def. (,(source.symbol-append class-name
								"." name)
					 ,@args)
				    ,@rest)))
		      (symbol?
		       `(def. ,(source.symbol-append class-name "." bind)
			  ,@rest)))))))


;; * add let.

;; In the tradition of |letv|, only accept one binding form.

(defmacro (let. vars+e . body)
  ;; heh, does such a great name call for speed-optimized access?
  (mcase vars+e
	 (`(`vars `e)
	  (with-gensym
	   V
	   `(##let ((,V ,e))
		   (##let ,(source-map (lambda (v)
					 (assert* symbol? v)
					 `(,v (,(source.symbol-append "." v)
					       ,V)))
				       vars)
			  ,@body))))))

(TEST
 > (let. ((string keyword) 'foo) (list string keyword))
 ("foo" foo:))
