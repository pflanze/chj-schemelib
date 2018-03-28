;;; Copyright 2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.

(require easy
	 (cj-env symbol-append)
	 (simple-match-1 assert*)
	 dot-oo)

(export (macro dot-oo-optim-for)
	(macro dot-oo-optim-later)

	;; move to dot-oo and change it to not support foo:.bar any
	;; more?
	genericname-symbol?)


;; XX TODO: this assumes methodname is always starting with a
;; dot. Hence can't handle foo:.bar (give up on this?)

(def (genericname-symbol? v)
     (and (symbol? v)
	  (let* ((str (symbol->string v))
		 (len (string-length str)))
	    (and (> len 1)
		 (char=? (string-ref str 0) #\.)))))

(def (dot-oo-optim-for-expand genericname num-args)
     (assert*
      genericname-symbol? genericname
      (lambda (genericname)
	(assert*
	 natural? num-args
	 (lambda (num-args)
	   (let ((ARGS (map (lambda_ (gensym)) (iota num-args)))
		 (generic-name (symbol-append genericname "/"
					      (number->string num-args))))
	     `(define (,generic-name ,@ARGS)
		(cond ,@(map (lambda (entry)
			       (let ((type-name (car entry)))
				 `((,(symbol-append type-name "?") ,(car ARGS))
				   (,(symbol-append type-name genericname)
				    ,@ARGS))))
			     (dot-oo:show-method-table
			      (eval ;; <-- XX forever evil unsafe
			       (symbol-append "dot-oo-method-table#"
					      genericname))))
		      (else
		       (error ,(string-append (symbol->string generic-name)
					      ": no method for:")
			      ,(car ARGS)))))))))))



(defmacro (dot-oo-optim-for genericname num-args)
  (dot-oo-optim-for-expand genericname num-args))


;; instead of having to write (.ref/2 s 2), allow (% .ref s 2)
(defmacro (% genericname . args)
  (assert* genericname-symbol? genericname
	   (lambda (genericname)
	     (let ((len (length args)))
	       `(,(symbol-append genericname "/" (number->string len))
		 ,@args)))))

;; and then since libraries may have to wait until their own method
;; calls are optimized (late binding), we need a redefinable proxy:

(defmacro (dot-oo-optim-later genericname num-args)
  (assert*
   genericname-symbol? genericname
   (lambda (genericname)
     (assert*
      natural? num-args
      (lambda (num-args)
	(let ((ARGS (map (lambda_ (gensym)) (iota num-args)))
	      (generic-name (symbol-append genericname "/"
					   (number->string num-args)))
	      ;; -- XX wow up to here it's all COPY-PASTE from
	      ;; dot-oo-optim-for-expand; and the next few forms are
	      ;; almost COPY-PASTE from dot-oo.scm:
	      (method-table-name (generic-name-string.method-table-name
				  (symbol->string genericname))))
	  `(begin
	     (define ,method-table-name
	       (macro-symbol-value-or ,method-table-name
				      dot-oo:new-method-table))
	     (define-if-not-defined ,genericname
	       (dot-oo:make-generic ',genericname ,method-table-name))
	     ;;/ copy-paste

	     ;; The redefinable proxy (fallback to genericname):
	     (define (,generic-name ,@ARGS)
	       (,genericname ,@ARGS))
	     ;; make sure when |dot-oo-optim-for| is used to replace
	     ;; generic-name, it is seen even in block mode:
	     (set! ,generic-name ,generic-name))))))))
