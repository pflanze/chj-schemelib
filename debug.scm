;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version. Also, as a special exception to the
;;;    terms of the GPL you can link it with any code produced by Categorical
;;;    Design Solutions Inc. from Quebec, Canada.

(require easy)

(export (macro DEBUG)
	(macro T)
	*debug* ;; well, by alias?
	2>)

(def *debug* 2) ;; statements below that level remain quiet; #f means
		 ;; don't compile debugging statements into the code
		 ;; at all


(def (debug:parse-level lvl rest cont)
     (let ((level (source-code lvl)))
       (if (real? level)
	   (cont rest level)
	   (cont (cons lvl rest) 1))))


;; lvl is optional, default is 1

(defmacro (DEBUG lvl . rest)
  (if *debug*
      (debug:parse-level
       lvl rest
       (lambda (args level)
	 `(if (and *debug* (<= *debug* ,level))
	      (warn ,@args))))
      `(##void)))


(defmacro (T lvl . rest)
  (if *debug*
      (debug:parse-level
       lvl rest
       (lambda (form level)
	 (with-gensym
	  res
	  (let ((vs (map (comp gensym .string) (cdr (iota (length form))))))
	    `(let ,(map (lambda (v arg)
			  `(,v ,arg))
			vs
			(cdr form))
	       (if (and *debug* (<= *debug* ,level))
		   (warn "T:"
			 ;;,(object->string (cj-desourcify (car form)))
			 (list
			  ',(car form)
			  ,@(map (lambda (v)
				   `(.show ,v))
				 vs))
			 '...))
	       (let ((,res (,(car form) ,@vs)))
		 (if (and *debug* (<= *debug* ,level))
		     (warn " :"
			   ;;,(object->string (cj-desourcify (car form)))
			   (list
			    ',(car form)
			    ,@(map (lambda (v)
				     `(.show ,v))
				   vs))
			   '->
			   (.show ,res)))
		 ,res))))))
      (debug:parse-level
       lvl rest
       (lambda (form)
	 form))))

(TEST
 > (both-times (def *debug* 2))
 > (T + 1 2)
 3
 > (T 3 + 2 3) ;; this one should make it to stderr
 5)


(def debug:default-port (current-error-port))

(def (2> #!optional #((maybe path-string?) path))
     (force-output (current-error-port))
     (if path
	 ;; XX O_APPEND ?
	 (current-error-port (open-output-file path))
	 (current-error-port debug:default-port)))

