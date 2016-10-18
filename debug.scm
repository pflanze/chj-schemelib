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


(def (debug:parse-level form want-marker? cont) ;; form/level/maybe-marker
     (def (with-level level form)
	  (if (and want-marker? (pair? form))
	      (let-pair ((b form*) form)
			(let ((B (source-code b)))
			  (if (string? B)
			      (cont form* level B)
			      (cont form level #f))))
	      (cont form level #f)))
     (if (pair? form)
	 (let-pair ((a form*) form)
		   (let ((A (source-code a)))
		     (if (real? A)
			 (with-level A form*)
			 (with-level 1 form))))
	 (cont form 1 #f)))

(TEST
 ;; DEBUG
 > (debug:parse-level '((a 1) b) #f list)
 (((a 1) b) 1 #f)
 > (debug:parse-level '("uh" (a 1) b) #f list)
 (("uh" (a 1) b) 1 #f)
 > (debug:parse-level '(3 "uh" (a 1) b) #f list)
 (("uh" (a 1) b) 3 #f)
 > (debug:parse-level '(3 (a 1) b) #f list)
 (((a 1) b) 3 #f)

 ;; T
 > (debug:parse-level '((a 1) b) #t list)
 (((a 1) b) 1 #f)
 > (debug:parse-level '("uh" (a 1) b) #t list)
 (((a 1) b) 1 "uh")
 > (debug:parse-level '(3 "uh" (a 1) b) #t list)
 (((a 1) b) 3 "uh")
 > (debug:parse-level '(3 (a 1) b) #t list)
 (((a 1) b) 3 #f))



;; DEBUG supports an optional level as the first arg (default: 1)
(defmacro (DEBUG . args)
  (if *debug*
      (debug:parse-level
       args #f
       (lambda (args level maybe-marker)
	 (assert (not maybe-marker))
	 `(if (and *debug* (<= *debug* ,level))
	      (warn ,@args))))
      `(##void)))


;; T supports an optional level then an optional marker string as the
;; first arg(s)
(defmacro (T . form)
  (if *debug*
      (debug:parse-level
       form #t
       (lambda (form level maybe-marker)
	 (with-gensym
	  res
	  (let ((vs (map (comp gensym .string) (cdr (iota (length form))))))
	    `(let ,(map (lambda (v arg)
			  `(,v ,arg))
			vs
			(cdr form))
	       (if (and *debug* (<= *debug* ,level))
		   (warn ,(if maybe-marker
			      (string-append "T " (source-code maybe-marker)":")
			      "T:")
			 ;;,(object->string (cj-desourcify (car form)))
			 (list
			  ',(car form)
			  ,@(map (lambda (v)
				   `(.show ,v))
				 vs))
			 '...))
	       (let ((,res (,(car form) ,@vs)))
		 (if (and *debug* (<= *debug* ,level))
		     (warn ,(if maybe-marker
			      (string-append "  " (source-code maybe-marker)":")
			      " :")
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
       form #t
       (lambda (form level maybe-marker)
	 form))))

(TEST
 > (both-times (def *debug* 2))
 > (T + 1 2)
 3
 > (T 3 + 2 3) ;; this one should make it to stderr
 5
 > (T 3 "ey" + 3 3) ;; dito
 6
 > (T "ey" + 3 4)
 7)


(def debug:default-port (current-error-port))

(def (2> #!optional #((maybe path-string?) path))
     (force-output (current-error-port))
     (if path
	 ;; XX O_APPEND ?
	 (current-error-port (open-output-file path))
	 (current-error-port debug:default-port)))

