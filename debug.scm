;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version. Also, as a special exception to the
;;;    terms of the GPL you can link it with any code produced by Categorical
;;;    Design Solutions Inc. from Quebec, Canada.

(require easy
	 (cj-warn warn
		  port-add-hook!))


(export (macro DEBUG)
	(macro DEBUG*)
	(macro T)

	*debug* ;; well, by alias? Hey, have syntax that sets compile
		;; time variables scoped by the compilation unit?
	2>
	2force

	warn-stop-on-line!
	;; UNTESTED and useless?
	(macro STOP-at-line)
	(inline debug:stop-at-line))


;; statements below that level remain quiet; #f means don't compile
;; debugging statements into the code at all
(def *debug* 2)

;; whether
(def *debug/continuations* #t)


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



;; DEBUG supports an optional level as the first arg (default:
;; 1). Side-effect only, returns (void) in all cases!
(defmacro (DEBUG . args)
  (if *debug*
      (debug:parse-level
       args #f
       (lambda (args level maybe-marker)
	 (assert (not maybe-marker))
	 `(if (and *debug* (<= *debug* ,level))
	      (warn ,@args))))
      `(##void)))

;; Variant of DEBUG that returns the values args evaluate to after
;; dropping a static string at the start of args (regardless of
;; whether debugging is active or not).

(def (debug:perhaps-warn/cont level maybe-cont maybe-msg vals)
     (if (and *debug* (<= *debug* level))
	 (let ((msg (if maybe-cont
			(let ((cstr (object->string maybe-cont)))
			  (if maybe-msg
			      (string-append cstr " " maybe-msg)
			      cstr))
			(or maybe-msg ""))))
	   (apply warn msg vals))))


(def (debug:continuation-capture recv)
     (if *debug/continuations*
	 (continuation-capture recv)
	 (recv #f)))

(defmacro (DEBUG* . args)
  (debug:parse-level
   args #t
   (lambda (args level maybe-marker)
     (let ((vars (map (lambda_ (gensym))
		      (iota (length args)))))
       (let ((exprs (if maybe-marker (cons maybe-marker args) args))
	     (cont

	      (lambda (maybe-msg real-exprs)
		(let ((C (gensym)))
		  `(debug:continuation-capture
		    (lambda (,C) (let ,(map list vars args)
			      (debug:perhaps-warn/cont
			       ,level ,C ,maybe-msg (##list ,@vars))
			      ,(if (one? vars) (car vars)
				   `(values ,@vars)))))))))

	 (if (and (pair? exprs)
		  (string? (source-code (car exprs))))
	     (cont (car exprs)
		   (cdr exprs))
	     (cont #f
		   exprs)))))))

(TEST
 > (def *debug* 2)
 > (def x 123) 
 > (DEBUG* x)
 123
 > (DEBUG* 1 x)
 123
 > (DEBUG* 0 x)
 123
 > (DEBUG* 2 x)
 ;; prints: #<continuation #4> 123
 123
 > (DEBUG* "x" x)
 123
 > (DEBUG* 2 "x" x)
 ;; prints: #<continuation #6> x 123
 123)


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

;; 'just a utility', to flush the port of an ongoing thing that used |2>|
(def (2force)
     (force-output (current-error-port)))


;; inline just to avoid ending up here? wait, happens anyway right?
(def-inline (debug:stop-at-line n)
  (let ((m (output-port-line (current-error-port))))
    (if (>= m n)
	(error "reached error-port line " m n))))
;; UNTESTED
(defmacro (STOP-at-line n)
  (assert* natural? n
	   (lambda (n)
	     (if *debug*
		 `(if *debug*
		      (debug:stop-at-line ,n))
		 `(##void)))))

;; better:
(def (warn-stop-on-line! n)
     (port-add-hook! (current-error-port)
		     (lambda (port)
		       (let ((m (output-port-line (current-error-port))))
			 (if (= m n)
			     (error "reached error-port line " n))))))
