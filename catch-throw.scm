;;; Copyright 2014 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Non-local exit syntax experiments.

;; Originally inspired by throw and catch in "Scheme: A Interpreter
;; for Extended Lambda Calculus" (Sussman and Steele 1975), but using
;; dynamic scoping (parameter values) and extending |catch| to accept
;; handlers.


(require easy ;; does that include cj-env, btw?
	 ;;cj-source-util-2
	 ;;source-wraps  hmm wanted a source-fold-right
	 cj-symbol
	 cj-match)

;; XX move to some lib?
(def (wrap-continuation c)
     (lambda vs
       (apply continuation-return c vs)))

(TEST
 > (continuation-capture (lambda (c) ((wrap-continuation c) 1) 2))
 1)

;; /lib


(defmacro (defexception nam msg)
  `(def ,nam (make-parameter (lambda vals ;; *not* (cont . vals)
			       (apply error ,msg vals)))))

(def (throw param . args)
     (apply (param) args))


(def (catch/handle-expand nam+handler-s body exit-code handle-code)
     (assert*
      list? nam+handler-s
      (lambda (nam+handler-s)
	(with-gensyms
	 (C Vs)
	 `(continuation-capture
	   (lambda (,C)
	     (parameterize
	      ,(map (lambda (nam+handler)
		      (let ((format (lambda (nam code)
				      `(,nam (lambda ,Vs
					       ,code)))))
			(mcase nam+handler
			       (symbol?
				;; no handler, just exit scope
				(format nam+handler (exit-code Vs C)))
			       (`(`nam `handler)
				(format nam (handle-code Vs C handler))))))
		    nam+handler-s)
	      ,@body)))))))


;; |handle| sets up the handlers so that they are run in the context
;; of the error condition. They are passed the exit continuation as
;; the first argument, and have to call it to leave the scope. Normal
;; returns will resume the error context with the returned value.

(defmacro (handle nam+handler-s . body)
  (catch/handle-expand nam+handler-s body
		       (lambda (Vs C)
			 `(apply continuation-return ,C ,Vs))
		       (lambda (Vs C handler)
			 `(apply ,handler
				 (wrap-continuation ,C)
				 ,Vs))))


;; |catch| does not allow to resume conditions: the handlers are run
;; in the context of the catch form, with the error condition context
;; lost.

(defmacro (catch nam+handler-s . body)
  (catch/handle-expand nam+handler-s body
		       (lambda (Vs C)
			 `(apply continuation-return ,C ,Vs))
		       (lambda (Vs C handler)
			 `(apply continuation-graft ,C ,handler ,Vs))))

(TEST
 > (defexception foo "we are fooed")
 > (%try-error (throw foo 77))
 #(error "we are fooed" 77)
 > (handle ((foo vector)) 1)
 1
 > (handle ((foo (lambda (c val)
		   (c (vector 'that val)))))
	   (throw foo 77)
	   88)
 #(that 77)
 > (handle (foo)
	   (+ 5 (throw foo -1) 2))
 -1
 > (handle ((foo (lambda (c val)
		   val)))
	   (+ 5 (throw foo -1) 2))
 6
 > (catch ((foo identity))
	   (+ 5 (throw foo -1) 2))
 -1
 > (handle ((foo (lambda (c val)
		   (if (< val 100)
		       (throw foo (* val val))
		       val))))
	   (+ 5 (throw foo -2) 2))
 263 ;; (+ 5 256 2)
 )


;; Well, for efficiency (and simplicity in simple cases) avoiding the
;; parameter values makes sense: we're back at the original form from
;; 1975.  But why call it catch? What about?:

(defmacro (with-exit var . body)
  `(call/cc (lambda (,var)
	      ,@body)))

(TEST
 > (with-exit foo
	      (+ 5 (foo -1) 2))
 -1)

