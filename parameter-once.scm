;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 warn-plus
	 test)

(export (macro defparameter-once)
	(macro parameterize-once))


(def parameter-once:get-meta (box 'get-meta))
(def parameter-once:nothing (box 'nothing))

(def (parameter-once:make checker name)
     (let* ((param (make-parameter parameter-once:nothing))
	    (meta (values param checker name)))
       (lambda (#!optional (arg parameter-once:nothing))
	 (cond ((eq? arg parameter-once:nothing)
		(let ((v (param)))
		  (if (eq? v parameter-once:nothing)
		      (error "parameter is unset:" name)
		      v)))
	       ((eq? arg parameter-once:get-meta)
		meta)
	       (else
		(if (eq? (param) parameter-once:nothing)
		    (param (checker arg))
		    ;; This also disallows manual re-setting of
		    ;; a parameter, but perhaps that's fine and
		    ;; consistent with our purposes; really
		    ;; Erlang like set-once.
		    (error "parameter is already set:" name)))))))


(defmacro (defparameter-once name . rest)
  (let ((cont
	 (lambda (maybe-pred)
	   ;; wrapping maybe-pred in a function just to get the usual error
	   ;; view, including proper error location.
	   (with-gensym
	    V
	    `(define ,name (parameter-once:make
			    ,(if-let ((pred maybe-pred))
				     `(lambda ([,pred ,V]) ,V)
				     `identity)
			    ',name))))))
    (mcase rest
	   (`(-> `pred) (cont pred))
	   (`() (cont #f)))))


(defmacro (parameterize-once parm-once+vals . body)
  (assert*
   list? parm-once+vals
   (lambda (parm-once+vals*)
     (let ((parms* (.map parm-once+vals*
			 (mcase-lambda (`(`parm-once `val)
					(values parm-once
						val
						(gensym 'meta)
						(gensym 'parm)
						(gensym 'v)))))))
       ;; evaluate values first, less to retain
       `(let ,(.map parms*
		    (lambda-values
		     ((parm-once val META PARM V))
		     `(,V ,val)))
	  ;; get out meta data
	  (let ,(.map parms*
		      (lambda-values
		       ((parm-once val META PARM V))
		       `(,META
			 (@-> (values-of procedure? procedure? symbol?)
			      (,parm-once parameter-once:get-meta)))))
	    ;; get out parm since used multiple times
	    (let ,(.map parms*
			(lambda-values
			 ((parm-once val META PARM V))
			 `(,PARM (@fst ,META))))
	      ,@(.map parms*
		      (lambda-values
		       ((parm-once val META PARM V))
		       (with-gensyms
			(OLDV NAME)
			`(let ((,OLDV (,PARM)))
			   (when (not (eq? ,OLDV parameter-once:nothing))
                                 (let ((,NAME (3rd ,META)))
                                   (if (eq? ,OLDV ,V)
                                       (WARN-ONCE "parameter set to identical value again"
                                                  ,NAME)
                                       (error "parameter already set" ,NAME))))))))
	      (parameterize ,(.map parms*
				   (lambda-values
				    ((parm-once val META PARM V))
				    `(,PARM ((@snd ,META) ,V))))
			    ,@body))))))))


(TEST
 > (defparameter-once foo -> natural?)
 > (%try-error (foo))
 [error "parameter is unset:" foo]
 > (foo 1)
 > (foo)
 1
 > (%try-error (foo 1.4))
 [error "parameter is already set:" foo]
 > (defparameter-once foo -> natural?)
 > (%try-error (foo 1.4))
 [error "gensym '\"V\" does not match natural?:" 1.4]
 ;; ^ would it be nice to just say argument 1 or so, in this case?
 > (%try-error (parameterize ((foo 15)) (foo)))
 [error "parameter is unset:" foo]
 ;; ^ Heh, accidental but good, disallows use of |parameterize| on
 ;; these. Avoids the need for me to override |parameterize| and
 ;; perhaps also implement function tagging.
 > (parameterize-once ((foo 16)) (foo))
 16
 > (foo 17)
 > (%try-error (parameterize-once ((foo 15)) (foo)))
 [error "parameter already set" foo]
 > (defparameter-once foo -> natural?)
 > (parameterize-once ((foo 15)) (foo))
 15
 > (%try-error (parameterize-once ((foo 15.1)) (foo)))
 [error "gensym '\"V\" does not match natural?:" 15.1]

 ;; And the main aim for writing this library:
 > (defparameter-once bar)
 > (parameterize-once ((bar 20))
		      (parameterize-once ((bar 20))
					 (bar)))
 ;; issues *** WARNING IN ".../parameter-once.scm"@137.9 -- #19 -- parameter set to identical value again: bar
 ;; ^ how to test for this?. XX Make a testing mode for warns.
 20
 > (%try-error (parameterize-once ((bar 20))
				  (parameterize-once ((bar 30))
						     (bar))))
 [error "parameter already set" bar])

