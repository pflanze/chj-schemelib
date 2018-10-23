;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require class
	 (cj-typed-1 (mutable cj-typed-1:error?)
		     (mutable cj-typed-1:.string)))

(export (interface error)
	error+?
	#!optional
	error#exception?
	)



;; Base trait/interface for all exception/error values.

(definterface error

  ;; human-readable string representation that explains what the error
  ;; is about in a nice way that's presentable to a user, OK?
  (method (string s))

  ;; Also require .show ?

  ;; Also, .sxml ?

  ;; What about .arguments, .message, .procedure like Gambit's ones?

  ;; What about special location types, in the structured report
  ;; (instead of sxml, or, (css-class?-)enriched sxml?)? Or just one,
  ;; about the datum in question? Why does Gambit have .arguments but
  ;; not .value ? More general, ok, unlike just the cj-typed thing(?)
  ;; (e.g. IO routines depending on path *and* permission arguments).
  )

;; Note that |error?| now doesn't return true for Gambit's own error
;; objects. Use |error+?| for that.

;; TODO: avoid calling .string on error objects then throwing that as
;; error-exception ones, like cj-typed currently does! (Only do the
;; .string at the UI boundary, i.e. repl, web, etc.)


;; let cj-typed know about us
(set! cj-typed-1:error? error?)
(set! cj-typed-1:.string .string)


;; also allow all of 'the other' (Gambit, modules?) error/exception
;; types; careful: they all need to implement the methods from the
;; error interface, too!


;; XX move to gambit-error, hack just to get error#exception?
;; predicate
(define-type error#exception
  id: 0bf9b656-b071-404a-a514-0fb9d05cf518
  constructor: #f
  extender: define-library-type-of-exception
  opaque:)


(def error+?
     (either error?
	     error#exception?))


(TEST
 > (def e (with-exception-catcher
	   identity (& (open-input-file "oqwuiavuosviue"))))
 > (error+? e)
 #t
 > (error? e)
 #f
 > (error#exception? e)
 #t
 > (no-such-file-or-directory-exception? e)
 #t)


(compile-time
 (def (error:define-library-type-of-exception-expand
       name
       #!key
       id
       constructor
       opaque ;; sigh, :opaque kind of flag, meh, have to give it
       #!rest
       r)

      (def (cont defs)
	   (let ((fieldnames (map car defs))
		 (accessor
		  (lambda (fieldname)
		    (symbol-append name "-" fieldname)))
		 (S (gensym)))

	     `(begin
		;; hacked constructor, wow?
		(def (,name ,@fieldnames)
		     ;; aha, allocate a structure of the required
		     ;; length, how? also, how to find #<type #34
		     ;; error-exception> etc.? Is it in a global?
		     (error "unfinished"))
		    
		(def. (,(symbol-append name '.show) ,S)
		  (list ',name
			,@(map (lambda (fieldname)
				 `(.show (,(accessor fieldname) ,S)))
			       fieldnames))))))
      
      (if opaque
	  (cont (cons opaque r))
	  (if (eq? (car r) opaque:)
	      (cont (cdr r))
	      (cont r)))))

(define-macro (define-library-type-of-exception . args)
  (apply error:define-library-type-of-exception-expand args))

(include "gambit-error--include.scm")

;; error-exception is missing in there ^, but error-exception.show is
;; already defined elsewhere. XX clean up this mess.


(TEST
 > (with-exception-catcher .show (& (error "hum f" #f)))
 (error "hum f" #f) ;; XX bad, value vs throwing, FIX this!
 > (with-exception-catcher .show (& (car "hum f" #f)))
 (wrong-number-of-arguments-exception car (list "hum f" #f)))

