;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require class
	 (cj-typed-1 (mutable cj-typed-1:error?)
		     (mutable cj-typed-1:.string))
	 gambit-error)

(export (interface error)
	error+?)



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


;; (def. (exception.show e)
;;   `(raise ,e))
;; oh {##,}exception? doesn't exist

(define. (error-exception.show e)
  `(error-exception ,(.show (error-exception-message e))
		    ,(.show (error-exception-parameters e))))

(TEST
 > (.show (error-exception 1 '(2 3)))
 (error-exception 1 (list 2 3))
 > (with-exception-catcher .show (& (error "foo" "bar" 2)))
 (error-exception "foo" (list "bar" 2)))


;; (define (unbound-global-exception var)
;;   (error "(XX unbound-global-exception not implemented)" var))

;; (define. (unbound-global-exception.show e)
;;   ;;`(unbound-global-exception )  hmm or really simply?:
;;   `(unbound-global-exception ',(unbound-global-exception-variable e)))


;; (define (no-such-file-or-directory-exception procedure arguments)
;;   ;; need to use Gambit header file, looks like it's macro only
;;   (error "XX no-such-file-or-directory-exception not implemented"))

;; (define. (no-such-file-or-directory-exception.show e)
;;   `(no-such-file-or-directory-exception
;;     ,(.show (no-such-file-or-directory-exception-procedure e))
;;     ,(.show (no-such-file-or-directory-exception-arguments e))))


;; (define (type-exception procedure
;; 			arguments
;; 			arg-num
;; 			type-id)
;;   ;; need to use Gambit header file, looks like it's macro only
;;   (error "XX no-such-file-or-directory-exception not implemented"))

;; (define. (type-exception.show e)
;;   `(type-exception
;;     ,(.show (type-exception-procedure e))
;;     ,(.show (type-exception-arguments e))
;;     ,(.show (type-exception-arg-num e))
;;     ,(.show (type-exception-type-id e))))





(def error+?
     (either error?
	     gambit-error#exception?))


(TEST
 > (def e (with-exception-catcher
	   identity (& (open-input-file "oqwuiavuosviue"))))
 > (error+? e)
 #t
 > (error? e)
 #f
 > (gambit-error#exception? e)
 #t
 > (no-such-file-or-directory-exception? e)
 #t)

