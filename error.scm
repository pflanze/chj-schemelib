;;; Copyright 2018-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require class
	 (cj-typed-1 (mutable cj-typed-1:error?)
		     (mutable cj-typed-1:.string))
	 gambit-error)

(export (interface error-interface)
        (methods error-exception.show
                 datum-parsing-exception.show)
	error?)



;; Base trait/interface for all exception/error values.

(definterface error-interface

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

;; TODO: avoid calling .string on error objects then throwing that as
;; error-exception ones, like cj-typed currently does! (Only do the
;; .string at the UI boundary, i.e. repl, web, etc.)



;; Allow all of 'the other' (Gambit, modules?) error/exception types;
;; careful: they all need to implement the methods from the error
;; interface, too!

(define. (error-exception.show e)
  `(error-exception ,(.show (error-exception-message e))
		    ,(.show (error-exception-parameters e))))

(TEST
 > (.show (error-exception 1 '(2 3)))
 (error-exception 1 (list 2 3))
 > (with-exception-catcher .show (& (error "foo" "bar" 2)))
 (error-exception "foo" (list "bar" 2)))


(def error?
     "Interface predicate for both Gambit's built-in errors and for
`error-interface?` (dot-oo based interface)."
     (either error-interface?
	     gambit-error#exception?))


(TEST
 > (def e (with-exception-catcher
	   identity (& (open-input-file "oqwuiavuosviue"))))
 > (error? e)
 #t
 > (error-interface? e)
 #f
 > (gambit-error#exception? e)
 #t
 > (no-such-file-or-directory-exception? e)
 #t)



;; let cj-typed know about us
(set! cj-typed-1:error? error-interface?) ;; XX or error?
(set! cj-typed-1:.string .string)




;; Gambit's own (non-user) exceptions, move to separate library?

(define. (datum-parsing-exception.show e)
  ;; XX evil?: should fail if we're not called via try-show, but if we
  ;; use .show, then try-show shows the top-most datum unpacked. TODO.
  `(datum-parsing-exception ,@(map try-show (cdr (##vector->list e)))))

