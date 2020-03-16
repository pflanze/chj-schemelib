;;; Copyright 2018-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require class
         (cj-typed-1 (mutable cj-typed-1:error?)
                     (mutable cj-typed-1:.string))
         (cj-source (type source-error))
         gambit-error
         (cj-port pretty-string)
         (string-util-2 string-possibly-strip-start
                        string-possibly-strip-end
                        chomp)
         (cj-functional-2 =>)
         show)

(export (interface error-interface)
        (methods error-exception.show
                 datum-parsing-exception.show)
        error?)



;; Base trait/interface for all exception/error values.

(definterface error-interface

  ;; human-readable string representation that explains what the error
  ;; is about in a nice way that's presentable to a user, OK?
  (method (string s) -> string?)

  ;; For web or Qt applications
  (method (sxml s) -> sxml?)
  
  ;; Also require .show ?

  (method (message s) -> string?)
  (method (parameters s) -> (ilist-of any?))
  ;; What about .procedure like Gambit's ones?

  ;; What about special location types, in the structured report
  ;; (instead of sxml, or, (css-class?-)enriched sxml?)? Or just one,
  ;; about the datum in question? Why does Gambit have .parameters but
  ;; not .value ? More general, ok, unlike just the cj-typed thing(?)
  ;; (e.g. IO routines depending on path *and* permission arguments).
  )

;; TODO: avoid calling .string on error objects then throwing that as
;; error-exception ones, like cj-typed currently does! (Only do the
;; .string at the UI boundary, i.e. repl, web, etc.)



(define (error-base-string s)
  ;; Outside error-base class so that it can be used by the
  ;; "patched-in" classes (Gambit's built-ins). Or should it simply be
  ;; `error.string`? Messy?

  ;; XX idea: make .message a formatting string?? That optionally
  ;; captures (some of) the parameters?
  (let ((msg (.message s))
        (parms (.parameters s)))
    ;; XX should have a way to position stuff for
    ;; pretty-string (via location (well, position)
    ;; info? (meaning, |source|))
    (if (null? parms)
        msg
        (=> parms
            try-show
            pretty-string
            (string-possibly-strip-start "(list")
            chomp
            (string-possibly-strip-end ")")
            ((lambda (str)
               (string-append msg ":" str)))))))


(defclass error-base
  implements: error-interface

  (defmethod string error-base-string)

  (defmethod (sxml s)
    (error "XX unfinished"))


  ;; message is required

  (defmethod (parameters s) '()))


;; Allow all of 'the other' (Gambit, modules?) error/exception types;
;; careful: they all need to implement the methods from the error
;; interface, too!

(define. (error-exception.show e)
  `(error-exception ,(.show (error-exception-message e))
                    ,(.show (error-exception-parameters e))))

(define. error-exception.message error-exception-message)
(define. error-exception.parameters error-exception-parameters)
(define. error-exception.string error-base-string)

(TEST
 > (.show (error-exception 1 '(2 3)))
 (error-exception 1 (list 2 3))
 > (with-exception-catcher .show (& (error "foo" "bar" 2)))
 (error-exception "foo" (list "bar" 2))
 > (.string (error-exception "Hi" '(2 "3")))
 "Hi: 2 \"3\""
 > (.string (error-exception "Hi" '()))
 "Hi")



(define. (source-error.show e)
  `(make-source-error ,(.show (source-error-source e))
                      ,(.show (source-error-message e))
                      ;; XX rename source-error-args to
                      ;; source-error-parameters or so
                      ,(.show (source-error-args e))))


(def error?
     "Interface predicate for both Gambit's built-in errors and for
`error-interface?` (dot-oo based interface)."
     (either error-interface?
             gambit-error#exception?
             source-error?))


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

