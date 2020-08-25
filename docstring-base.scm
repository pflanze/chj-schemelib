;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
         (simple-match-1 assert*))

(export (macro docstring-from)
        docstring)


"Base library for docstrings. For parsing / `docstring` function see
`docstring.scm`"


(include "cj-standarddeclares.scm")


(define-macro* (docstring-from fnname)
  "Add a marker to let docstring retrieval tools get docstring from
fnname"
  ;; (at runtime, presumably)
  (assert* symbol? fnname
           (lambda (fnname)
             ;; Hmm, FUTURE: allow fnname to be a local variable, so
             ;; that runtime references to other runtime function
             ;; values which are not at the top level can be possible.
             `(begin docstring-from: ',fnname
                     ;; To make sure the function this is used in
                     ;; doesn't return the fnname symbol:
                     #!void))))


