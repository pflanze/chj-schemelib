;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (cj-functional-2 either)
         lazy
         (debuggable-promise possibly-use-debuggable-promise);; ever hack
         (oo-util-lazy iseq?)
         (scheme-meta homogenous-vector?)
         (cj-inline inline)
         (cj-struct-tag cj-struct#vector?)
         ;; test
         )

(export seq?)

"sequence interface definition. Note that unlike Clojure's seq which are called iseq? here, these include (number and other) vectors."


(include "cj-standarddeclares.scm")
(possibly-use-debuggable-promise)


;; performance business
(define seq#iseq? (inline iseq?))

(define seq#homogenous-vector?
  (let ()
    (include "scheme-meta-homogenous-vector--include.scm")
    homogenous-vector?))


(define seq? (either seq#iseq?
                     cj-struct#vector?
                     seq#homogenous-vector?))

