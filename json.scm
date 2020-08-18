;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         easy-table
         template
         test
         local-test)

(export json-expr?
        ijson-expr?)

"JSON parser (and printer?)"


(include "cj-standarddeclares.scm")


;; Some inspirations from:

;; https://docs.racket-lang.org/json/index.html


(def (json-expr?/list-of list-of)
  (named json-expr?
         (either boolean?
                 string?
                 exact-integer?
                 ;; ^ isn't range limited in json? Allow it anyway
                 ;; is the name of the game I guess?
                 (both inexact-real? rational?)
                 (list-of json-expr?)
                 (TABLE-of json-expr?))))


;; XX finish seq-of rework then get rid of these
(def *iseq-of (lambda (T?)
                (either (vector-of T?) (iseq-of T?))))
(def *seq-of *iseq-of)

(def json-expr?
     "exhaustive check"
     (json-expr?/list-of *seq-of))

(def ijson-expr?
     "fast check only"
     (json-expr?/list-of *iseq-of))

(TEST
 > (def (t jsexpr?)
        (local-TEST*
         ;; > (jsexpr? 'null)
         ;; #t
         ;; ^ XX what for?
         > (jsexpr? #t)
         #t
         > (jsexpr? "cheesecake")
         #t
         > (jsexpr? 3.5)
         #t
         > (jsexpr? (list 18 "" #f))
         #t
         ;; > (jsexpr? (list 18 'null #f))
         ;; #t
         > (jsexpr? (TABLE turnip: 82))
         #t
         > (jsexpr? (vector 1 2 3 4))
         #t ;; accepting all `seq?`
         > (jsexpr? (=> (TABLE turnip: 82) (table-set! 'hey 83)))
         #f
         > (jsexpr? +inf.0)
         #f))
 > (%test (t json-expr?))
 > (%test (t ijson-expr?)))



