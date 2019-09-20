;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy)

(export number-vector?
        iseq-of-number?)

"Like predicates.scm but for math"


(include "../cj-standarddeclares.scm")


(def number-vector?
     (either homogenous-vector?
             (vector-of number?)))

(def iseq-of-number?
     (iseq-of number?))

