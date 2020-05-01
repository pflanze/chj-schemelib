;;; Copyright 2014-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require oo-vector-lib
         test)

(include "cj-standarddeclares.scm")

(def-oo-vector-lib-for vector)

(TEST
 > (define v (vector (sqrt -2) 39 488))
 > (.ref-inexact v 0)
 0.+1.4142135623730951i
 > (.ref-inexact-real v 1)
 39.
 > (%try (.ref-real v 0))
 (exception text: "value fails to meet predicate: (real? +1.4142135623730951i)\n")
 > (%try (.ref-inexact-real v 0))
 (exception text: "value fails to meet predicate: (real? +1.4142135623730951i)\n")
 > (.list/tail '[a b c] '(1 2 3))
 (a b c 1 2 3))

