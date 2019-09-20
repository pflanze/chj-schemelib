;;; Copyright 2014-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require oo-vector-lib
         test)

(include "cj-standarddeclares.scm")

(def-oo-vector-lib-for u32vector)

(TEST
 > (define v (u32vector 0 39 488))
 > (.ref-real v 1)
 39
 > (.ref-inexact v 1)
 39.
 > (.ref-inexact-real v 2)
 488.)

