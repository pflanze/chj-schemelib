;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; More tests on the Clojure infrastructure modules (esp. generative
;; ones)

(require easy
         clojure
         test
         test-random)


* ;: (comp first reverse) equals last


