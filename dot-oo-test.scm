;;; Copyright 2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Additional tests for dot-oo (added before implementing new method
;; selection strategy)

;; Also benchmarks (also to verify the new implementation works
;; properly).

(require cj-env
	 test
	 dot-oo
	 ;; hmm but actually more, for "comfort", and, to load more
	 ;; code that uses dot-oo so that selected method calls are
	 ;; actually slow (XX but then should load more stuff
	 ;; *explicitely*):
	 easy
	 jclass)




(def (oo-bench n)
     (def v (vector n 20))
     (def l (list n 20))
     (time (repeat n (.ref (vector 10 20) 1)))
     (time (repeat n (.ref v 1)))
     (time (repeat n (.ref l 1)))
     )