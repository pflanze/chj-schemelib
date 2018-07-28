;;; Copyright 2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.

(require (cj-gambit-sys-0 @vector-ref))


(define (@promise-evaluated? v)
  (##not (##eq? (@vector-ref v 1) v)))

(define (@promise-value v)
  (@vector-ref v 1))
;; > (define v (delay 1))
;; > (@vector-ref v 1)  
;; #<promise #3>
;; ahaaa, that's why he does this circular ref thing??

