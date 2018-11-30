;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy)

(include "cj-standarddeclares.scm")


(def (list-maybe-ref lis [natural0? n])
     (cond
      ((null? lis) #f)
      ((zero? n) (first lis))
      (else (list-maybe-ref (rest lis) (- n 1)))))


(def (maybe-fifth lis)
     (list-maybe-ref lis 4))


