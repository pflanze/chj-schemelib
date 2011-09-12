;;; Copyright 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; a quasiquote that retains location information

(define-macro* (template:quasiquote t)
  (mcase
   t
   (`(unquote `x)
    'found-unquote)
   (else
    'nomatch)))

