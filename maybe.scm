;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;; This file is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License (LGPL)
;;; as published by the Free Software Foundation, either version 2 of
;;; the License, or (at your option) any later version.


(require easy ;; dot-oo would be enough, tough
         (predicates-1 any?)
         test)

(export maybe?
        (method maybe.>>=))


(def maybe? any?) ;; oh my

(def. (maybe.>>= a r)
  (and a
       (r a)))

;; >> requires a thunk [or promise]. Don't define that just yet?
;; Well, go ahead:

(def. (maybe.>> a b)
  ;; or should it force a and leave b alone?
  (and a
       (force b)))


(TEST
 > (.>>= 2 inc*)
 3
 > (.>>= #f inc*)
 #f)

