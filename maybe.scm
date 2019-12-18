;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;; This file is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License (LGPL)
;;; as published by the Free Software Foundation, either version 2 of
;;; the License, or (at your option) any later version.


(require easy ;; dot-oo would be enough, tough
         (predicates-1 any?)
         monad/syntax
         (cj-typed is-monad-name!)
         test)

(export maybe?
        (methods maybe.>>= maybe.return)
        (macro  maybe.>>))


;; tell cj-typed that the maybe type constructor is a monad
(is-monad-name! 'maybe)


(def-inline (maybe->>= a r)
  (and a
       (r a)))

(defmacro (maybe->> a b)
  `(and ,a ,b))

(def-inline (maybe-return v)
  v)


;; (def maybe? any?) ;; oh my

;; with monad/generic -- no, neither is it going to be type safe
;; enough, nor would we get the required lazyness in >>

(def maybe.>>= (maybe->>=-lambda))


(TEST
 > (maybe.>>= 2 inc*)
 3
 > (maybe.>>= #f inc*)
 #f)


(TEST
 > (def actions '())
 > (def (t msg val)
        (push! actions msg)
        val)
 > (in-monad maybe (mdo (t 'a 2)
                        (t 'b 3)
                        (t 'c 4)))
 4
 > actions
 (c b a)
 > (in-monad maybe (mdo (t 'd 2)
                        (t 'e #f)
                        (t 'f 4)))
 #f
 > actions
 (e d c b a)
 > (in-monad maybe (mlet (x (t 'g 2)) x))
 2
 > (in-monad maybe (mlet ((x (t 'h #f))
                          (y (t 'i 3)))
                         x))
 #f
 > actions
 (h g e d c b a)

 > (expansion mdo-in maybe a b c)
 (and a b c))


