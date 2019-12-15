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


(def maybe? any?) ;; oh my

(def. (maybe.>>= a r)
  (and a
       (r a)))

;; >> requires a thunk [or promise]. Don't define that just yet?
;; Well, go ahead:

;; (def. (maybe.>> a b)
;;   ;; or should it force a and leave b alone?
;;   (and a
;;        (force b)))

;; But the one actually chosen by the macrology in monad/syntax:

(defmacro (maybe.>> a b)
  `(and ,a ,b))

;; This will mean that higher-order(?, generic) monadic functions like
;; mfor-each will not work for maybe! Any solution or just get on with
;; it? That's not a typing issue (even), but one of eagerness. Hmm,
;; for indirect monads it's not a problem so could make an indirect
;; one for Maybe or even maybe *?* (Or just accept that mdo is more
;; like a function, not really threading of execution model. It's
;; threading of data not order of evaluation. Hm?)



(def. (maybe.return v)
  v)


(TEST
 > (.>>= 2 inc*)
 3
 > (.>>= #f inc*)
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
 (letrec ((>>= maybe.>>=) (return maybe.return)) (and a b c)))


