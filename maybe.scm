;;; Copyright 2019-2020 by Christian Jaeger <ch@christianjaeger.ch>

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
        (inline just
                nothing)
        (macro if-just)
        (macro when-just)
        (methods maybe.>>= maybe.return)
        (macro maybe.>>) maybe.unwrap maybe-unwrap
        cat-maybes)

(include "cj-standarddeclares.scm")


;; tell cj-typed that the maybe type constructor is a monad
(def-monad/constructors maybe)


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


(defclass (maybe-nothing-exception))

;; Can't safely use def. currently
(def (maybe.unwrap r)
  (or r
      (raise (maybe-nothing-exception))))

(def maybe-unwrap maybe.unwrap)


(def-inline (just v) v) ;; == id
(def-inline (nothing) #f) ;; == false/0

(defmacro (if-just t then else)
  "If the value returned by `t` is not #f, it is bound to `it` and
`then` is evaluated, otherwise `else` is evaluated."
  (with-gensym
   V
   `(let ((,V ,t))
      (if ,V
          (let ((it ,V)) ,then)
          ,else))))

(defmacro (when-just t . body)
  "If the value returned by `t` is not #f, it is bound to `it` and the
body is evaluated, otherwise the void value is returned."
  (with-gensym
   V
   `(let ((,V ,t))
      (if ,V
          (let ((it ,V)) ,@body)
          (void)))))

(TEST
 > (when-just #f it)
 #!void
 > (when-just 10 it)
 10
 > (when-just 10 11 it)
 10)


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

(TEST
 > (maybe.unwrap (just 'hi))
 hi
 > (%try (maybe.unwrap (nothing)))
 (exception text: "This object was raised: [(maybe-nothing-exception)]\n")
 > (in-monad maybe
             (unwrap (just 'hi)))
 hi
 > (%try (in-monad maybe
                   (unwrap (nothing))))
 (exception text: "This object was raised: [(maybe-nothing-exception)]\n"))




;; catMaybes :: [Maybe a] -> [a]

;; def (cat-Maybes [ilist? l]) -> ilist?

(def (cat-maybes l)
     (if-let-pair ((a l*) l)
                  (if a
                      (cons a (cat-maybes l*))
                      (cat-maybes l*))
                  (-> null? l)))

(TEST
 > (cat-maybes (list 1 3 #f 4))
 (1 3 4)
 > (%try (cat-maybes (improper-list (Just 1) (Just 3) #f 4)))
 (exception text: "value fails to meet predicate: (null? 4)\n"))


