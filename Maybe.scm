;;; Copyright 2016-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Also see Result.scm

(require easy
         (if-let if-let*-expand
                 if-let-expand) ;; incl. monad-ops
         monad/generic
         (cj-typed is-monad-name!)
         test
         monad/syntax)

(export (class Maybe ;; yes a class, not an interface
               (class Nothing)
               (class Just))
        Maybe:Just?
        Maybe:Nothing?
        Maybe
	(macro Maybe:if) (macro if-Just)
        (macro Maybe:cond)
        (macro Maybe:if-let*)
        (macro Maybe:if-let)
        Maybe-or ;; 2-ary
        Maybe:or ;; n-ary
        ;; Maybe-and -- use Maybe->>, ok?

        ;; monad ops (XX make an exporter for those! 'implements')
        (methods Maybe.>>= Maybe.>> Maybe.return Maybe.unwrap)
        (inline Maybe->>=) (macro Maybe->>) Maybe-return Maybe-unwrap
        cat-Maybes)



(defclass Maybe

  (defclass ((Nothing _Nothing))

    (defmethod (maybe-value s)
      #f)

    (defmethod (show s)
      `(Nothing)))

  (defclass (Just value)
    (defmethod maybe-value Just.value))

  ;; name if-present instead?
  (defmethod (if-Just v then els)
    (if (Just? v)
        (then (@Just.value v))
        (els)))

  (defmethod (monad-ops _)
    Maybe:monad-ops))


;; Variants of Just? and Nothing? that throw for non-Maybe values:

(def (Maybe:Just? v)
     (cond ((Just? v) #t)
           ((Nothing? v) #f)
           (else (error "not a Maybe:" v))))

(def (Maybe:Nothing? v)
     (cond ((Nothing? v) #t)
           ((Just? v) #f)
           (else (error "not a Maybe:" v))))


;; optimization:
(def _Nothing_ (_Nothing))
(def (Nothing)
     _Nothing_)

(TEST
 > (eq? (Nothing) (Nothing))
 #t
 > (map (lambda (v)
          (map (C _ v) (list Maybe? Nothing? Just?
                             (lambda (v)
                               (with-exception-catcher
                                error-exception-message
                                (& (.if-Just v
                                             identity
                                             (& 'n))))))))
        (list #f
              (values)
              (Nothing)
              (Just 1)
              (Just #f)
              (Just (Nothing))
              (Just (Just 13))))
 ((#f #f #f "no method found for generic .if-Just for value:")
  (#f #f #f "no method found for generic .if-Just for value:")
  (#t #t #f n)
  (#t #f #t 1)
  (#t #f #t #f)
  (#t #f #t #((Nothing)))
  (#t #f #t #((Just) 13)))
 > (Just.value (.value (Just (Just 13))))
 13)


(TEST
 > (%try-error (.if-Just 'foo (lambda_ 1) (lambda_ 2)))
 #(error "no method found for generic .if-Just for value:" foo))


(def (Maybe:error v)
     (error "not a Maybe:" v))

;; XX rename to if-Just (for consistency with Result.scm)? (or if-present ?)
(defmacro (Maybe:if t
                    then
                    #!optional
                    else)
  `(let ((it-Maybe ,t))
     (cond ((Just? it-Maybe)
            (let ((it (@Just.value it-Maybe)))
              ,then))
           ((Nothing? it-Maybe)
            ,(or else `(void)))
           (else
            (Maybe:error it-Maybe)))))

(TEST
 > (Maybe:if (Just 1) it 'no)
 1
 > (Maybe:if (Nothing) it 'no)
 no
 > (%try-error (Maybe:if 'foo 1 2))
 #(error "not a Maybe:" foo))



;; once again (where did I have something like this?):
;; This never returns `(begin), which might be what you want ('usually
;; always'?)
(both-times
 (def (rest->begin rest)
      (trif-one rest
                identity
                (C cons `begin _)
                (& `(void)))))

(TEST
 > (rest->begin '())
 (void)
 > (rest->begin '(a))
 a
 > (rest->begin '(a b))
 (begin a b))


;; Stupid, only allowing one test, why. Should extend, similar to
;; Maybe:or, ah wait, Result:or. Anyway, similar to cond.
(defmacro (Maybe:cond t+then #!optional else)
  (let ((else* (if else
                   (mcase else
                          (`(else . `rest)
                           (rest->begin rest))
                          (`(`t . `rest)
                           (assert* true? t)
                           (rest->begin rest)))
                   `(void))))
    (mcase t+then
           (`(`t => `then)
            (with-gensym V
                         `(let ((,V ,t))
                            (cond ((Just? ,V)
                                   (,then (@Just.value ,V)))
                                  ((Nothing? ,V)
                                   ,else*)
                                  (else
                                   (Maybe:error ,V))))))
           (`(`t . `rest)
            ;; actually introduces |it| like, well, Maybe:if
            `(Maybe:if ,t
                       ,(rest->begin rest)
                       ,else*)))))

(TEST
 > (Maybe:cond ((Nothing) => 'no))
 #!void
 > (Maybe:cond ((Nothing) => 'no) (else 'fail))
 fail
 > (Maybe:cond ((Just 2) it) (else 'fail))
 2
 > (Maybe:cond ((Just 3) => identity) (else 'fail))
 3
 )

(TEST
 > (def (psqrt x)
        (if (positive? x)
            (Just (sqrt x))
            (Nothing)))
 > (def (f x)
        (Maybe:if (psqrt x)
                  (inc it)
                  'n))
 > (def (f* x)
        (Maybe:if (psqrt x)
                  (inc it)))
 > (def counter 0)
 > (def (g x)
        (Maybe:cond ((psqrt x) => inc)
                    (else (inc! counter)
                          'n)))
 > (def (g* x)
        (Maybe:cond ((psqrt x) => inc)))
 > (map (lambda (x)
          (list (f x)
                (g x)
                (f* x)
                (g* x)))
        (list 4 9 -4))
 ((3 3 3 3)
  (4 4 4 4)
  (n n #!void #!void))
 > counter
 1
 > (%try-error (Maybe:cond ((sqrt 4) => inc)))
 #(error "not a Maybe:" 2))


(def (Maybe pred)
     (lambda (v)
       (or (Nothing? v)
           (and (Just? v)
                (pred (@Just.value v))))))

(TEST
 > (def Maybe-integer? (Maybe integer?))
 > (map Maybe-integer? (list (Nothing) 10 (Just 10)))
 (#t #f #t))


(defmacro (Maybe:if-let* assignments yes #!optional no)
  ;; return (Nothing) in "void" case? Doesn't make sense over #f. Just void?
  (if-let*-expand `Maybe:cond assignments yes (or no `(void))))

(defmacro (Maybe:if-let assignments yes #!optional no)
  (if-let-expand `Maybe:cond assignments yes (or no `(void))))

;; same as Maybe:if-let
;; (defmacro (if-Just assignments yes #!optional no)
;;   (if-let-expand `Maybe:cond assignments yes (or no `(void))))

(defmacro (if-Just t then else)
  "If the value returned by `t` is a `Just`, its contents is bound to
`it` and `then` is evaluated, otherwise it is verified to be a
`Nothing` and `else` is evaluated (otherwise an exception is thrown)."
  (with-gensym
   V
   `(let ((,V ,t))
      (if (Maybe:Just? ,V)
          (let ((it (@Just.value ,V))) ,then)
          ,else))))



(TEST
 > (%try (Maybe:if-let ((a 2)) 3))
 (exception text: "not a Maybe: 2\n")
 > (%try (Maybe:if-let ((a #f)) 3 4))
 (exception text: "not a Maybe: #f\n")
 > (Maybe:if-let ((a (Just 2))) a)
 2
 > (Maybe:if-let ((a (Just 2))) a 4)
 2
 > (Maybe:if-let ((a (Nothing))) a 4)
 4
 > (Maybe:if-let ((a (Nothing))) a)
 #!void
 > (Maybe:if-let ((a (Just 2))
                  (b (Just 3)))
                 (list a b)
                 4)
 (2 3)
 > (Maybe:if-let ((a (Just 2))
                  (b (Nothing)))
                 (list a b)
                 5)
 5
 > (Maybe:if-let ((a (Nothing))
                  (b (Just 3)))
                 (list a b)
                 5)
 5
 > (%try (Maybe:if-let ((z8wm5y6dp9 (Just 1))
                        (b z8wm5y6dp9))
                       (list a b)
                       5))
 (exception text: "Unbound variable: z8wm5y6dp9\n")
 > (%try (Maybe:if-let* ((z8wm5y6dp9 (Just 1))
                         (b z8wm5y6dp9))
                        (list a b)
                        5))
 (exception text: "not a Maybe: 1\n")
 >  (Maybe:if-let* ((z8wm5y6dp9 (Just 1))
                    (b (Just (inc z8wm5y6dp9))))
                   (list z8wm5y6dp9 b)
                   5)
 (1 2)
 )


(defmacro (Maybe-or a b)
  (with-gensym
   V
   `(let ((,V ,a))
      (if (Maybe:Just? ,V)
          ,V
          ,b))))

(defmacro (Maybe:or . exprs)
  ;; RA is more efficient than LA
  `(RA Maybe-or ,@exprs))


;; === Maybe monad =======================================================

;; tell cj-typed that our type constructor is a monad
(is-monad-name! 'Maybe)

(def-inline (Maybe->>= a f)
  (Maybe:if-let ((v a))
                (f v)
                _Nothing_))

(def. Maybe.>>= (Maybe->>=-lambda))


(defmacro (Maybe->> a b)
  `(if (Maybe:Just? ,a)
       ,b
       _Nothing_))

(def. (Maybe.>> a b)
  ;; XX might still yet move to make b lazy automatically
  (error "can't do this with eager b"))


(def Maybe-return Just)
(def. Maybe.return Just)


(defclass (Maybe-nothing-exception))

(def. (Maybe.unwrap r)
  (if-Just r
           it
           (raise (Maybe-nothing-exception))))

(def Maybe-unwrap Maybe.unwrap)


(def Maybe:monad-ops
     (monad-ops Maybe.>>
                Maybe.>>=
                Maybe-return
                Maybe.unwrap))



;; Adapted tests from maybe.scm:


(TEST
 > (Maybe.>>= (Just 2) inc*)
 3
 ;; ^ not type correct, though--XX catch it?
 > (.show (Maybe.>>= (Just 2) (comp Maybe.return inc*)))
 (Just 3)
 > (.show (Maybe.>>= (Nothing) inc*))
 (Nothing))



(TEST
 > (def actions '())
 > (def (t msg val)
        (push! actions msg)
        val)
 > (.show (in-monad Maybe (mdo (t 'a (Just 2))
                               (t 'b (return 3))
                               (t 'c (return 4)))))
 (Just 4)
 > actions
 (c b a)
 > (.show (in-monad Maybe (mdo (t 'd (return 2))
                               (t 'e (Nothing))
                               (t 'f (return 4)))))
 (Nothing)
 > actions
 (e d c b a)
 > (in-monad Maybe (mlet (x (t 'g (Just 2))) x))
 2
 > (.show (in-monad Maybe (mlet ((x (t 'h (Nothing)))
                                 (y (t 'i (return 3))))
                                x)))
 (Nothing)
 > actions
 (h g e d c b a)

 > (expansion mdo-in Maybe a b c)
 (if (Maybe:Just? a)
     (if (Maybe:Just? b)
         c
         _Nothing_)
     _Nothing_))


(TEST
 > (.show (in-monad Maybe (=<< (comp return inc) (Just 123))))
 (Just 124))

(TEST
 > (.unwrap (Just 'hi))
 hi
 > (%try (.unwrap (Nothing)))
 (exception text: "This object was raised: [(Maybe-nothing-exception)]\n")
 > (in-monad Maybe
             (unwrap (Just 'hi)))
 hi
 > (%try (in-monad Maybe
                   (unwrap (Nothing))))
 (exception text: "This object was raised: [(Maybe-nothing-exception)]\n"))


;; Generic monads

;;(TEST )



;; catMaybes :: [Maybe a] -> [a]

;; def (cat-Maybes [(ilist-of Maybe?) l]) -> ilist?
;; Can't use ilist type, dependency cycle.

(def (cat-Maybes l)
     (if-let-pair ((a l*) l)
                  (if-Just a
                           (cons it (cat-Maybes l*))
                           (cat-Maybes l*))
                  (-> null? l)))

(TEST
 > (cat-Maybes (list (Just 1) (Just 3) (Nothing)))
 (1 3)
 > (%try (cat-Maybes (improper-list (Just 1) (Just 3) (Nothing))))
 (exception text: "value fails to meet predicate: (null? '[(Nothing)])\n"))


