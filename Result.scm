;;; Copyright 2016-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Unlike Maybe, this includes a value in the failure case instead of
;; nothing.

;; Compare to Rust: https://doc.rust-lang.org/std/result/
;; or Haskell: http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Either.html

;; Following Rust terminology here.

(require easy-2
         (dot-oo void/1)
         (oo-util-lazy ilist? iseq-of)
         monad/generic
         test
         monad/syntax)

(export (class Result
                (class Ok)
                (class Error))
        (macros if-Ok
                Result:and
                Result:or
                Result:try)
        Results?
        Result-of
        Ok-of
        Error-of
        Result:Ok?
        Result:Error?
        (method Results.Result)
        Error-source
        Error-source-unfinished-after
        ;; monad ops (XX make an exporter for those! 'implements')
        (methods Result.>>= Result.>> Result.return Result.unwrap)
        (inline Result->>=) (macro Result->>) Result-return Result-unwrap)

(include "cj-standarddeclares.scm")


(defclass Result

  (defmethod- (if-Ok v yes no)
    (cond ((Ok? v)
           (yes (Ok.value v)))
          ((Error? v)
           (no (Error.value v)))
          (else
           (error "not a Result:" v))))

  (defmethod (monad-ops _)
    Result:monad-ops)
       

  (defclass (Ok value))
            
  ;; call .value .reason instead?:
  (defclass (Error value)))


;; Variants of predicates that throw for non-Result values:

(def (Result:Ok? v)
     (cond ((Ok? v) #t)
           ((Error? v) #f)
           (else (error "not a Result:" v))))

(def (Result:Error? v)
     (cond ((Error? v) #t)
           ((Ok? v) #f)
           (else (error "not a Result:" v))))


(TEST
 > (map (lambda (v)
          (map (C _ v) (list Result? Ok? Error?
                             (lambda (v)
                               (if (Result? v)
                                   (.value v)
                                   'n)))))
        (list #f
              (values)
              (Ok 21)
              (Error 1)
              (Error #f)
              (Error (Ok 'hihi))
              (Error (Error 13))))
 ((#f #f #f n)
  (#f #f #f n)
  (#t #t #f 21)
  (#t #f #t 1)
  (#t #f #t #f)
  (#t #f #t #((Ok) hihi))
  (#t #f #t #((Error) 13)))
 > (Error.value (.value (Error (Error 13))))
 13)



(TEST
 > (.if-Ok (Ok '()) (lambda_ 'ok) (lambda_ 'fail))
 ok
 > (.if-Ok (Ok '()) identity (lambda_ 'fail))
 ()
 > (.if-Ok (Error 'foo) (lambda_ 'ok) (lambda_ 'fail))
 fail
 > (.if-Ok (Error 'foo) (lambda_ 'ok) identity)
 foo
 ;; > (.if-Ok (Error 'foo) (lambda_ 'ok))
 ;; #!void
 ;; no, don't allow this, OK?
 )


(defmacro (if-Ok t
                 then
                 #!optional
                 else)
  `(let ((it-Result ,t))
     ;; use |scheme:if| since |if| is overridden to not allow not
     ;; having an else branch, and |when| is impractical here.
     (scheme:if (Ok? it-Result)
                (let ((it (@Ok.value it-Result)))
                  ,then)
                ,@(if else
                      ;; Note: do *not* use @Error.value here, Ok?
                      ;; check above didn't check for that yet! (And
                      ;; this is the slow path, so, calling another
                      ;; remote function is fine.)
                      (list `(let ((it (Error.value it-Result)))
                               ,else))
                      '()))))

(TEST
 > (if-Ok (Ok '()) 'ok 'fail)
 ok
 > (if-Ok (Ok '()) it 'fail)
 ()
 > (if-Ok (Error 'foo) 'ok 'fail)
 fail
 > (if-Ok (Error 'foo) 'ok it)
 foo
 > (if-Ok (Error 'foo) 'ok)
 #!void)


(defmacro (Result:and . clauses)
  (if (null? clauses)
      (raise-source-error stx "need at least one clause")
      (if (one-item? clauses)
          (first clauses)
          (let-pair ((a r) (reverse clauses))
                    (fold (lambda (clause next)
                            (with-gensym
                             V
                             `(let ((,V ,clause))
                                (if (Result:Ok? ,V)
                                    ,next
                                    ,V))))
                          a
                          r)))))

(TEST
 ;; > (Result:and)
 ;; (Error "empty Result:and statement")
 ;; or better simply don't allow syntactically?
 > (Result:and (Error "foo"))
 #((Error) "foo")
 > (Result:and (Error "foo") (Error "bar"))
 #((Error) "foo")
 > (Result:and (Ok 1) (Error "bar"))
 #((Error) "bar")
 > (Result:and (Ok 1) (Ok 2) (Ok "bar"))
 #((Ok) "bar")
 > (Result:and (Ok "foo") (Ok 2))
 #((Ok) 2)
 > (%try (Result:and (void) (Ok 2)))
 (exception text: "not a Result: #!void\n"))


(defmacro (Result:or . clauses)
  (if (one-item? clauses)
      (first clauses)
      (let-pair ((a r) (reverse clauses))
                (fold (lambda (clause next)
                        (with-gensym
                         V
                         `(let ((,V ,clause))
                            (if (Ok? ,V)
                                ,V
                                ,next))))
                      a
                      r))))

(TEST
 ;; > (Result:or)
 ;; (Error "empty Result:or statement") or don't allow
 > (Result:or (Error "foo"))
 #((Error) "foo")
 > (Result:or (Error "foo") (Error "bar"))
 #((Error) "bar")
 > (Result:or (Ok 1) (Error "bar"))
 #((Ok) 1)
 > (Result:or (Error "bar") (Ok 1))
 #((Ok) 1)
 > (Result:or (Ok 1) (Ok 2) (Error "bar"))
 #((Ok) 1)
 > (Result:or (Error "foo") (Error "bar") (Ok 1) (Ok 2))
 #((Ok) 1))


;; Predicates

;; XX just call |Result|, for consistency with |Maybe|? Difficult
;; times. (Have type constructors ucfirst, value constructors
;; lcfirst?)
(def (Result-of pred-result pred-failure)
     (lambda (v)
       (or (and (Ok? v)
                (pred-result (Ok.value v)))
           (and (Error? v)
                (pred-failure (Error.value v))))))

(def (Ok-of pred)
     (lambda (v)
       (and (Ok? v)
            (pred (Ok.value v)))))

(def (Error-of pred)
     (lambda (v)
       (and (Error? v)
            (pred (Error.value v)))))


(TEST
 > (def l
        (list (Ok 5)
              (Ok 'x)
              10
              (Error 10)
              (Error 'y)))
 > (map (Result-of symbol? integer?) l)
 (#f #t  #f #t #f)
 > (map (Ok-of integer?) l)
 (#t #f #f #f #f)
 > (map (Error-of symbol?) l)
 (#f #f #f #f #t))


(defmacro (Result:try . body)
  `(with-exception-catcher Error
                           (lambda () (Ok (begin ,@body)))))


(def Results? (iseq-of Result?))


;; (Somewhat (not really) related to |cat-Maybes|.)

;; Monad m => [m a b] -> m [a] [b]

;; This is not the same as Haskell's `sequence`, which only returns
;; the first failure: (Traversable t, Monad m) => t (m a) -> m (t a)

;; Prelude> sequence [Just 1, Just 2, Just 3]
;; Just [1,2,3]
;; Prelude> sequence [Just 1, Nothing, Just 3]
;; Nothing

;; Prelude> sequence [Right 1, Right 2, Right 3]
;; Right [1,2,3]
;; Prelude> sequence [Right 1, Left 2, Right 3]
;; Left 2
;; Prelude> sequence [Right 1, Left 2, Left 3]
;; Left 2

(def. (Results.Result l) -> (Result-of ilist? ilist?)
  "Return an Error of the Error values in l if any (dropping all Ok
values); otherwise return an Ok of the Ok values in l."
  (let lp ((l l)
           (ok '())
           (err '()))
    (if-let-pair ((a l*) (force l))
                 (if-Ok a
                        (lp l* (cons it ok) err)
                        (lp l* ok (cons it err)))
                 (if (null? err)
                     (Ok (reverse ok))
                     (Error (reverse err))))))

(TEST
 > (show (.Result (list (Ok 1) (Ok 2) (Error 30) (Ok 3) (Error 40) (Ok 5))))
 (Error (list 30 40))
 > (show (.Result (list (Ok 1) (Ok 2))))
 (Ok (list 1 2)))


(def (Error-source val msg . args)
     "Same as |raise-source-error| but in the Result monad instead of
the builtin IO/error one."
     (Error (make-source-error val msg args)))

(def (Error-source-unfinished-after val)
     "Unexpected end of input"
     (Error-source val "unfinished after"))



;; === Result (Either) monad ===========================================

;; Adapted from Maybe.scm

;; tell cj-typed that our type constructor is a monad
(def-monad/constructors Result Result-of)

(def (Result:->Result v)
     (-> Result? v))

(def-inline (Result->>= a f)
  (if (Ok? a)
      (f (@Ok.value a))
      (Result:->Result a)))

(def. Result.>>= (Result->>=-lambda))


(defmacro (Result->> a b)
  `(Result:and ,a ,b))

(def. (Result.>> a b)
  ;; XX might still yet move to make b lazy automatically
  (error "can't do this with eager b"))


(def Result-return Ok)
(def. Result.return Ok)


(def. (Result.unwrap r)
  (if-Ok r
         it
         (raise it)))

(def Result-unwrap Result.unwrap)


(def Result:monad-ops
     (monad-ops Result.>>
                Result.>>=
                Result-return
                Result.unwrap))



(TEST
 > (Result.>>= (Ok 2) inc*)
 3
 ;; ^ not type correct, though--XX catch it?
 > (show (Result.>>= (Ok 2) (comp Result.return inc*)))
 (Ok 3)
 > (show (Result.>>= (Error 2) inc*))
 (Error 2))



(TEST
 > (def actions '())
 > (def (t msg val)
        (push! actions msg)
        val)
 > (show (in-monad Result (mdo (t 'a (Ok 2))
                                (t 'b (return 3))
                                (t 'c (return 4)))))
 (Ok 4)
 > actions
 (c b a)
 > (show (in-monad Result (mdo (t 'd (return 2))
                                (t 'e (Error "foo"))
                                (t 'f (return 4)))))
 (Error "foo")
 > (%try (in-monad Result (mdo (void)
                               (t 'f (return 4)))))
 (exception text: "not a Result: #!void\n")
 > actions
 (e d c b a)
 > (in-monad Result (mlet (x (t 'g (Ok 2))) x))
 2
 > (show (in-monad Result (mlet ((x (t 'h (Error "foo")))
                                  (y (t 'i (return 3))))
                                 x)))
 (Error "foo")
 > actions
 (h g e d c b a)

 > (show (-> (Result-of integer? symbol?) (mdo (Ok 2) (Ok 3))))
 (Ok 3)
 > (show (-> (Result-of integer? symbol?) (mdo (Error 'foo) (Ok 3))))
 (Error 'foo)
 
 > (define TEST:equal? syntax-equal?)
 > (expansion mdo-in Result a b c)
 (let ((GEN:V-4099 a))
   (if (Result:Ok? GEN:V-4099)
       (let ((GEN:V-4100 b))
         (if (Result:Ok? GEN:V-4100)
             c
             GEN:V-4100))
       GEN:V-4099)))

;; ==>

(TEST
 > (in-monad Result (==> (Ok 12) Ok Error Ok))
 [(Error) 12]
 > (in-monad Result (==> (Ok 12) list))
 (12)
 > (in-monad Result (==> (Error 12) list))
 [(Error) 12])

;; ==>*

(TEST
 > ((in-monad Result (==>*-nary Error list)) (Ok 10))
 [(Error) 10]
 ;; > ((in-monad Result (==>*-nary (Error) (list))) (Ok 10))
 ;; ERROR IN syntax, .. -- need a symbol here to support n-arity
 > ((in-monad Result (==>*-nary Ok list)) (Ok 10))
 (10)
 > ((in-monad Result (==>*-nary Ok list)) (Error 10))
 [(Error) 10]
 ;; > ((in-monad Result (==>*-nary (Ok) list)) (Ok 10))
 ;; ERROR IN syntax, .. -- need a symbol here to support n-arity
 > ((in-monad Result (==>*-nary Error list)) (Error 10))
 [(Error) 10])

(TEST
 > ((in-monad Result (==>* Error list)) (Ok 10))
 [(Error) 10]
 > ((in-monad Result (==>* (Error) (list))) (Ok 10))
 [(Error) 10]
 > ((in-monad Result (==>* Ok list)) (Ok 10))
 (10)
 > ((in-monad Result (==>* Ok list)) (Error 10))
 [(Error) 10]
 > ((in-monad Result (==>* (Ok) list)) (Ok 10))
 (10)
 > ((in-monad Result (==>* Error list)) (Error 10))
 [(Error) 10])

(TEST
 > (.unwrap (Ok 'hi))
 hi
 > (%try (.unwrap (Error 'hi)))
 (exception text: "This object was raised: hi\n")
 > (in-monad Result
             (unwrap (Ok 'hi)))
 hi
 > (%try (in-monad Result
                   (unwrap (Error 'ha))))
 (exception text: "This object was raised: ha\n"))



;; Generic monads

;;(TEST )
