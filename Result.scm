;;; Copyright 2016-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Unlike Maybe, this includes a value in the failure case instead of
;; nothing.

;; Compare to Rust: https://doc.rust-lang.org/std/result/
;; or Haskell: http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Either.html

;; Following Rust terminology here.

(require easy
         jclass
         (dot-oo void/1)
         test)

(export (jclass Result
                (jclass Ok)
                (jclass Error))
        (macros if-Ok
                Result:and
                Result:or)
        Result-of
        Ok-of
        Error-of)

(jclass Result

        (def-method- (if-Ok v yes no)
          (cond ((Ok? v)
                 (yes (Ok.value v)))
                ((Error? v)
                 (no (Error.value v)))
                (else
                 (error "not a Result:" v))))

        (jclass (Ok value))
            
        ;; call .value .reason instead?:
        (jclass (Error value)))

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
      (source-error stx "need at least one clause")
      (if (one-item? clauses)
          (first clauses)
          (let-pair ((a r) (reverse clauses))
                    (fold (lambda (clause next)
                            (with-gensym
                             V
                             `(let ((,V ,clause))
                                (if (Ok? ,V)
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
 #((Ok) 2))

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

