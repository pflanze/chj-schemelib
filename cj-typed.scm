;;; Copyright 2010-2020 by Christian Jaeger, ch at christianjaeger ch

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;;;
;;;; simple explicit type checking
;;;

(require define-macro-star
         (scheme-meta perhaps-quote dsssl-meta-object?)
         (cj-functional-2 list-of)
         (simple-match-1 assert*)
         test
         values
         ;; cj-match  could, no need so far
         (cj-source-util-2 assert)
         (improper-list improper-length
                        improper-fold-right*
                        improper-fold-right)
         cj-typed-1)

(export (macro type-check)
        (macro source-type-check)
        perhaps-typed.var
        perhaps-typed.maybe-predicate
        typed?
        @typed.var
        typed.var
        typed.predicate
        args-detype
        args-vars
        (macro typed-lambda)
        (macro define-typed)
        (macro ->)
        (macro @->)
        ;; indirectly: ->-error
        (macros cj-typed-disable
                cj-typed-enable)
        is-monad-name!
        is-constructor-name-for-monad!
        (macro def-monad/constructors)
        maybe-monad-name
        
        #!optional
        predicate-symbol?
        typed-body-parse
        typed-lambda-expand
        define-typed-expand)


(both-times
 (define (type-check-expand predicate expr body use-source-error?
                            type-check-error)
   (let ((expr-str (let ((expr* (cj-desourcify expr)))
                     ;; avoid putting gensyms into exception messages,
                     ;; to make code using this testable.
                     (if (cj-gensym? expr*)
                         (cond ((cj-gensym-maybe-name expr*)
                                => (lambda (name)
                                     (string-append "gensym '"
                                                    (scm:object->string name))))
                               (else
                                #f))
                         (scm:object->string expr*))))
         (pred-str (scm:object->string (cj-desourcify predicate)))
         (V (gensym))
         (W (gensym)))
     
     `(##let* ((,V ,expr)
               (,W (,predicate ,V)))
              (##if (##or (##eq? ,W #t)
                          (cj-typed#type-check-warn
                           ,use-source-error?
                           ,expr-str
                           ,pred-str
                           ,W
                           ,V))
                    (##let () ,@body)
                    (,type-check-error
                     ,use-source-error?
                     ,expr-str
                     ,pred-str
                     ,W
                     ,V))))))

(define-macro* (type-check predicate expr . body)
  (type-check-expand predicate expr body #f
                     `cj-typed#type-check-error))

(define-macro* (type-check/type-check-error type-check-error
                                            predicate expr . body)
  (type-check-expand predicate expr body #f
                     type-check-error))

(define-macro* (source-type-check predicate expr . body)
  (type-check-expand predicate expr body #t
                     `cj-typed#type-check-error))

(TEST
 ;; test that there's no "Ill-placed 'define'" compile-time error
 > (let ((foo "foo"))
     (type-check string? foo
                 (##begin (define bar "bar") (string-append foo bar))))
 "foobar"
 > (let ((foo "foo"))
     (type-check string? foo
                 (define bar "bar")
                 (string-append foo bar)))
 "foobar")



(define (transform-arg arg args body type-check-error)
  ;; -> (values args* body*)
  (let ((arg* (source-code arg)))
    (define (err)
      (raise-source-error arg "expecting symbol or [predicate var]"))
    (cond ((symbol? arg*)
           (values (cons arg args)
                   body))
          ((vector? arg*)
           (if (= (vector-length arg*) 2)
               (let ((pred (vector-ref arg* 0))
                     (var (vector-ref arg* 1)))
                 (assert* symbol? var
                          (lambda (_)
                            (values (cons var args)
                                    `(type-check/type-check-error
                                      ,type-check-error
                                      ,pred ,var
                                      ,body)))))
               (err)))
          ((dsssl-meta-object? arg*)
           (values (cons arg* args)
                   body))
          ((pair? arg*)
           ;; should be after an #!optional; XX verify? or leave that
           ;; up to the next language layer?
           (if (= (improper-length arg*) 2)
               (let ((arg** (car arg*))
                     (default (cadr arg*)))
                 (letv ((subargs body*) (transform-arg arg** args body
                                                       type-check-error))
                       (let-pair ((subarg _) subargs)
                                 (values (cons (possibly-sourcify
                                                `(,subarg ,default)
                                                arg)
                                               args)
                                         body*))))
               ;; XX could give better error message, though
               (err)))
          (else
           (err)))))

(TEST
 > (define s1 '[[source1]
                ([[source1] pair? (console) 1048595]
                 [[source1] a (console) 1441811])
                (console)
                983059])
 > (values->vector (transform-arg s1 '() 'BODY 'TYPECHECKERROR1))
 [([[source2] ([[source1] pair? (console) 1048595]
               [[source1] a (console) 1441811])
    (console)
    983059])
  BODY])


;; for use by other code

;; Note: "var" (i.e. the result type) *should* be a symbol; we don't
;; check here, but it would be an error. (Default values would be
;; given outside, not here: `(#(number? y) 10) not `#(number? (y 10)))

(define (perhaps-typed.var x)
  (car (fst (transform-arg x '() '() 'TYPECHECKERROR2))))

(define (perhaps-typed.maybe-predicate x)
  ;; hacky, pick out of something like `(type-check foo? x ())
  (let ((r (snd (transform-arg x '() '() 'TYPECHECKERROR3))))
    (if (pair? r)
        (caddr r)
        #f)))

(TEST
 > (perhaps-typed.maybe-predicate (quote-source x))
 #f
 > (def s (perhaps-typed.maybe-predicate (quote-source [foo? x])))
 > (source-code s)
 foo?
 > (source? s)
 #t)


(define (typed? x)
  ;; stupid ~COPY
  (let ((x* (source-code x)))
    (and (vector? x*)
         (= (vector-length x*) 2)
         (symbol? (source-code (vector-ref x* 1))))))

(define (@typed.var x) ;; careful, unsafe!
  ;; again stupid ~COPY
  (vector-ref (source-code x) 1))

(define (typed.var expr)
  (source-type-check typed? expr
                     (@typed.var expr)))

(define (typed.predicate expr)
  (source-type-check typed? expr
                     (perhaps-typed.maybe-predicate expr)))


(TEST
 > (perhaps-typed.var '[foo? x])
 x
 > (perhaps-typed.var 'y)
 y
 > (perhaps-typed.maybe-predicate '[foo? x])
 foo?
 > (typed.predicate '[(maybe foo?) x])
 (maybe foo?)
 > (with-exception-catcher source-error-message (& (typed.predicate 'x)))
 "expr does not match typed?"
 > (typed? 'y)
 #f
 > (typed? '[foo? x])
 #t
 > (@typed.var '[foo? x])
 x)



(define (args-detype args)
  (improper-fold-right* (lambda (tail? arg args*)
                          (let ((a* (fst (transform-arg arg args* #f
                                                        'TYPECHECKERROR4))))
                            (if tail?
                                (car a*)
                                a*)))
                        '()
                        (source-code args)))

(TEST
 > (args-detype '(a b . c))
 (a b . c)
 > (args-detype '(a b #!optional c))
 (a b #!optional c)
 > (args-detype '([pair? a] b #!optional [number? c]))
 (a b #!optional c)
 > (args-detype '([pair? a] b #!optional (c 10)))
 (a b #!optional (c 10))
 > (args-detype '([pair? a] b #!optional ([number? c] 10)))
 (a b #!optional (c 10)))


(define (args-vars args)
  (improper-fold-right
   (lambda (arg vars)
     (if (dsssl-meta-object? (source-code arg))
         vars
         (cons (perhaps-typed.var arg) vars)))
   '()
   (source-code args)))

(TEST
 > (args-vars 'foo)
 (foo)
 > (args-vars '(foo))
 (foo)
 > (args-vars '(foo bar))
 (foo bar)
 > (args-vars '(foo bar . baz))
 (foo bar baz)
 > (args-vars '(foo bar . [foo? baz]))
 (foo bar baz)
 > (args-vars '(foo #!optional bar #!key boo . [foo? baz]))
 (foo bar boo baz))


(define (typed-body-parse maybe-stx body cont/maybe-pred+body)
  (assert (not (source? body)))
  (let ((body+ (if maybe-stx
                   (sourcify body maybe-stx)
                   body)))
    (if (pair? body)
        (let ((fst* (source-code (car body))))
          (if (eq? fst* '->)
              (if ((improper-list/length>= 3) body)
                  (cont/maybe-pred+body (cadr body) (cddr body))
                  (raise-source-error
                   body+
                   "a body starting with -> needs at least 2 more forms"))
              (cont/maybe-pred+body #f body)))
        (raise-source-error body+
                            "expecting body forms"))))

(TEST
 > (typed-body-parse #f '(a) vector)
 [#f (a)]
 > (typed-body-parse #f '(a b c) vector)
 [#f (a b c)]
 > (typed-body-parse #f '(-> b c) vector)
 [b (c)])

(define (typed-lambda-args-expand args body begin-form
                                  type-check-error)
  ;; -> (values-of (improper-list-of (possibly-source-of
  ;;                                  ;; not just symbol? but also #!rest etc.
  ;;                                  sexpr-object?))
  ;;               (possibly-source-of sexpr-object?))
  
  (let rem ((args args))
    (let ((args_ (source-code args)))
      (cond ((null? args_)
             (values '()
                     ;; can't just return body, as it's also used in
                     ;; transform-arg via the recursive call below:
                     `(,begin-form ,@body)))
            ((pair? args_)
             (let-pair ((arg args*) args_)
                       (letv (($1 $2) (rem args*))
                             (transform-arg arg $1 $2
                                            type-check-error))))
            (else
             ;; rest arg, artificially pick out the single var
             (letv ((vars body) (letv (($1 $2) (rem '()))
                                      (transform-arg args $1 $2
                                                     type-check-error)))
                   (assert (= (length vars) 1))
                   (values (car vars)
                           body)))))))

(TEST
 > (define s
     ;; (quote-source ((pair? a)))
     '[[source1]
       ([[source1]
         ([[source1] pair? (console) 1048595]
          [[source1] a (console) 1441811])
         (console)
         983059])
       (console)
       917523])
 > (values->vector (typed-lambda-args-expand s 'BODY '##begin 'TYPECHECKERROR5))
 [([[source2] ([[source1] pair? (console) 1048595]
               [[source1] a (console) 1441811])
    (console)
    983059])
  (##begin . BODY)]
 ;; ehr well this is the test in action:
 > (values->vector (typed-lambda-args-expand '([pair? x]) 'BODY '##begin
                                             'TYPECHECKERROR6))
 [(x) (type-check/type-check-error TYPECHECKERROR6 pair? x (##begin . BODY))])


(define (typed-lambda-expand stx args body begin-form
                             type-check-error)
  (typed-body-parse
   stx body
   (lambda (maybe-pred body)
     (let ((body (if maybe-pred
                     `((-> ,maybe-pred ,@body))
                     body)))
       (let ((args* (source-code args)))
         ;; Expand curried lambdas (nested variable lists), similar to
         ;; typed-define
         (if (and (pair? args*)
                  (pair? (source-code (car args*))))
             `(typed-lambda ,(car args*)
                            (typed-lambda ,(possibly-sourcify
                                            (cdr args*)
                                            args)
                                          ,@body))
             ;; Not curried:
             (letv ((vars body-expr)
                    (typed-lambda-args-expand args body begin-form
                                              type-check-error))
                   `(##lambda ,vars
                              ,body-expr))))))))


(define-macro* (typed-lambda args . body)
  (typed-lambda-expand stx args body '##begin
                       `cj-typed#type-check-error))

(TEST
 ;; Curried definitions:
 > (((typed-lambda ((x) y)
                   (list x y)) 10) 20)
 (10 20)
 ;; Other:
 > (expansion#typed-lambda (a b) 'hello 'world)
 (##lambda (a b) (##begin 'hello 'world))
 > (expansion#typed-lambda foo 'hello 'world)
 (##lambda foo (##begin 'hello 'world))
 > (expansion#typed-lambda (a [pair? b]) 'hello 'world)
 (##lambda (a b)
           (type-check/type-check-error
            cj-typed#type-check-error
            pair? b
            (##begin 'hello 'world)))
 > (expansion#typed-lambda (a [pair? b] . c) 'hello 'world)
 (##lambda (a b . c)
           (type-check/type-check-error
            cj-typed#type-check-error
            pair? b
            (##begin 'hello 'world)))
 > (expansion#typed-lambda (a [pair? b] #!rest c) 'hello 'world)
 (##lambda (a b #!rest c) (type-check/type-check-error
                           cj-typed#type-check-error
                           pair? b (##begin 'hello 'world)))
 > (expansion#typed-lambda (a [pair? b] . [number? c]) 'hello 'world)
 (##lambda (a b . c)
           (type-check/type-check-error
            cj-typed#type-check-error
            pair? b
            (type-check/type-check-error
             cj-typed#type-check-error
             number? c (##begin 'hello 'world))))
 ;;^ XX wrong? make it list-of ? (this would be a redo, sigh)
 > (expansion#typed-lambda (a #!key [pair? b] #!rest [number? c]) 'hello 'world)
 (##lambda (a #!key b #!rest c)
           (type-check/type-check-error
            cj-typed#type-check-error
            pair? b
            (type-check/type-check-error
             cj-typed#type-check-error
             number? c (##begin 'hello 'world))))
 > (expansion#typed-lambda ([pair? a] b #!optional ([number?  c] 10)) hello)
 (##lambda (a b #!optional (c 10))
           (type-check/type-check-error
            cj-typed#type-check-error
            pair? a
            (type-check/type-check-error
             cj-typed#type-check-error
             number? c (##begin hello)))))

;; and -> result checks:
(TEST
 > (expansion#typed-lambda ([pair? a] b #!optional ([number?  c] 10))
                           -> foo?
                           hello)
 (##lambda (a b #!optional (c 10))
           (type-check/type-check-error
            cj-typed#type-check-error
            pair? a
            (type-check/type-check-error
             cj-typed#type-check-error
             number? c (##begin (-> foo? hello))))))



;; ==== disabling type checks =============

(define-macro* (detyped-lambda args . body)
  `(##lambda ,(args-detype args)
     ,@(typed-body-parse stx body
                         (lambda (pred body)
                           body))))

(TEST
 > (expansion#detyped-lambda (a [pair? b] . c) 'hello 'world)
 (##lambda (a b . c)
      'hello 'world)
 > (expansion#detyped-lambda (a . [pair? b]) -> integer? 'hello 'world)
 (##lambda (a . b)
      'hello 'world)
 )

;; With a directive

(define-macro* (cj-typed-disabled#typed-lambda args . body)
  `(##lambda ,(args-detype args)
        ,@(typed-body-parse stx body
                            (lambda (pred body)
                              body))))


(define-macro* (cj-typed-disable)
  `(##namespace ("cj-typed-disabled#" typed-lambda)))

(define-macro* (cj-typed-enable)
  `(##namespace ("" typed-lambda)))


(TEST
 > (begin
     (cj-typed-enable)
     (let ((a (typed-lambda ([pair? x]) x)))
       (%try-error (a 1))))
 [error "x does not match pair?:" 1]
 > (begin
     (cj-typed-disable)
     (let ((a (typed-lambda ([pair? x]) x)))
       (%try-error (a 2))))
 2
 > (begin
     (cj-typed-enable)
     (let ((a (typed-lambda ([pair? x]) x)))
       (%try-error (a 3))))
 [error "x does not match pair?:" 3]
 )


;; ==========================================

(define (define-typed-expand stx frst+args body begin-form)
  (let ((frst+args_ (source-code frst+args)))
    (let-pair
     ((frst args) frst+args_)
     ;; allow frst to be a list, too, for curried definition; and make
     ;; sure that level allows typing, too:
     `(,(if (pair? (source-code frst))
            `define-typed
            `define)
       ,frst
       ,(typed-lambda-expand stx args body begin-form
                             `cj-typed#type-check-error)))))

(define-macro* (define-typed frst+args . body)
  (define-typed-expand stx frst+args body '##begin))

(TEST
 > (define-typed ((f [string? s]) [number? x])
     (list s x))
 > ((f "a") 7)
 ("a" 7)
 > (%try-error ((f 'a) 7))
 [error "s does not match string?:" a]
 > (%try-error ((f "a") "7"))
 [error "x does not match number?:" "7"])


;; (TEST
;;  > (require (cj-symbol)
;;          (cj-expansion)))
;; (TEST
;;  > (define TEST:equal? syntax-equal?)
;;  > (expansion define-typed (f #(integer? x) #(symbol? a)) (vector x a))
;;  ...
;;  )
;; (XX provide actual tests instead.)


(define (->-error pred-code val)
  (error "value fails to meet predicate:"
         (list pred-code (perhaps-quote val))))


;; dry implementation of in-monad; see monad/syntax.scm for the real
;; implementation, if used. (Smart, elegant, or will you curse me?)
;; (You can write spaghetti in any language.)
(define-macro* (in-monad name . body)
  `(##begin ,@body))

;; Is this hack even bigger?:

(define cj-typed#is-monad (make-table)) ;; symbol -> boolean (or absent)

(define (is-monad-name! sym)
  (assert (symbol? sym))
  (table-set! cj-typed#is-monad sym sym))

(define (is-constructor-name-for-monad! constr monadname)
  (assert (symbol? constr))
  (assert (symbol? monadname))
  (table-set! cj-typed#is-monad constr monadname))

(define-macro* (def-monad/constructors monadname . constructor-names)
  "`monadname` is without question mark; `monadname?` is automatically
understood as a predicate for `monadname`. `constructor-names` should
give any type constructor names that return a predicate for the
`monadname` type."
  (assert* symbol? monadname)
  (assert* (list-of (source-of symbol?)) constructor-names)
  `(begin
     ;; XX simply assumes `monadname` itself is a predicate or
     ;; constructur for monadname
     (is-monad-name! ',monadname)
     ;; Not "constructor-name", XX bad naming, blah.
     (is-constructor-name-for-monad! ',(symbol-append (source-code monadname)
                                                      "?")
                                     ',monadname)
     ,@(map (lambda (cname)
              `(is-constructor-name-for-monad! ',cname ',monadname))
            constructor-names)))

(define (maybe-monad-name v) ;; -> (maybe symbol?)
  "returns the monad name given a type constructor name, if any"
  (and (symbol? v)
       (table-ref cj-typed#is-monad v #f)))


(define (predicate-symbol? v)
  (and (symbol? v)
       (let* ((s (symbol->string v))
              (len (string-length s)))
         (and (>= len 2)
              (eq? (string-ref s (dec len)) #\?)))))

(TEST
 > (predicate-symbol? 'v)
 #f
 > (predicate-symbol? '?)
 #f
 > (predicate-symbol? 't?)
 #t
 > (predicate-symbol? "t?")
 #f)


(define-macro* (-> pred . body)
  (if (eq? (source-code pred) '!)
      ;; Special case: never returning; thus no need to check either;
      ;; XX better would be to throw an execption if it returns; but
      ;; solve TCO issue first!
      `(begin ,@body)

      ;; Normal case:
      (let ((maincode
             (with-gensym V
                          `(##let ((,V (##let () ,@body)))
                                  (##if (,pred ,V) ,V
                                        (->-error ',pred ,V))))))
        (let ((pred* (source-code pred))
              (lookup (lambda (typeconstructor-name)
                        (cond ((maybe-monad-name typeconstructor-name)
                               => (lambda (monadname)
                                    `(in-monad ,monadname ,maincode)))
                              (else
                               maincode)))))
          (cond ((pair? pred*)
                 (lookup (source-code (car pred*))))
                ((predicate-symbol? pred*)
                 (lookup pred*))
                (else
                 maincode))))))

;; and for easy disabling:
(define-macro* (@-> pred . body)
  `(begin
     ,@body))

(TEST
 > (-> number? 5)
 5
 > (-> number? "bla" 5)
 5
 > (%try-error (-> number? "5"))
 [error "value fails to meet predicate:" (number? "5")]
 )

;; test source location propagation
(TEST
 > (def e (with-exception-catcher
           identity
           (&
            (eval
             (quote-source
              ;; missing actual body, triggering the message that we
              ;; want to test
              (typed-lambda (x) -> echz))))))
 > (source-error-message e)
 "a body starting with -> needs at least 2 more forms"
 > (source? (source-error-source e))
 #t)
