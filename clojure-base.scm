;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.

(require easy)

(export (macro use-clojure-base))


(defmacro (use-clojure-base)
  `(##namespace ("clojure#" defn fn false true nil nil? = not if if-not when let
                 false? true?
                 lazy-seq
                 seq sequence chunked-seq? first rest next)))

(##namespace (""))
;; ^ needed when re-loading this file via "load" when tests ran. Sigh,
;; namespaces

;; This uses unsafe ops, as length is supposed to be used and this
;; code only used with correct number of argumetns in VS
(def (scheme:@bind-dispatch sbinds body VS)
     (let rec ((sbinds sbinds))
       (cond ((null? sbinds)
              `(##begin ,@body))
             ((pair? sbinds)
              (let-pair ((V sbinds) sbinds)
                        `(##let ((,V (##car ,VS))
                                 (,VS (##cdr ,VS)))
                                ,(rec sbinds))))
             (else
              ;; rest argument
              `(##let ((,sbinds ,VS))
                      ,@body)))))

(TEST
 > (scheme:@bind-dispatch '() '(body0 body1) 'GEN:VS)
 (##begin body0 body1)
 > (scheme:@bind-dispatch '(a) '(body0 body1) 'GEN:VS)
 (##let ((a (##car GEN:VS)) (GEN:VS (##cdr GEN:VS))) (##begin body0 body1))
 ;; ^ could optimize away the last ##cdr call; although compiler
 ;; 'should' rather do that.
 > (scheme:@bind-dispatch '(a b) '(body0 body1) 'GEN:VS)
 (##let ((a (##car GEN:VS)) (GEN:VS (##cdr GEN:VS)))
        (##let ((b (##car GEN:VS)) (GEN:VS (##cdr GEN:VS))) (##begin body0 body1)))
 > (scheme:@bind-dispatch '(a b . c) '(body0 body1) 'GEN:VS)
 (##let ((a (##car GEN:VS)) (GEN:VS (##cdr GEN:VS)))
        (##let ((b (##car GEN:VS)) (GEN:VS (##cdr GEN:VS)))
               (##let ((c GEN:VS)) body0 body1))))


(def (clojure->scheme-args args)
     (assert* vector? args
              (lambda (args*)
                (let (rl (vector.list-reverse args*))
                  (if (and (pair? rl)
                           (let (rl* (cdr rl))
                             (and (pair? rl*)
                                  (eq? (source-code (car rl*)) '&))))
                      (append-reverse (cddr rl) (car rl))
                      (reverse rl))))))

(def (clojure:fixbody body)
     (if (null? body)
         `((void))
         body))

(def (clojure:arity-error len)
     (error "missing definition for arity:" len))

(defmacro (clojure#defn . args)
  (let*-values
      (((name args) (if (pair? args)
                        (values (car args) (cdr args))
                        (source-error stx "missing function name")))
       ((docstrings args) (if (and (pair? args)
                                   (string? (source-code (car args))))
                              (values (list (car args)) (cdr args))
                              (values (list) args)))
       ;; ((maybe-attributes args)
       ;;  ...)
       )
    (assert* symbol? name)
    (if (pair? args)
        (let-pair
         ((a args*) args)
         (mcase a
                (vector?
                 `(def ,name (##lambda ,(clojure->scheme-args a)
                                  ,@docstrings
                                  ,@(clojure:fixbody args*))))
                (pair?
                 ;; multiple-match form
                 ;; (M (list 3 4) ((a b) b))  gah not supported yet.
                 ;;  and not optimized anyway.
                 (with-gensyms
                  (VS LEN)
                  (let (cases ;; (values len body orig-binds)
                        (map (lambda (c)
                               (assert*
                                pair? c
                                (lambda (c*)
                                  (let-pair
                                   ((binds body) c*)
                                   (let (sbinds (clojure->scheme-args binds))
                                     (values
                                      (improper-length sbinds)
                                      (scheme:@bind-dispatch sbinds
                                                             (clojure:fixbody body)
                                                             VS)
                                      binds))))))
                             args))
                    (let ((real-cases (filter (comp (complement negative?) fst)
                                              cases))
                          (else-cases (filter (comp negative? fst)
                                              cases)))

                      (let ((duplicate-cases (=> (sort real-cases (on fst <))
                                                 (group-by (on fst =))
                                                 (.filter (=>* length ((C > _ 1)))))))
                        (if
                         (pair? duplicate-cases)
                         (source-error (3rd (caar duplicate-cases))
                                       "Can't have 2 overloads with same arity")

                         (if-let (bad (and (pair? else-cases)
                                           (let* ((toolargelen (- (fst (first else-cases))))
                                                  (bad (filter (lambda (c)
                                                                 (>= (fst c) toolargelen))
                                                               real-cases)))
                                             (and (pair? bad)
                                                  (first bad)))))
                                 (source-error
                                  (3rd bad)
                                  "Can't have fixed arity function with more params than variadic function")
                                 `(def (,name . ,VS)
                                       ,@docstrings
                                       (##let
                                        ((,LEN (length ,VS)))
                                        (##case
                                         ,LEN
                                         ,@(map (lambda-values ((len code orig-binds))
                                                          `((,len) ,code))
                                                real-cases)
                                         (else
                                          ,(cond ((null? else-cases)
                                                  `(clojure:arity-error ,LEN))
                                                 (((list-of-length 1) else-cases)
                                                  (letv ((len code orig-binds) (car else-cases))
                                                        `(##if (> ,LEN ,len)
                                                               ,code
                                                               (clojure:arity-error ,LEN))))
                                                 (else
                                                  (source-error
                                                   stx
                                                   "Can't have more than 1 variadic overload"
                                                   ))))))))))))))))
        (source-error stx "missing function arguments/body"))))


(TEST
 > (use-clojure-base)

 > (defn t "fun"
     ([a b] (list 2 a b))
     ([c] (list 1 c))
     ([x y z] (list 3 x z)))
 > (t 10)
 (1 10)
 > (%try (t))
 (exception text: "missing definition for arity: 0\n")
 > (%try (t 3 4 5 6))
 (exception text: "missing definition for arity: 4\n")
 > (t 3 4)
 (2 3 4)
 > (t 3 4 5)
 (3 3 5)

 > (defn t
     [a b & r] (list a b r))
 > (%try (t 1))
 (exception text: "Wrong number of arguments passed to procedure\n(t 1)\n")
 > (t 2 3)
 (2 3 ())
 > (t 2 3 4 5)
 (2 3 (4 5))

 > (with-exception-catcher
    source-error-message
    (& (eval '(defn t
                ([a b & r] (list 'first a b r))
                ([a b] (list 'second a b))
                ([a b c & r] (list 'third a b c))))))
 "Can't have more than 1 variadic overload"
 > (with-exception-catcher
    source-error-message
    (& (eval '(defn t
                ([a b & r] (list 'first a b r))
                ([a b] (list 'second a b))
                ([c d] (list 'third c d))))))
 "Can't have 2 overloads with same arity"
 > (with-exception-catcher
    source-error-message
    (& (eval '(defn t
                ([a b & r] (list 'first a b r))
                ([a b] (list 'second a b))
                ([a b c] (list 'third a b c))))))
 "Can't have fixed arity function with more params than variadic function"
 > (defn t
     ([a b & r] (list 'first a b r))
     ([a b] (list 'second a b)))
 > (t 3 4)
 (second 3 4)
 > (t 3 4 5)
 (first 3 4 (5)))


(defmacro (clojure#fn arg0 . args)
  (let*-values (((maybe-name args)
                 (mcase arg0
                        (symbol?
                         (values arg0 args))
                        (else
                         (values #f (cons arg0 args)))))
                ((binds args)
                 (if (null? args)
                     (source-error stx "fn: not enough arguments")
                     (let-pair
                      ((a args) args)
                      (mcase a
                             (vector?
                              (values a args))
                             (else
                              (source-error stx "fn: missing bindings list")))))))
    (let ((code `(lambda ,(clojure->scheme-args binds) ,@args)))
      (if maybe-name
          `(named ,maybe-name ,code)
          code))))


(def clojure#false #f)
(def clojure#true #t)
(def clojure#nil 'clojure#nil)
;; ^ performance improvement: just use #!void, test then can become a
;; single bit masking, right? As soon as I know I don't really profit
;; from "know that this came from Clojure code using nil".
(def (clojure#nil? v)
     (eq? v 'clojure#nil))

(TEST
 > (use-clojure-base)
 > (nil? '())
 #f
 > (nil? nil)
 #t
 ;; > (nil? 'nil)
 ;; true
 )


(def (clojure-=/2 a b)
     (FV (a b)
         (let retry ((a a)
                     (b b))
           (cond ((pair? a)
                  (cond ((pair? b)
                         ;; heh back at treating pairs as
                         ;; cdr-type-flexible entities, since
                         ;; clojure#concat will re-use vectors as cdr,
                         ;; at least currently (not sure what Clojure
                         ;; does internally)
                         (let-pair ((a0 a) a)
                                   (let-pair ((b0 b) b)
                                             (and (clojure-=/2 a0 b0)
                                                  (clojure-=/2 a b)))))
                        ((vector? b)
                         (let (len (vector-length b))
                           (let lp ((s a)
                                    (i 0))
                             (FV (s)
                                 (if (null? s)
                                     (= i len)
                                     (if (< i len)
                                         (let-pair ((s0 s*) s)
                                                   (and (clojure-=/2
                                                         s0 (vector-ref b i))
                                                        (lp s*
                                                            (inc i))))
                                         #f))))))
                        ((string? b)
                         #f)
                        (else
                         (WARN "don't know how to handle these" a b)
                         (equal? a b))))
                 ((vector? a)
                  (if (vector? b)
                      (let ((la (vector-length a))
                            (lb (vector-length b)))
                        (and (= a b)
                             (let lp ((i 0))
                               (if (< i len)
                                   (and (clojure-=/2 (vector-ref a i)
                                                     (vector-ref b i))
                                        (lp (inc i)))
                                   #t))))
                      ;; save flipped duplication
                      (retry b a)))
                 ;; (((either number? string?) a)
                 ;;  (equal? a b))
                 ;; ((boolean? a)
                 ;;  (if (boolean? b)
                 ;;      (eq? a b)
                 ;;      (retry b a)))
                 ;; ((eq? a 'clojure#nil)
                 ;;  )
                 (else
                  (equal? a b))))))

(def (clojure#= a b . rest)
     ;; Clojure allows calling it with a single argument; what point
     ;; is that for? Try to avoid alloc.
     (and (clojure-=/2 a b)
          (or (null? rest)
              (every (lambda (v)
                       ;; or should it be the one before each time ??
                       (clojure-=/2 a v))
                     rest))))

(TEST
 >  (clojure#= '(a b c . [d e f]) '(a b c d e f))
 #t
 ;; This was done since (concat '[a b c] '[d e f]) used to return that
 ;; value in the first argument. Not any more. Leave it as is?
 )


(TEST
 > (use-clojure-base)
 > (= true true)
 #t
 > (= true false)
 #f
 > (= false false)
 #t
 > (= false nil)
 #f
 > (= false '())
 #f
 > (= nil '())
 #f ;; false
 > (= nil nil)
 #t ;; true

 > (= '(a) '[a])
 #t ;; true
 > (= '(a) '[a b])
 #f ;; false
 > (= '(a) '"a")
 #f ;; false
 > (= '"a" '"a")
 #t ;; true
 > (= '"a" '"ab")
 #f ;; false
 )


(insert-result-of
 (let ((test '(or (eq? val #f)
                  ;; (eq? val '()) NO! This is true in Clojure!
                  ;; Heh. Well, convenient.
                  (eq? val 'clojure#nil)
                  (eq? val #!void))))
   `(begin
      (def (clojure#not val) ,test)
      (def (clojure#not-not val) (##not ,test)))))

(def (clojure#false? v)
     (eq? v #f))

(def (clojure#true? v)
     (eq? v #t))


(defmacro (clojure#if test yes #!optional (no 'clojure#nil))
  `(##if (clojure#not-not ,test)
         ,yes
         ,no))

(defmacro (clojure#if-not test  yes #!optional (no 'clojure#nil))
  `(##if (clojure#not ,test)
         ,yes
         ,no))

(defmacro (clojure#when test . branches)
  `(##if (clojure#not-not ,test)
         (begin ,@branches)
         clojure#nil))


(TEST
 > (use-clojure-base)
 > (if '[] 'y 'n)
 y
 > (if '() 'y 'n)
 y
 > (if nil 'y 'n)
 n)


(defmacro (clojure#let binds . body)
  (mcase binds
         (vector?
          `(##let ,(sourcify
                    (sequential-pairs (=> binds source-code vector.list)
                                      list)
                    binds)
                  ,@body))
         ;; fall back to Scheme
         ((either pair? symbol?)
          `(easy#let ,binds ,@body))))

(def (clojure#first v)
     (FV (v)
         (cond ((pair? v)
                (car v))
               ((null? v)
                clojure#nil)
               (else
                (clojure#first (.stream v))))))


(def (clojure#rest v)
     (FV (v)
         (cond ((pair? v)
                (cdr v))
               ((null? v)
                '())
               (else
                (clojure#rest (.stream v))))))

(def clojure#next (comp clojure#seq clojure#rest))

(TEST
 > (use-clojure-base)
 > (def os '(() [] (a) [a] (a b) [a b]))
 > (F (map next os))
 (clojure#nil clojure#nil clojure#nil clojure#nil (b) (b))
 > (F (map rest os))
 (() () () () (b) (b))
 > (F (map first os))
 (clojure#nil clojure#nil a a a a))



(def (empty-vector? v)
     (and (vector? v)
          (zero? (vector-length v))))

(def (clojure#seq v)
     (FV (v)
         (case v
           ((clojure#nil ()) clojure#nil)
           (else
            (if-let* ((null? (CAN. .null? v))
                      ;; Scheme's empty?
                      (_ (null? v)))
                     clojure#nil
                     (.stream v))))))

(TEST
 > (use-clojure-base)
 > (define TEST:equal? =)
 > (seq "foo")
 (#\f #\o #\o) ;; (\f \o \o)
 > (seq '())
 clojure#nil
 > (seq nil)
 clojure#nil
 > (seq '[])
 clojure#nil
 > (if (seq '()) 'y 'n)
 n
 > (if (seq (seq '())) 'y 'n)
 n)

(def (clojure#sequence v)
     (case v
       ((clojure#nil) '())
       (else (.stream v))))

(TEST
 > (use-clojure-base)
 > (F (seq "foo"))
 (#\f #\o #\o)
 > (sequence '())
 ()
 > (sequence nil)
 ()
 > (F (sequence '[]))
 ())



(def (clojure-lazy-seq-return v)
     (cond ((clojure#nil? v)
            '())
           (((either pair? null? promise? ) v)
            v)
           (else
            (.stream v))))

(defmacro (clojure#lazy-seq . body)
  "Takes a body of expressions that returns an ISeq or nil, and yields
  a Seqable object that will invoke the body only the first time seq
  is called, and will cache the result and return it on all subsequent
  seq calls. See also - realized?"
  `(delay (clojure-lazy-seq-return (##begin ,@body))))



(def (clojure#chunked-seq? v)
     ;; XX will have to change i guess, or how does this all work, and
     ;; what is the purpose?
     ;;(istream? v)
     #f)


(TEST
 > (use-clojure-base)
 > (chunked-seq? '())
 #f ;; false
 > (chunked-seq? '(a))
 #f ;; false
 > (chunked-seq? (seq '(a)))
 #f ;; false
 ;; > (chunked-seq? (seq '[a]))
 ;; #t ;; true  XX implement chunked sequences
 > (chunked-seq? (seq '[]))
 #f ;; false

 > (seq nil)
 clojure#nil ;; nil
 > (F (lazy-seq nil))
 ()
 > (def s (lazy-seq '[a b]))
 > (promise? s)
 #t
 > (F s)
 (a b)
 > (def s (lazy-seq "foo"))
 > (promise? s)
 #t
 > (F s)
 (#\f #\o #\o)
 > (if (lazy-seq nil) ;; well that is '() anyway, even if eager
       'y 'n)
 y)



;;  SGH. When does Clojure force things? lazy delay it does? Q?

(TEST
 > (use-clojure-base)
 ;; Apparently does not auto-evaluate promises from delay:
 > (if (delay nil) 'y 'n)
 y
 > (if nil 'y 'n)
 n
 ;; 
 > (F (lazy-seq '()))
 ()
 > (F (lazy-seq nil))
 ()
 > (if (lazy-seq '()) 'y 'n)
 y
 > (if (lazy-seq nil) 'y 'n)
 y
 > (if (seq (lazy-seq nil)) 'y 'n)
 n
 > (if (seq (lazy-seq '())) 'y 'n)
 n

 > (%try (seq (delay nil)))
 ;; Execution error (IllegalArgumentException) at do4clojure.core/eval1636 (form-init6112985740636491022.clj:1).
 ;; Don't know how to create ISeq from: clojure.lang.Delay
 (value clojure#nil) ;; XX only can fix via separate promise type, right?
 > (F (lazy-seq (delay nil)))
 ;; Error printing return value (IllegalArgumentException) at clojure.lang.RT/seqFrom (RT.java:553).
 ;; Don't know how to create ISeq from: clojure.lang.Delay
 clojure#nil ;; XX ditto
 )


;; XX move to some clojure-utils.scm ?

(def (make-sexpr-map atom-fn)
     (named rec
            (lambda (v)
              (cond ((pair? v)
                     (let-pair ((a r) v)
                               (cons (rec a)
                                     (rec r))))
                    ((vector? v)
                     (vector-map rec v))
                    (else
                     (atom-fn v))))))

(def clojure-symbols->scheme
     (make-sexpr-map (lambda (v)
                       (case v
                         ((true) #t)
                         ((false) #f)
                         ((nil) 'clojure#nil)
                         (else
                          v)))))

;; also see .show-clojure
(def scheme->clojure-symbols
     (make-sexpr-map (lambda (v)
                       (case v
                         ((#t) 'true)
                         ((#f) 'false)
                         ((clojure#nil) 'nil)
                         (else
                          v)))))
;; /XX


(TEST
 > (use-clojure-base)
 ;; > (map false? '[nil false true "" [] ()])
 ;; OK; I don't have seq wrapper and printer yet, OK could use =, but,
 ;; also can't quote vector here since we don't specially parse nil,
 ;; false, true.
 > (def vs (list nil false true "" '[] '()))
 > (map false? vs)
 (#f #t #f #f #f #f)
 > (map true? vs)
 (#f #f #t #f #f #f)
 > (map not vs)
 (#t #t #f #f #f #f))


