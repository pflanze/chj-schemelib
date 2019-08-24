;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.

(require easy
         (cj-source-util-2 form-unquote-splicing?))

(export (macro use-clojure-base))

(defmacro (use-clojure-base)
  `(##namespace ("clojure#" defn fn defmacro quasiquote macroexpand macroexpand-1
                 false true nil nil? = not if if-not when let if-let cond
                 false? true? keyword?
                 lazy-seq
                 seq sequence chunked-seq? first rest next)))

(defmacro (scheme . body)
  `(##let ()
          (##namespace (""))
          ,@body))



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


(defclass (clojure-definition-case [fixnum? arity]
                                   binds-scheme
                                   binds-clojure
                                   body-clojure)
  (defmethod (body-scheme _ VS)
    (scheme:@bind-dispatch binds-scheme
                           (clojure:fixbody body-clojure)
                           VS)))

(defclass (clojure-definition [(source-of symbol?) name]
                              [(list-of (source-of string?)) docstrings]
                              [false? attributes] ;; currently unused
                              )

  (defclass (clojure-definition-singlecase binds
                                           [list? body])
    (defmethod (scheme-code s DEF name-prefixer)
      `(,DEF (,(name-prefixer name) ,@(clojure->scheme-args binds))
             ,@docstrings
             ,@(clojure:fixbody body))))

  (defclass (clojure-definition-multicase [(list-of clojure-definition-case?) real-cases]
                                          [(list-of clojure-definition-case?) else-cases])
    (defmethod (scheme-code s DEF name-prefixer)
      (with-gensyms
       (VS LEN)
       `(,DEF (,(name-prefixer name) . ,VS)
              ,@docstrings
              (##let
               ((,LEN (length ,VS)))
               (##case
                ,LEN
                ,@(map (lambda (c)
                         `((,(.arity c)) ,(.body-scheme c VS)))
                       real-cases)
                (else
                 ,(cond ((null? else-cases)
                         `(clojure:arity-error ,LEN))
                        (((list-of-length 1) else-cases)
                         (let (s (car else-cases))
                           `(##if (> ,LEN ,(.arity s))
                                  ,(.body-scheme s VS)
                                  (clojure:arity-error ,LEN))))
                        (else
                         (source-error
                          (second else-cases)
                          "Can't have more than 1 variadic overload")))))))))))


(def (clojure:definition-parse stx args)
     (let*-values
         (((name args) (if (pair? args)
                           (values (car args) (cdr args))
                           (source-error stx "missing definition name")))
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
                    (clojure-definition-singlecase name
                                                   docstrings
                                                   #f
                                                   a
                                                   args*))
                   (pair?
                    ;; multiple-match form
                    ;; (M (list 3 4) ((a b) b))  gah not supported yet.
                    ;;  and not optimized anyway.
                    (let (cases ;; (list-of clojure-definition-case)
                          (map (lambda (c)
                                 (assert*
                                  pair? c
                                  (lambda (c*)
                                    (let-pair
                                     ((binds body) c*)
                                     (let (sbinds (clojure->scheme-args binds))
                                       (clojure-definition-case (improper-length sbinds)
                                                                sbinds
                                                                binds
                                                                body))))))
                               args))
                      (let ((real-cases (filter (comp (complement negative?) .arity)
                                                cases))
                            (else-cases (filter (comp negative? .arity)
                                                cases)))

                        (let ((duplicate-cases (=> (sort real-cases (on .arity <))
                                                   (group-by (on .arity =))
                                                   (.filter (=>* length ((C > _ 1)))))))
                          (if
                           (pair? duplicate-cases)
                           (source-error (.binds-clojure (second (first duplicate-cases)))
                                         "Can't have 2 overloads with same arity")

                           (if-let (bad (and (pair? else-cases)
                                             (let* ((toolargelen (- (.arity (first else-cases))))
                                                    (bad (filter (lambda (c)
                                                                   (>= (.arity c) toolargelen))
                                                                 real-cases)))
                                               (and (pair? bad)
                                                    (first bad)))))
                                   (source-error
                                    (.binds-clojure bad)
                                    "Can't have fixed arity function with more params than variadic function")
                                   (clojure-definition-multicase name
                                                                 docstrings
                                                                 #f
                                                                 real-cases
                                                                 else-cases)))))))))
           (source-error stx "missing function arguments/body"))
       
       ))

(defmacro (clojure#defn . args)
  (=> (clojure:definition-parse stx args)
      (.scheme-code `def identity)))

(def (in-namespace namespace sym)
     (assert*
      symbol? sym
      (lambda (sym*)
        (let* ((str (symbol.string sym*))
               (prefix-it
                (lambda ()
                  (assert*
                   string? namespace
                   (lambda (ns)
                     (=> (string-append ns str)
                         string.symbol
                         (possibly-sourcify sym)))))))
          (if-let (i (string-find-char str #\#))
                  (let (len (string.length str))
                    (if (= i (dec len))
                        (prefix-it)
                        sym))
                  (prefix-it))))))

(TEST
 > (in-namespace "foo#" 'bar)
 foo#bar
 > (in-namespace "foo#" 'bar#)
 foo#bar#
 > (in-namespace "foo#" 'bar#baz)
 bar#baz)

(defmacro (clojure#defmacro . args)
  (=> (clojure:definition-parse stx args)
      (.scheme-code `define-macro*
                    ;; XX will need a way to get the namespace of the
                    ;; current module! (Or change define-macro*
                    ;; implementation); for now HACK in just clojure#
                    ;; as a constant.
                    (C in-namespace "clojure#" _))))

;; Clojure's macroexpand is a function, not a macro!
(def (clojure#macroexpand e)
     (cj-desourcify (macro-star-expand e)))

(def (clojure#macroexpand-1 e)
     (cj-desourcify (macro-star-expand-1 e)))


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
    (& (eval (quote-source (defn t
                             ([a b & r] (list 'first a b r))
                             ([a b] (list 'second a b))
                             ([a b c & r] (list 'third a b c)))))))
 "Can't have more than 1 variadic overload"
 > (with-exception-catcher
    source-error-message
    (& (eval (quote-source (defn t
                             ([a b & r] (list 'first a b r))
                             ([a b] (list 'second a b))
                             ([c d] (list 'third c d)))))))
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
 (first 3 4 (5))

 > (defmacro t-fixx [x] x)
 > (##namespace ("clojure#" t-fixx)) ;; *Sigh* is this a hack now.
 > (t-fixx 10)
 10
 > (defmacro t-fixx
     ([x] x)
     ([x y] (list '* x y)))
 > (t-fixx 10)
 10
 > (t-fixx 10 20)
 200)


;; Hack to get ~foo and foo~ syntax since we don't have custom reader
;; yet.
(def (clojure:quasiquote-symbol-kind sym)
     (let* ((str (symbol.string (source-code sym)))
            (len (string.length str)))
       (if (< len 2)
           'none
           (if (eq? (string.ref str 0) #\~)
               'unquote
               (if (eq? (string.ref str (dec len)) #\#)
                   'gensym
                   'none)))))

(def (clojure:quasiquote-unquote->value sym)
     (let* ((str (symbol.string (source-code sym)))
            (len (string.length str)))
       (if (< len 2)
           (error "not a  ~symbol")
           (if (eq? (string.ref str 0) #\~)
               (let ((str* (substring str 1 len)))
                 (or (string->number str*)
                     (string->symbol str*)))
               (error "not a  ~symbol")))))

(def (clojure:quasiquote-gensym->symbol sym)
     (let* ((str (symbol.string (source-code sym)))
            (len (string.length str)))
       (if (< len 2)
           (error "not a symbol#")
           (if (eq? (string.ref str (dec len)) #\#)
               (let ((str* (substring str 0 (dec len))))
                 (string->symbol str*))
               (error "not a symbol#")))))

(defmacro (clojure#quasiquote e)
  "Supporting ~foo (etc.?) via HACK; also still supporting Scheme's
unquote and unquote-splicing at the same time"
  (let (gensyms (table))
    (let expand ((e e))
      (mcase e
             (pair?
              (let-pair
               ((a e*) (source-code e))
               (case (source-code a)
                 ((~)
                  (let (e* (source-code e*))
                    ;; ( ^ shouldn't have any source anyway)
                    (if (pair? e*)
                        (let-pair
                         ((b e**) e*)
                         ;; no need for "generate |unquote|"
                         ;; business. just 'do' it ":)"
                         `(cons ,b
                                ,(expand e**)))
                        ;; Clojure fails this at read time already,
                        ;; but we're "hacked"...:
                        (source-error a "missing item after ~"))))

                 ((unquote)
                  (let (e* (source-code e*))
                    (if (pair? e*)
                        (let-pair
                         ((b e**) e*)
                         (if (null? e**)
                             b
                             `(cons ',a
                                    ,(expand e*))))
                        `(cons ',a
                               ,(expand e*)))))
                             
                 (else
                  (mcase a

                         (form-unquote-splicing?
                          (let (b (cadr (source-code a)))
                            (if (null? e*) ;; optim
                                b
                                `(append ,b
                                         ,(expand e*)))))
                      
                         (else
                          `(cons ,(expand a)
                                 ,(expand e*))))))))
             (null?
              `'())
             (vector?
              (source-error e "unfinished"))
             (symbol?
              (xcase (clojure:quasiquote-symbol-kind e)
                     ((unquote)
                      (clojure:quasiquote-unquote->value e))
                     ((gensym)
                      `',(let (sym (clojure:quasiquote-gensym->symbol e))
                           (or (table-ref gensyms sym #f)
                               (let (sym* (gensym sym))
                                 (table-set! gensyms sym sym*)
                                 sym*))))
                     ((none)
                      `',e)))
             ((either string? number?) ;; self-quoting
              e)
             (else
              (source-error e "unfinished2"))))))

(TEST
 > (use-clojure-base)
 > (expansion#clojure#quasiquote (a 1))
 (cons 'a (cons 1 '()))
 > (expansion#clojure#quasiquote (a ~1))
 (cons 'a (cons 1 '()))
 > (clojure#quasiquote (a ~1))
 (a 1)
 > (clojure#quasiquote (a ~(+ 1 2)))
 (a 3)
 > (expansion#clojure#quasiquote (a ~ 1))
 (cons 'a (cons 1 '()))
 > (expansion#clojure#quasiquote (a b (c) 1))
 (cons 'a (cons 'b (cons (cons 'c '()) (cons 1 '()))))
 > (eval #)
 (a b (c) 1)
 > (expansion#clojure#quasiquote (a b (~ c) 1))
 (cons 'a (cons 'b (cons (cons c '()) (cons 1 '()))))
 > (def c 13)
 > (clojure#quasiquote (a b (~ c) 1))
 (a b (13) 1)
 > `(a b (~c d) 1)
 (a b (13 d) 1)
 ;; ^ all of these: XX: pending: namespacing

 ;; Still support Scheme, too:
 > `(a b unquote c)
 (a b . 13)
 > `(a b unquote c d)
 (a b unquote c d)
 > `(a ,@(list 1 2))
 (a 1 2)
 > `(,@(list 1 2))
 (1 2)
 > `,@(list 1 2)
 ,@(list 1 2)
 > `(a b (,c ,@(list 1 2) d) 1)
 (a b (13 1 2 d) 1)

 > (defmacro t-fixx ([x] x) ([x y] `(/ ~x ~ y)))
 > (t-fixx 10 20)
 1/2
 ;;> (##namespace ("clojure#" t-fixx)) ;; *Sigh* is this a hack now.[again]
 ;; oh that doesn't even work. TOO BAD XX.
 > (macroexpand (quote-source (clojure#t-fixx 10 20)))
 (/ 10 20)
 > (defmacro t-fixx
     ([x] x)
     ([x y]
      (first x)
      `(list (first ~y) ~ x)))
 > (macroexpand (quote-source (clojure#t-fixx (clojure#t-fixx "10" "30") "20")))
 (list (first "20") (clojure#t-fixx "10" "30"))
 ;; (clojure.core/list (clojure.core/first "20") (t-fixx "10" "30"))
 > (eval #)
 (#\2 (#\3 "10"))

 ;; Gensym
 > (define TEST:equal? syntax-equal?)
 > `(foo# foo# bar#)
 ;; (foo__1949__auto__ foo__1949__auto__ bar__1950__auto__)
 (GEN:foo-1 GEN:foo-1 GEN:bar-2)
 > (%try `(~foo#))
 ;; Syntax error compiling at (form-init6177359002304238130.clj:1:1621).
 ;; Unable to resolve symbol: foo# in this context
 (exception text: "Unbound variable: foo#\n"))




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


(def (clojure#keyword? v)
     (or ((scheme keyword?) v) ;; ?
         (and ((scheme symbol?) v)
              (let* ((s (symbol.string v))
                     (len (string-length s)))
                (and (>= len 1)
                     (eq? (string-ref s 0) #\:))))))


(defmacro (clojure#cond . cases)
  (if (even? (length cases))
      `(##cond ,@(map (lambda-pair ((t e))
                              `(,(if (clojure#keyword? (source-code t))
                                     `',t
                                     `(clojure#not-not ,t))
                                ,e))
                      (sequential-pairs cases cons))
               ;; simply always provide an else case is easiert.. (let
               ;; compiler optim it away, ok?). Also don't use |else|
               ;; here? Ah Gambit would guarantee it though. But that's
               ;; not R5RS, right?
               (#t clojure#nil))
      (source-error stx "cond requires an even number of forms")))

(TEST
 > (use-clojure-base)
 > (cond true 'a)
 a
 > (cond false 'a)
 clojure#nil
 > (cond false 'a true 'b)
 b
 > (cond false 'a true 'b else 'c)
 ;; Syntax error compiling at (form-init5831580352679050351.clj:1:1).
 ;; Unable to resolve symbol: else in this context
 b
 ;; ^ because Gambit doesn't stop statically.
 > (%try (cond false 'a else 'c))
 (exception text: "Unbound variable: else\n")
 > (with-exception-catcher
    source-error-message
    (& (eval (quote-source (cond false)))))
 ;; Syntax error macroexpanding cond at (form-init5831580352679050351.clj:1:1).
 ;; cond requires an even number of forms
 "cond requires an even number of forms"
 > (cond false 'a true 'b :else 'c)
 b
 > (cond false 'a nil 'b :else 'c)
 c
 > (cond false 'a '() 'b :else 'c)
 b
 > (cond false 'a :else 'c '() 'b)
 c
 ;; :else is just taken as keyword, true value
 > (cond false 'a nil 'b :eff 'c)
 c)


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


(def (myscheme#cond-expand cases IF use-else-keyword? no-match)
     (fold-right
      (lambda (c res)
        (assert*
         pair? c
         (lambda (c*)
           (let-pair
            ((t c*) c*)
            (if (and use-else-keyword?
                     (eq? (source-code t) 'else))
                (if (eq? res no-match)
                    `(##begin ,@c*)
                    (source-error c "else clause must be last"))
                (let ((normal (lambda ()
                                `(,IF ,t
                                      (##begin ,@c*)
                                      ,res))))
                  (if (pair? c*)
                      (let-pair
                       ((a c**) c*)
                       (if (eq? (source-code a) '=>)
                           (with-gensym
                            V
                            `(##let ((,V ,t))
                                    (,IF ,V
                                         (,@c** ,V)
                                         ,res)))
                           (normal)))
                      (normal))))))))
      no-match
      cases))

(defmacro (myscheme#cond . cases)
  (myscheme#cond-expand cases
                        `##if ;; hygienic
                        #t
                        `(##void)))

(defmacro (myscheme-unhygienic#cond . cases)
  (myscheme#cond-expand cases
                        `if
                        #f ;; OK? used for generated code anyway, so,
                           ;; fine ?
                        `(void)))


(TEST
 > (define TEST:equal? syntax-equal?)
 > (expansion#myscheme#cond ((x v) (newline) #t) (ELSE 'fun))
 (##if (x v)
       (##begin (newline) #t)
       (##if ELSE
             (##begin 'fun)
             (##void)))
 > (expansion#myscheme#cond ((x v) (newline) #t) (else 'fun))
 (##if (x v)
       (##begin (newline) #t)
       (##begin 'fun))
 > (expansion#myscheme-unhygienic#cond ((x v) (newline) #t) (else 'fun))
 (if (x v)
     (##begin (newline) #t)
     (if else
         (##begin 'fun)
         (void)))

 > (expansion#myscheme-unhygienic#cond ((x v) (newline) #t) ((dd) => meh) (else 'fun))
 (if (x v)
     (##begin (newline) #t)
     (##let ((GEN:V-10932 (dd)))
            (if GEN:V-10932
                (meh GEN:V-10932)
                (if else
                    (##begin 'fun)
                    (void))))))

(defmacro (clojure#if-let binds yes #!optional (no `clojure#nil))
  (mcase binds
         (vector?
          ;; Thanks for offering me if-let-expand (working around ""
          ;; namespace macro referral issue--OK actually have got
          ;; access to expander# anyway, thanks myself again) and
          ;; taking cond form as a parameter (was already there)!
          ;; Doesn't allow to directly parameterize the boolean
          ;; testing, but we can use an indirection via a custom cond
          ;; macro.
          (if-let-expand `myscheme-unhygienic#cond
                         (=> binds
                             source-code
                             vector->list
                             list
                             (sourcify binds))
                         yes no))
         ;; fall back to Scheme
         ((either pair? symbol?)
          (if-let-expand `##cond binds yes no))))

(TEST
 > (use-clojure-base)
 ;; > (if-let [a '()])
 ;; Syntax error macroexpanding clojure.core/if-let at (form-init5831580352679050351.clj:1:1).
 ;; () - failed: Insufficient input at: [:then]
 > (if-let [a nil] (list a) 'n)
 n
 > (if-let [a ""] (list a) 'n)
 ("")
 > (if-let [a '()] (list a) 'n)
 (())
 > (if-let [a '()] (list a))
 (())
 > (if-let [a false] (list a))
 clojure#nil
 > (with-exception-catcher
    source-error-message
    (& (eval '(if-let [a false] (list a) 'n 'z))))
 ;; Syntax error macroexpanding clojure.core/if-let at (form-init5831580352679050351.clj:1:1).
 ;; ((quote z)) - failed: Extra input
 "too many arguments"
 > (with-exception-catcher
    source-error-message
    (& (eval (quote-source (let [nil 1] (if-let [a false] (list a)))))))
 ;; Syntax error macroexpanding clojure.core/let at (form-init5831580352679050351.clj:1:1).
 ;; nil - failed: simple-symbol? at: [:bindings :form :local-symbol] spec: :clojure.core.specs.alpha/local-name
 ;; nil - failed: vector? at: [:bindings :form :seq-destructure] spec: :clojure.core.specs.alpha/seq-binding-form
 ;; nil - failed: map? at: [:bindings :form :map-destructure] spec: :clojure.core.specs.alpha/map-bindings
 ;; nil - failed: map? at: [:bindings :form :map-destructure] spec: :clojure.core.specs.alpha/map-special-binding
 clojure#nil
 ;; binds nil but it's a different variable than the nil that is
 ;; mapped to clojure#nil.

 ;; BTW: this is interesting: Gambit's namespacing does more than
 ;; first thought? Or, let really binds the given symbol doesn't
 ;; namespace-prefix it?
 > (let [nil 1] nil)
 1
 > (let [nil 1] clojure#nil)
 clojure#nil
 > (let [clojure#nil 1] clojure#nil)
 1
 > (let [clojure#nil 1] nil)
 clojure#nil)



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


