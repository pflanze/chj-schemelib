;;; Copyright 2014-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; short identifiers

(require define-macro-star
         fixnum
         fixnum-more
         cj-env ;; identity ?, define-if-not-defined
         cj-functional ;; compose etc.
         cj-struct
         cj-typed ;; incl. args-vars
         dot-oo ;; incl. define.
         oo-util ;; for .string generic and some basic methods
         values
         ;; values-util ?
         define-module
         cj-match
         simple-match ;; provided by cj-match ?
         (cj-source-wraps source:symbol-append)
         (enum define-enum)
         (cj-source-quasiquote quasiquote-source)
         string-interpolate
         test
         (test-random %try-syntax-error)
         (string-util-2 string-any)
         list-util ;; e.g. let-pair
         list-util-3 ;; e.g. if-let-pair
         list-util-lazy)

(export (macro if)
        (macro scheme:if)
        (macro defstruct)
        (macro def)
        (macro defvar)
        (macro defvar-once)
        (macro defmacro)
        (macro &)
        (macro def&)
        (macro forward-def)
        (macro def.)
        (macro def-inline)
        (macro defenum)
        (macro def-values)
        (macro def*)
        (macro defparameter)
        (macro def-once)
        comp-function
        (macro comp*)
        (macro comp)
        (macro comp//)
        maybe-comp
        id
        (macro defmodule)
        (macro the)
        (macro modimport)
        (macro modimport/prefix)
        (macro lambda)
        (macro let)
        (macro letrec)
        (macro $)
        desourcify
        
        #!optional
        module-symbol?
        def-expand
        easy-if-expand)


;; Paul Graham's idea for nested paren-less |if| (Arc)

(both-times
 (define (easy-if-expand stx t b1 b2 args t-map)
   (let rec ((t t)
             (b1 b1)
             (b2 b2)
             (args args))
     `(##if ,(t-map t)
            ,b1
            ,(if (null? args)
                 b2
                 (let-pair ((b1* args) args)
                           (if-let-pair ((b2* args) args)
                                        (rec b2 ;; becomes test
                                             b1*
                                             b2*
                                             args)
                                        (raise-source-error
                                         stx
                                         "if requires an uneven number of arguments (and at least 3)"))))))))

(define-macro* (if t b1 b2 . args)
  (easy-if-expand stx t b1 b2 args identity))

(TEST
 > (if #t 'yes 'no)
 yes
 > (if #f 'yes 'no)
 no
 > (if #f 'yes #t 'yes2 'no2)
 yes2
 > (if #f 'yes #f 'yes2 'no2)
 no2
 > (if #t 'yes #f 'yes2 'no2)
 yes
 > (with-exception-catcher source-error-message
                           (& (eval '(if #t 'yes #f 'yes2))))
 "if requires an uneven number of arguments (and at least 3)")


;; To compensate for the inability to use |if| for selective side
;; effects, use |when| from cj-env.scm, or this:

(define-macro* (scheme:if t then #!optional else)
  `(##if ,t ,then ,@(if else (list else) '())))



(define-macro* (defstruct . args)
  `(define-struct. ,@args))


(both-times
 (define (def-expand stx first body0 bodyrest begin-form)
   (define (err msg)
     (raise-source-error stx msg))
   (if (pair? (source-code first))
       (define-typed-expand stx first (cons body0 bodyrest) begin-form)
       (if (null? bodyrest)
           `(##define ,first ,body0 ,@bodyrest)
           (let-pair
            ((body1 bodyrest) bodyrest)
            (if (and (string? (source-code body0))
                     (null? bodyrest))
                ;; docstring
                (let* ((code (macro-star-expand body1))
                       (anyway (lambda ()
                                 ;; Simply drop the docstring, for now; it's still
                                 ;; useful for documenting already!
                                 `(##define ,first ,code))))
                  (if-let-pair
                   ((c0 crest) (source-code code))
                   (case (source-code c0)
                     ;; |lambda| without the ## should never happen since
                     ;; it's a macro expanding to the latter.
                     ((##lambda)
                      (if-let-pair ((binds body) crest)
                                   `(##define ,first
                                              (##lambda ,binds ,body0 ,@body))
                                   (raise-source-error
                                    code "got lambda without bind form")))
                     (else
                      ;; (err "|def| using docstring but body is not expanding to a lambda, thus can't move it there")
                      (anyway)))
                   ;; (err "|def| using docstring but body is not a form, thus can't move it there")
                   (anyway)))
                (err "|def|, if given a bare variable, requires only one expression, or two if the first is a docstring")))))))


(define-macro* (def first body0 . bodyrest)
  (def-expand stx first body0 bodyrest '##begin))

(TEST
 
 ;; curried definitions:
 > (expansion#def ((f x) y) (list x y))
 ;; (define-typed ((f x) y) (list x y))
 ;; (define-typed (f x) (typed-lambda (y) (list x y)))
 (define-typed (f x) (##lambda (y) (##begin (list x y))))
 > (expansion#define-typed ((f x) y) (list x y))
 ;; (define-typed (f x) (typed-lambda (y) (list x y)))
 (define-typed (f x) (##lambda (y) (##begin (list x y))))
 > (expansion#define-typed (f x) (typed-lambda (y) (list x y)))
 ;; (define f (typed-lambda (x) (typed-lambda (y) (list x y))))
 (define f (##lambda (x) (##begin (typed-lambda (y) (list x y)))))
 
 ;; docstrings:
 > (expansion#def (x a) 10)
 ;; (define-typed (x a) 10)
 ;; (define x (typed-lambda (a) 10))
 (define x (##lambda (a) (##begin 10)))
 > (expansion#def (x a) "foo" 10)
 ;; (define-typed (x a) "foo" 10)
 ;; (define x (typed-lambda (a) "foo" 10))
 (define x (##lambda (a) (##begin "foo" 10)))
 > (def x "foo" 10)
 > x
 10
 > (expansion#def x "foo")
 ;; (define x "foo")
 (##define x "foo")
 > (expansion#def x "foo" (lambda (n) n))
 ;; (define x (##lambda (n) "foo" (##begin n)))
 (##define x (##lambda (n) "foo" (##begin n))))


;; make sure there's a set! for the variable in the module scope so
;; that block mode won't make it appear immutable for code in the
;; module.
(define-macro* (defvar name expr)
  (assert* symbol? name)
  `(begin
     (define ,name #f)
     (set! ,name ,expr)))

(define-macro* (defvar-once name expr)
  (assert* symbol? name)
  `(begin
     (define-if-not-defined ,name ,expr)
     (set! ,name ,name)))

(define-macro* (defmacro . args)
  `(define-macro* ,@args))

(define-macro* (& . args)
  ;; `(thunk ,@args)
  `(lambda () ,@args))

(define-macro* (def& bind . type+body)
  (mcase bind
         (`(`name `thunkvar)
          ;; ^ XX could allow optional and keyword args though
          (let ((macrocode
                 `(quasiquote (,name (& ))))
                (name* (source:symbol-append name '&)))
            (with-gensym
             E
             `(begin
                (def ,bind ,@type+body)
                (defmacro (,name* . ,E)
                  `(,',name (##lambda () ,@,E)))))))))

(TEST
 > (def& (lol x) (x))
 > (lol (& 'ha))
 ha
 > (lol& 'ha)
 ha
 ;; for now:
 > (%try-syntax-error (def& (lol x y) (x)))
 [source-error "no match, expecting: `(`name `thunkvar)"])


;; forward declaration; no body, but might accept ->
(define-macro* (forward-def . args)
  ;; in the context of a host that doesn't require forward
  ;; declarations:
  `(begin))

(define-macro* (def. . args)
  `(define. ,@args))

;; adapted from cj-inline.scm to accept typing
(define-macro* (def-inline name+args body0 . body)
  (match-list*
   name+args
   ((name . args)
    (let* ((lambdacode
            `(typed-lambda ,args
                           ,body0 ,@body))
           (templatecode
            `(,lambdacode
              ,@(map (lambda (arg)
                       (list 'unquote (perhaps-typed.var arg)))
                     args))))
      (quasiquote-source
       (begin
         (define-macro* (,(source:symbol-append name '-lambda))
           ,(list 'quasiquote-source lambdacode))
         (define-macro* (,name ,@(map perhaps-typed.var args))
           (source-code
            ,(list 'quasiquote-source templatecode)))))))))

(define-macro* (defenum name . args)
  `(define-enum ,name ,@args))

(define-macro* (def-values . args)
  `(define-values ,@args))

;; Rather than implement def-list and def-pair, go "all the way"?...:
(defmacro (def* binds expr)
  "Define given bindings with the list from expr bound to them, the
same way (once implemented) that lambda variables are bound to the argument
list."
  (let ((binds* (source-code binds)))
    (if (pair? binds*)
        ;; Some special cases for efficiency (and better error
        ;; handling?):
        (let-pair
         ((a b) binds*)
         (let* ((b* (source-code b)))
           (cond
            ((and (null? b*)
                  ;; no typing required?:
                  (symbol? (source-code a)))
             `(def ,a (xone (force ,expr))))
            ((and (symbol? b*)
                  (symbol? (source-code a)))
             (with-gensyms
              (A B)
              `(begin
                 (define ,a)
                 (define ,b)
                 (let-pair ((,A ,B) (force ,expr))
                           (set! ,a ,A)
                           (set! ,b ,B)))))
            (else
             ;; including typed cases of the above
             (let ((vars (args-vars binds)))
               (with-gensym
                VS
                `(begin
                   ,@(map (lambda (var)
                            `(define ,var))
                          vars)
                   (safer-apply ',(schemedefinition-arity:pattern->template
                                   binds*)
                                (lambda ,binds
                                  (##vector ,@vars))
                                ,expr
                                error
                                ;; such a hack. Should really just
                                ;; implement the full args parser,
                                ;; finally.
                                (lambda (,VS)
                                  ,@(map/iota
                                     (lambda (var i)
                                       `(##set! ,var (vector-ref ,VS ,i)))
                                     vars))))))))))
        (raise-source-error
         binds
         "expecting binding list (optionally improper / with DSSSL meta objects)"))))

(TEST
 > (def* (a) (list 10))
 > a
 10
 > (def* (a . b) (list 10 20))
 > (list a b)
 (10 (20))
 > (def* (a b) (list 10 20))
 > (list a b)
 (10 20)
 > (def* ([integer? a] b #!optional c) (list 10 20 30))
 > (list a b c)
 (10 20 30)
 > (%try (def* ([integer? a] b #!optional c) (list "10" 20 30)))
 (exception text: "a does not match integer?: \"10\"\n")
 > (%try (def* ([integer? a] b #!optional c) (list 10 20 30 40)))
 (exception text: "too many arguments\n"))


(define-macro* (defparameter . args)
  `(define-parameter ,@args))

(define-macro* (def-once . args)
  `(define-if-not-defined ,@args))

(def comp-function compose-function)
(defmacro (comp* . args)
  `(compose* ,@args))

(defmacro (comp . es)
  `(compose ,@es))

(defmacro (comp// n . es)
  `(compose// ,n ,@es))

(def maybe-comp maybe-compose)

(def id identity)

(defmacro (defmodule . args)
  `(define-module ,@args))


;; if given one optional argument, it is evaluated in either the
;; not-found and too-many cases, if two optional arguments are given,
;; the first is evaluated in the not-found case the second in the
;; too-many case.
(defmacro (the expr #!optional error-expr too-many-error-expr)
  (if error-expr
      (with-gensym
       V
       `(let* ((,V ,expr)
               (it (force ,V)))
          ,(if too-many-error-expr
               `(cond ((one-item? it) (car it))
                      ((null? it) ,error-expr)
                      (else
                       ,too-many-error-expr))
               `(if (one-item? it)
                    (car it)
                    ,error-expr))))
      `(xone ,expr)))

(TEST
 > (%try-error (the '()))
 [error "expected one item, but:" not-found ()]
 > (%try-error (the '(1)))
 1
 > (%try-error (the '(1 2)))
 [error "expected one item, but:" found-too-many (1 2)]

 > (the '() 'nope)
 nope
 > (the '(1) 'nope)
 1
 > (the '(1 2) 'nope)
 nope

 > (the '() 'nothing 'many)
 nothing
 > (the '(1) 'nothing 'many)
 1
 > (the '(1 2) 'nothing 'many)
 many)



;; --  extension of define-module.scm ---

;; A new import form, that accepts expressions, but also prefixing iff
;; the expression is using a symbol that starts with a #\< as head,
;; and also (a new feature) allows to load pre-evaluated 'modules'
;; (module expressions?) (i.e. what a module exports when called)
;; stored in a symbol that is already available at macro expansion
;; time. (Hm. Good or bad? Could fail if that symbol was defined in
;; another place but re-defined in the current module; but that's evil
;; anyway, and future module system of mine (?) will/should do that
;; better anyway, ookay?)

(def (module-symbol? v)
     (and (symbol? v)
          (let* ((s (symbol->string v))
                 (len (string-length s)))
            (and (>= len 3)
                 (char=? (string-ref s 0) #\<)
                 (char=? (string-ref s (dec len)) #\>)
                 (string-any (lambda (v)
                               (not (or (char=? v #\<)
                                        (char=? v #\>))))
                             s)))))


(TEST
 > (map module-symbol? '(< <= <> <=> <foo>module <foo>))
 (#f #f #f #t #f #t))


;; check for exporter (runtime value)
(def (modimport-expand:exporterholder modsymbol export)
     (*if-symbol-value
      modsymbol
      (lambda (exporter)
        (if (procedure? exporter)
            (export (exporter #f))
            (raise-source-error
             modsymbol
             (string-append
              "modimport: expecting an exporter procedure"
              " in module-holding symbol"))))
      (lambda ()
        (raise-source-error
         modsymbol
         (string-append
          "modimport: module-holding symbol does not"
          " contain a value (yet?)")))))

;; check for -exports symbol (compiletime value)
(def (modimport-expand:mod maybe-prefix mod expr *else)
     (*if-symbol-value

      (source:symbol-append mod '-exports)

      (lambda (exports)
        (if maybe-prefix
            `(module:import/prefix ,expr ,maybe-prefix ,@exports)
            `(module:import ,expr ,@exports)))

      *else))


(def (modimport-expand maybe-prefix expr vars)
     (if (pair? vars)
         (if maybe-prefix
             `(module:import/prefix ,expr ,maybe-prefix ,@vars)
             `(module:import ,expr ,@vars))

         ;; No vars given, need to find the exports list from the
         ;; module name with -exports appended, or in case of expr
         ;; being a bare symbol, checking if it holds a value at
         ;; expansion time already.
         (mcase
          expr

          (symbol?
           ;; Always assume that it is the result of a module
           ;; parametrization (exporter); checking for mod-exports is
           ;; pointless in this case, and dangerous (conflicts with
           ;; other actual modules (previously defined ones, although
           ;; admittedly that's a problem of the current (non-)module
           ;; system)).
           (modimport-expand:exporterholder
            expr
            (lambda (exports)
              (if maybe-prefix
                  `(module:import/prefix ,expr ,maybe-prefix ,@exports)
                  `(module:import ,expr ,@exports)))))

          (`(`mod . `args)
           (if (module-symbol? (source-code mod))

               (modimport-expand:mod
                maybe-prefix
                mod
                expr
                (&
                 (raise-source-error
                  mod
                  (string-append
                   "modimport: can't find -exports entry"
                   " for this module symbol"))))

               (raise-source-error
                mod
                ;; XX this was "if no prefix is given", hmm, which is
                ;; true really?
                (string-append
                 "modimport requires syntax matching"
                 " `module-symbol?` here if no import list is given")))))))

(defmacro (modimport expr . vars)
  (if (keyword? (source-code expr))
      (raise-source-error (second (source-code stx))
                          ;; expr has been stripped of source information,
                          ;; gah, for macro expander keyword functionality
                          (string-append
                           "modimport: can't take keyword as first argument "
                           "(did you mean to use |modimport/prefix|?)"))
      (modimport-expand #f expr vars)))

(defmacro (modimport/prefix prefix expr . vars)
  (modimport-expand prefix expr vars))



;; Offer nesting for lambda, too? Or offer lambda* and def* for curried
;; definitions from non-nested variables lists? Explicit nesting has
;; the benefit of control. Thus offer it too on lambda, but actually also
;; do it in cj-typed.scm

(defmacro (lambda bs expr . rest)
  ;;(quasiquote-source (typed-lambda ,bs ,expr ,@rest))
  ;; Nope, more often I do want to see where it is being used! Should
  ;; be aware now that lambda is redefined, ok?
  (quasiquote (typed-lambda ,bs ,expr ,@rest)))


(TEST
 > (expansion#modimport tj8znc94e7fkdsqfm a b c)
 (module:import tj8znc94e7fkdsqfm a b c)
 ;; > (expansion#modimport tj8znc94e7fkdsqfm)
 > (with-exception-catcher source-error-message
                           (& (modimport-expand #f 'tj8znc94e7fkdsqfm '())))
 "modimport: module-holding symbol does not contain a value (yet?)"
 > (with-exception-catcher source-error-message
                           (& (modimport-expand #f '<tj8znc94e7fkdsqfm> '())))
 "modimport: module-holding symbol does not contain a value (yet?)"
 > (with-exception-catcher source-error-message
                           (& (modimport-expand #f '<tj8znc94e7fkdsqfm> '(a))))
 (module:import <tj8znc94e7fkdsqfm> a)
 > (with-exception-catcher source-error-message
                           (& (modimport-expand foo: '<tj8znc94e7fkdsqfm> '(a))))
 (module:import/prefix <tj8znc94e7fkdsqfm> foo: a)
 ;; a shadow definition, defines foo-exports :
 > (defmodule (foo x) (export bad guys) (def bad 1) (def guys 2))
 ;; the definition we want to use:
 > (defmodule (<foo> a b) (export bar baz) (def bar a) (def baz b))

 ;; <foo> is *not* an exporter, it's the module itself, which has to
 ;; be parametrized (XX although, is it really the case that we never
 ;; want to use defmodule for unparametrized modules?), thus this is
 ;; an error:
 > (with-exception-catcher wrong-number-of-arguments-exception?
                           (& (modimport-expand #f '<foo> '())))
 #t
 ;; correct:
 > (modimport-expand #f '(<foo> Y) '())
 (module:import (<foo> Y) bar baz)
 > (modimport-expand foo: '(<foo> Y) '())
 (module:import/prefix (<foo> Y) foo: bar baz)
 ;; defining evaluated module: (software, please do *not* take |foo-exports| !)
 > (def foo (<foo> 10 11))
 > (modimport-expand foo: '(<foo> 13 14) '())
 (module:import/prefix (<foo> 13 14) foo: bar baz)
 > (with-exception-catcher source-error-message
                           (& (modimport-expand foo: '(foo 13 14) '())))
 "modimport requires syntax matching `module-symbol?` here if no import list is given"
 > (modimport-expand foo: 'foo '())
 (module:import/prefix foo foo: bar baz))


(def (easy#let..-expand stx binds body LET maybe-NAMED-LET)
     (def (lonely-binds? binds*)
          (and (pair? binds*)
               (symbol? (source-code (car binds*)))))
     (let ((binds* (source-code binds)))
       (cond ((lonely-binds? binds*)
              ;; single-binding let: this means that we can't bind multiple
              ;; values though. (But that's what letv is for?)
              `(,LET (,binds) ,@body))
             ((symbol? binds*)
              ;; named let
              (cond (maybe-NAMED-LET
                     =>
                     (lambda (NAMED-LET)
                       (if-let-pair ((bindsagain body*) body)
                                    (if (lonely-binds? (source-code bindsagain))
                                        `(,NAMED-LET ,binds
                                                     (,bindsagain)
                                                     ,@body*)
                                        `(,NAMED-LET ,binds
                                                     ,bindsagain
                                                     ,@body*))
                                    (raise-source-error
                                     stx "missing bindings for named let"))))
                    (else
                     (raise-source-error stx "letrec does not support naming"))))
             (else
              `(,LET ,binds ,@body)))))

(defmacro (easy#let binds . body)
  (easy#let..-expand stx binds body '##let '##let))

(defmacro (easy#letrec binds . body)
  (easy#let..-expand stx binds body '##letrec #f))

(defmacro (let . rest)
  `(easy#let ,@rest))

(defmacro (letrec . rest)
  `(easy#letrec ,@rest))


(TEST
 > (let (a 1) a)
 1
 > (let ((a 2)) a)
 2
 > (let ((a 2) (b 3)) b)
 3
 > (expansion#easy#let foo ((a 1)) foo)
 (##let foo ((a 1)) foo)
 > (expansion#easy#let foo (a 1) foo)
 (##let foo ((a 1)) foo))


(def $-string
     (##lambda (v)
               (##if (##let ()
                            (##declare (block)
                                       (standard-bindings)
                                       (extended-bindings)
                                       (not safe) (fixnum))
                            (##namespace (""))
                            (string? v))
                     v
                     (.string v))))

(defmacro ($ . exprs)
  `(string-interpolate (##let () ;; oh well
                              (##namespace (""))
                              $-string)
                       ,@exprs))

(TEST
 > (define bar-world 11)
 > ($ "foo" " $bar-world, you" 12 (inc 13))
 "foo 11, you1214"
 > (let ((foo#bar-world 100))
     (##namespace ("foo#"))
     (##namespace ("" $ string-interpolate string-append .string inc fx+))
     ($ "foo" " $bar-world, you" 12 (inc 13)))
 "foo 100, you1214")


(def desourcify cj-desourcify)

