;;; Copyright 2010-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         (cj-functional-2 =>*-expand/placement))

(export (macros use-monad
                in-monad
                mdo
                mdo-in
                mlet
                mlet-in
                mlift
                ==>
                ==>*
                ==>>
                ==>>*))

;;;
;;;; Infrastructure to write monads
;;;

;; >>, >>=, return are defined by |use-monad| as macros, which proxy
;; to its own name prefixed with the current monad name, which is
;; given lexically either explicitly to the monadic syntactical forms
;; (in-monad, mdo-in, mlet-in.. which all use |use-monad| in some
;; way), or set by |->| from cj-typed.scm, and a "-".

;; The reason those are macros are so that they can dispatch to
;; implementations which might also be macros (at least Gambit doesn't
;; support name-aliasing macros via let), which is needed for some
;; monads for >> to be lazy in the second argument. Also, this allows
;; for inlining (arguably the compiler should support cross-module
;; inlining instead, but that's currently not the case), and may allow
;; elimination of closures/objects in the resulting monad execution
;; altogether (back to natural execution flow hence natural speed and
;; debugging experience).

;; Also defined are >>-function, >>=-function, return-function as
;; 0-ary macros which expand to the reference of the same-named (sans
;; -function) op with monad name and "." prefixed, with the idea to
;; give an actual function value for higher-order use (is this ever
;; useful?).

;; Generic dispatch can be achieved by resolving the monad operators
;; to their name with just "." prepended (dot-oo generics), although
;; for speed it is better to instead retrieve all monad operator
;; function values once by calling .monad-ops on the given monadic
;; value, which should return a monad-ops object holding functions for
;; the monad ops (see monad/monad-ops.scm). monad/generic.scm defines
;; |use-monad-for| and |in-monad-for| along with |mlet-for| etc. which
;; alias >>, >>=, return from there, as well as >>-function,
;; >>=-function, return-function (in this case all directly as
;; functions). Alternatively, retrieve the monad-ops struct via
;; `($monad-ops)` which is defined by |in-monad| / |use-monad|.


(defmacro (use-monad monadname)
  (assert* symbol? monadname
           (lambda (monadname*)
             `(begin

                ;; ops aliases
                
                (##define-syntax
                 >>
                 (lambda (stx)
                   (##sourcify-deep
                    (apply
                     (lambda (_name . args)
                       (cons ',(symbol-append monadname* "->>") args))
                     (##source-code stx))
                    stx)))

                (##define-syntax
                 >>=
                 (lambda (stx)
                   (##sourcify-deep
                    (apply
                     (lambda (_name . args)
                       (cons ',(symbol-append monadname* "->>=") args))
                     (##source-code stx))
                    stx)))

                (##define-syntax
                 =<<
                 (lambda (stx)
                   (##sourcify-deep
                    (apply
                     (lambda (_name a b)
                       (list ',(symbol-append monadname* "->>=") b a))
                     (##source-code stx))
                    stx)))

                (##define-syntax
                 return
                 (lambda (stx)
                   (##sourcify-deep
                    (apply
                     (lambda (_name . args)
                       (cons ',(symbol-append monadname* "-return") args))
                     (##source-code stx))
                    stx)))

                (##define-syntax
                 unwrap
                 (lambda (stx)
                   (##sourcify-deep
                    (apply
                     (lambda (_name . args)
                       (cons ',(symbol-append monadname* "-unwrap") args))
                     (##source-code stx))
                    stx)))


                ;; ops-function accessors
                
                (##define-syntax
                 >>-function
                 (lambda (stx)
                   (##sourcify-deep
                    (apply
                     (lambda (_name)
                       ',(symbol-append monadname* ".>>"))
                     (##source-code stx))
                    stx)))

                (##define-syntax
                 >>=-function
                 (lambda (stx)
                   (##sourcify-deep
                    (apply
                     (lambda (_name)
                       ',(symbol-append monadname* ".>>="))
                     (##source-code stx))
                    stx)))

                (##define-syntax
                 return-function
                 (lambda (stx)
                   (##sourcify-deep
                    (apply
                     (lambda (_name)
                       ',(symbol-append monadname* ".return"))
                     (##source-code stx))
                    stx)))

                (##define-syntax
                 unwrap-function
                 (lambda (stx)
                   (##sourcify-deep
                    (apply
                     (lambda (_name)
                       ',(symbol-append monadname* ".unwrap"))
                     (##source-code stx))
                    stx)))

                
                ;; To call generic monad functions like
                ;; monad-ops.sequence :

                (##define-syntax
                 $monad-ops
                 (lambda (stx)
                   (##sourcify-deep
                    (apply
                     (lambda (_name)
                       ',(symbol-append monadname* ":monad-ops"))
                     (##source-code stx))
                    stx)))

                ;; And nicer variant? Unfinished. Not sure how
                ;; exactly. Or if at all.
                ;; (##define-syntax
                ;;  monadic
                ;;  (lambda (stx)
                ;;    (##sourcify-deep
                ;;     (apply
                ;;      (lambda (_name . fn+args)
                ;;        (if-let-pair
                ;;         ((fn args) fn+args)
                ;;         `(,(symbol-append .. fn) ;; prefix with
                ;;                                  ;; monad-ops. *if*
                ;;                                  ;; symbol?
                ;;           ,(symbol-append monadname* ":monad-ops")
                ;;           ,@args)
                ;;         (raise-source-error
                ;;          stx "need fn and its arguments as arguments")))
                ;;      (##source-code stx))
                ;;     stx)))

                
                (##define-syntax
                 minline! ;; with m: prefix?
                 (lambda (stx)
                   (##sourcify-deep
                    (apply
                     (lambda (_name . args)
                       (cons ',(symbol-append monadname* ".inline!") args))
                     (##source-code stx))
                    stx)))))))

(defmacro (in-monad monadname . body)
  (assert* symbol? monadname
           (lambda (monadname*)
             `(let ()
                (use-monad ,monadname)
                (let ()
                  ;; ^ necessary so that "Scheme" doesn't complain
                  ;; about further defines after the ##define-syntax
                  ;; from use-monad?
                  ,@body)))))


;; General (except the -in variants), but depending on the above in
;; their scope or using the slower generics:

(def (mdo-expand stx exprs)
     (right-associate (lambda (e r)
                        (sourcify
                         `(>> ,e ,r)
                         e))
                      exprs
                      (lambda (errmsg)
                        (raise-source-error stx errmsg))))

(defmacro (mdo . exprs)
  (mdo-expand stx exprs))

(defmacro (mdo-in monadname . exprs)
  `(in-monad ,monadname
             ,(mdo-expand stx exprs)))

(TEST
 > (expansion#mdo a b c)
 (>> a (>> b c)))



(def (mlet-expand stx bindS expr)
     (def (expand1 bind expr)
          (mcase bind
                 (`(`var1 `expr1)
                  (possibly-sourcify
                   `(>>= ,expr1
                         (lambda (,var1) ,expr))
                   expr1))))
     (assert* pair? bindS
              (lambda (bindS*)
                (if (pair? (source-code (car bindS*)))
                    ;; normal multiple let
                    (fold-right expand1
                                expr
                                bindS*)
                    ;; single "beautiful" let
                    (expand1 bindS expr)))))

(defmacro (mlet bindS expr)
  (mlet-expand stx bindS expr))

(defmacro (mlet-in monadname bindS expr)
  `(in-monad ,monadname
             ,(mlet-expand stx bindS expr)))

(TEST
 > (expansion#mlet ((x 2) (y 3)) x)
 (>>= 2 (lambda (x) (>>= 3 (lambda (y) x))))
 > (expansion#mlet ((x 2)) x)
 (>>= 2 (lambda (x) x))
 > (expansion#mlet (x 2) x)
 (>>= 2 (lambda (x) x)))



;; mlift: can't use a function because the >>= >> or mlet needs to be
;; taken from the context syntactically.

(defmacro (mlift f . args)
  (let ((vs (map (lambda_ (gensym)) args))) ;; XX do I have sth or not for this?
    `(mlet ,(map list vs args)
           (return ,f ,@vs))))

;; XXX: mlift or lift? Is this not one of the methods like return ?



;; Threading macros like =>, =>*, =>>, =>>*, but monadic (via >>=):

;; (Partially copy-paste from cj-functional-2: changed in that the
;; first expression to ==> ==>> is also passed through >>=, not just
;; fed directly into the next form. That part is pretty hacky: wrap an
;; extra * variant of the syntax.)

(def inner-==>-expand (=>*-expand/placement (lambda (prev-result rest)
                                              `(,prev-result ,@rest))
                                            'mlet))
(def inner-==>>-expand (=>*-expand/placement (lambda (prev-result rest)
                                               `(,@rest ,prev-result))
                                             'mlet))

(def (==>-expand start exprs)
     (if (null? exprs)
         start
         (with-gensym
          V
          `(mlet ((,V ,start))
                 ,(inner-==>-expand V exprs)))))

(define-macro* (==> start . exprs)
  (==>-expand start exprs))

(TEST
 > (define TEST:equal? syntax-equal?)
 > (expansion#==> (foo) (bar 1) (baz 2))
 (mlet ((GEN:V-2343 (foo)))
       (mlet ((GEN:tmp-2344 (bar GEN:V-2343 1)))
             (baz GEN:tmp-2344 2)))
 > (expansion#==> (foo) (bar 1))
 (mlet ((GEN:V-2343 (foo)))
       (bar GEN:V-2343 1))
 > (expansion#==> (foo))
 (foo)
 > (expansion#==> foo)
 foo)


(define-macro* (==>*-nary expr0 . exprs)
  (if (symbol? (source-code expr0))
      (with-gensyms
       (VS WS)
       `(##lambda ,VS
                  (mlet ((,WS (monad-ops.sequence ($monad-ops) ,VS)))
                        ,(==>-expand (possibly-sourcify
                                      `(##apply ,expr0 ,WS)
                                      expr0)
                                     exprs))))
      (raise-source-error expr0
                          "need a symbol here to support n-arity")))

;; But the normal case will really be the 1-ary one:
(define-macro* (==>* expr0 . exprs)
  (with-gensym
   V
   `(##lambda (,V)
              ,(==>-expand V (cons expr0 exprs)))))


(TEST
 > (define TEST:equal? syntax-equal?)
 > (expansion#==>* (foo 0) (bar 1) (baz 2))
 (##lambda (GEN:V-1)
           (mlet ((GEN:V-2 GEN:V-1))
                 (mlet ((GEN:V-2343 (foo GEN:V-2 0)))
                       (mlet ((GEN:tmp-2344 (bar GEN:V-2343 1)))
                             (baz GEN:tmp-2344 2)))))
 > (expansion#==>* (foo) (bar 1))
 (##lambda (GEN:V-1)
           (mlet ((GEN:V-2 GEN:V-1))
                 (mlet ((GEN:V-2343 (foo GEN:V-2)))
                       (bar GEN:V-2343 1))))
 > (expansion#==>*-nary foo (bar 1))
 (##lambda GEN:VS-2539
           (mlet ((GEN:WS-2540 (monad-ops.sequence ($monad-ops)
                                                  GEN:VS-2539)))
                 (mlet ((GEN:V-2541 (##apply foo GEN:WS-2540)))
                       (bar GEN:V-2541 1))))
 > (expansion#==>* foo (bar 1))
 (##lambda (GEN:V-2539)
           (mlet ((GEN:V-2540 GEN:V-2539))
                 (mlet ((GEN:tmp-2541 (foo GEN:V-2540)))
                       (bar GEN:tmp-2541 1))))
 > (expansion#==> (foo 0))
 (foo 0)
 > (expansion#==> foo)
 foo)



;; (define-macro* (==>*/arity n expr0 . exprs)
;;   (let ((n* (eval n)))
;;     (if (exact-natural0? n*)
;; 	(let ((VS (map (lambda (i) (gensym))
;; 		       (iota n*))))
;; 	  `(##lambda ,VS
;; 		     ,(==>-expand (possibly-sourcify `(,expr0 ,@VS) expr0)
;;                                   exprs)))
;; 	(raise-source-error n "expecting expression evaluating to natural0"))))


;; (define ==>>-expand (=>*-expand/placement
;;                      (lambda (prev-result rest)
;;                        `(,@rest ,prev-result))
;;                      'mlet))

;; (define-macro* (==>> start . exprs)
;;   (==>>-expand start exprs))

;; (define-macro* (==>>* expr0 . exprs)
;;   (with-gensym
;;    V
;;    (if (symbol? (source-code expr0))
;;        `(##lambda ,V
;; 		  ,(==>>-expand (possibly-sourcify `(##apply ,expr0 ,V) expr0)
;;                                 exprs))
;;        ;; otherwise can't support multiple values:
;;        `(##lambda (,V)
;; 		  ,(==>>-expand V (cons expr0 exprs))))))

