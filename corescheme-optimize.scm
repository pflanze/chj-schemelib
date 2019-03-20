;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         corescheme
         test
         corescheme-to-scheme)


(export corescheme-optimize ;; avoid using .optimize directly
        )


;; helpers, move to corescheme-helpers.scm?

(def (possibly-vars-equal? vars1 vars2 fn1 fn2)
     "vars1 and vars2 are either lists from app, or from lambda; i.e. from
the latter case, they can contain DSSSL, be improper; in the former
case, be expressions. Only return true if both lists are really the
same and the result of passing the item through fn* only gives
variables, and they are proper lists (i.e. n-ary case is excluded.)"
     (let lp ((vars1 vars1)
              (vars2 vars2))
       (if-let-pair ((a vars1*) vars1)
                    (if-let-pair ((b vars2*) vars2)
                                 (let ((a* (fn1 a))
                                       (b* (fn2 b)))
                                   (and (corescheme-var? a*)
                                        (corescheme-var? b*)
                                        (corescheme-var.equal? a* b*)
                                        (lp vars1* vars2*)))
                                 #f)
                    (and (null? vars1)
                         (null? vars2)))))

(TEST
 > (def (t l1 l2)
        (possibly-vars-equal? l1 l2 id id))
 > (t '() '())
 #t
 > (t '() '(a))
 #f
 > (t '(a) '(a))
 #f
 > (t '(a) '())
 #f
 > (t (list (corescheme-var 'a 1))
      (list (corescheme-var 'a 1)))
 #t
 > (t (corescheme-var 'a 1)
      (corescheme-var 'a 1))
 #f
 > (t (list (corescheme-var 'a 1))
      (corescheme-var 'a 1))
 #f
 > (t (corescheme-var 'a 1)
      (list (corescheme-var 'a 1)))
 #f
 > (t (list (corescheme-var 'a 1))
      (list (corescheme-var 'a 1) (corescheme-var 'a 1)))
 #f
 > (t (list (corescheme-var 'a 1))
      (list (corescheme-var 'a 2)))
 #f
 > (%try (t (list (corescheme-var 'a 1))
            (list (corescheme-var 'b 1))))
 (exception text: "assertment failure: (eq? name (corescheme-var.name b)) (eq? 'a (corescheme-var.name '[(corescheme-var) b 1]))\n"))

(def (arg->maybe-var arg)
     (and (corescheme-ref? arg)
          (corescheme-ref.var arg)))


;; /helpers


(defclass (corescheme-optimize-parameters
           [boolean? lambda-unwrap?]
           [boolean? apply?])
  /keywords?: #t)


(def corescheme-optimize-parameters-off
     (apply corescheme-optimize-parameters (make-list 2 #f)))
(def corescheme-optimize-parameters-on
     (apply corescheme-optimize-parameters (make-list 2 #t)))

(defparameter current-corescheme-optimize-parameters
  corescheme-optimize-parameters-on)

;; (def (corescheme-optimize-parameter accessor)
;;      (accessor (corescheme-optimize-parameters)))
(defmacro (CORESCHEME-OPTIMIZE-PARAMETER name)
  (assert* symbol? name
           (lambda (name)
             `(,(symbol-append 'corescheme-optimize-parameters. name)
               (current-corescheme-optimize-parameters)))))


(def (corescheme-optimize s
                          #!key
                          [(maybe corescheme-optimize-parameters?) parameters])
     (let ((r (parameterize ((current-optimizing? #t)
                             (current-corescheme-optimize-parameters
                              (or parameters
                                  (current-corescheme-optimize-parameters))))
                            (.optimize s))))
       (assert (corescheme.optimized? r))
       r))


(def.* (corescheme-literal.optimize s)
  (corescheme-literal.optimized?-set s #t))

(def.* (corescheme-ref.optimize  s)
  (corescheme-ref.optimized?-set s #t))

(def.* (corescheme-lambda.optimize s)
  (let* ((expr* (.optimize expr))
         (return-s* (lambda ()
                      (corescheme-lambda vars expr*))))
    (if (and (CORESCHEME-OPTIMIZE-PARAMETER lambda-unwrap?)
             (corescheme-app? expr*))
        (with. corescheme-app expr*
               (cond
                ;; remove needless lambda wrappers: (lambda (x y) (f x y))
                ((and (possibly-vars-equal? args
                                            vars
                                            arg->maybe-var
                                            id)
                      (or (corescheme-ref? proc)
                          (and (corescheme-lambda? proc)
                               (not (.references? proc vars)))))
                 proc)

                (else
                 (return-s*))))
        (return-s*))))

(def.* (corescheme-app.optimize s)
  (let ((proc* (.optimize proc))
        (args* (map .optimize args)))
    (def (fallback)
         (corescheme-app proc* args*))
    (if
     (CORESCHEME-OPTIMIZE-PARAMETER apply?)
     (if
      (corescheme-lambda? proc*)
      (with.
       corescheme-lambda proc* ;; expr and vars

       (if (lengths-= args* vars)

           (cond
            ;; Apply if the lambda is trivial (it only has one argument and
            ;; its body is the variable for that argument), or the
            ;; body is an app and the variable is used only once and
            ;; no app calls are to its left (left-to-right evaluation
            ;; order?)--no, be safe and check that *no* other sub-app
            ;; arguments are apps--or, allow it since order of eval in
            ;; app is unspecified by Scheme:
            ((or (and (length-= vars 1)
                      (corescheme-ref? expr)
                      (corescheme-var.equal? (corescheme-ref.var expr)
                                             (first vars)))
                 (and (corescheme-app? expr)
                      (let
                          ;; this is the sub-application, i.e.
                          ;; ((lambda (a b c) (c b a) ;; <-- this
                          ;;     ) (g) (h) f)
                          ;; BTW order of evaluation: I see. Thanks to
                          ;; leaving it open, this case *can* be
                          ;; optimized.
                          ((proc** (corescheme-app.proc expr))
                           (args** (corescheme-app.args expr)))
                        (and (.every args**
                                     ;; corescheme-lambda? nope; really
                                     ;; only these two:
                                     (either corescheme-ref?
                                             corescheme-literal?))
                             (.every vars
                                     (lambda (var)
                                       ;; must occur, otherwise the
                                       ;; orig expr would not be
                                       ;; evaluated anymore.
                                       (one? (.num-references expr var))))))))
             (.interpolate expr vars args*))
     
            ;; Interpolate lexical lambda arguments which are
            ;; variables, or literals or lambdas and are only used once
            ;; (to, currently, avoid undue code bloat) (XX losing
            ;; naming for values, bad esp. for lambdas?)

            ;; XX hm schould instead really move args inside until
            ;; endofpossibilities, THEN if ((lambda ()..)) then move
            ;; .. out.  this way, makes lambdas smaller,
            ;; too. well.

            ;; ((lambda (x) (g x y)) y)

            ;; If none of the argument expressions are self-evaluating
            ;; (i.e. their evaluation results in itself--XX add a
            ;; predicate?), replace the app with the body of proc*, with the
            ;; variables interpolated positionally like the args* (this is
            ;; function application!..). |vars| and |expr| are |proc*|'s.

            ;; (can't use |every| since also need to walk vars)
            ((let lp ((vars vars)
                      (args args*))
               (if-let-pair
                ((var vars*) vars)
                (let-pair
                 ((arg args*) args)
                                           
                 (and (or (corescheme-ref? arg)
                          (and (or (corescheme-lambda? arg)
                                   (corescheme-literal? arg))
                               ;; the corresponding variable is only
                               ;; used max once
                               (<= (.num-references expr var) 1)))
                      (lp vars* args*)))
                (begin
                  (assert (null? args))
                  #t)))
             (.interpolate expr vars args*))

            (else
             (fallback)))

           (source-error
            s
            "immediate lambda application with different number of arguments than variables")))
      (fallback))
     (fallback))))

(def.* (corescheme-def.optimize s)
  (let ((val* (.optimize val)))
    (corescheme-def var val*)))

(def.* (corescheme-set!.optimize s)
  (let ((val* (.optimize val)))
    (corescheme-set! var val*)))

(def.* (corescheme-begin.optimize s)
  (corescheme-begin (map .optimize body)))

(def.* (corescheme-if.optimize s)
  (corescheme-if (.optimize test)
                 (.optimize then)
                 (and else (.optimize else))))

(def.* (corescheme-letrec.optimize s)
  (corescheme-letrec vars
                     (map .optimize exprs)))


(TEST
 > (def parameters corescheme-optimize-parameters-on)
 > (def t
        (=>* (source.corescheme globals: '(f g h .>>=))
             (corescheme-optimize parameters: parameters)
             .scheme))

 ;; remove needless lambda wrappers:
 > (t '(lambda (x y) (f x y)))
 f
 > (%try (t '(lambda (x #!optional y) (f x y))))
 ;; in future: (lambda (x #!optional y) (f x y))
 (exception
  text: "name does not match (possibly-source-of symbol?): #!optional\n")
 ;; can't optimize this since it would change evaluation order:
 > (t '(lambda (x y) ((f) x y)))
 (lambda (x y) ((f) x y))

 ;; lambda instead of symbol in call position:
 > (def parameters (=> corescheme-optimize-parameters-off (.apply?-set #t)))
 > (t '(lambda (y) ((lambda (x) (.>>= (g x) h)) y)))
 (lambda (y) (.>>= (g y) h))
 > (t '(lambda (q) (lambda (y) ((lambda (x) (.>>= (g x q) h)) y))))
 (lambda (q) (lambda (y) (.>>= (g y q) h)))

 ;; with app optimization turned on as well, that one happens first,
 ;; hence we get variables renamed
 > (def parameters corescheme-optimize-parameters-on)
 > (t '(lambda (y) ((lambda (x) (.>>= (g x) h)) y)))
 (lambda (y) (.>>= (g y) h))
 > (t '(lambda (q) (lambda (y) ((lambda (x) (.>>= (g x q) h)) y))))
 (lambda (q) (lambda (y) (.>>= (g y q) h)))
 ;;XX and add test to see conflict handling. Already easily happens.
 
 ;; lambda that captures a variable of its parent lambda thus can't be
 ;; optimized: (could interpolate y though instead to get rid of it)
 > (t '(lambda (y) ((lambda (x) (.>>= (g x y) h)) y)))
 ;; (lambda (y) ((lambda (x) (.>>= (g x y) h)) y))
 ;; now since interpolation:
 (lambda (y) (.>>= (g y y) h))

 ;; interpolate lexical lambda arguments which are variables or literals
 > (t '(lambda (y) ((lambda (x) (g x y)) y)))
 (lambda (y) (g y y))
 > (t '(lambda (y) ((lambda (x) (g x)) y)))
 g
 > (t '(lambda (y) ((lambda (x z) (g x y z)) y 10)))
 (lambda (y) (g y y 10))
 ;; or lambdas
 > (t '(lambda (y) ((lambda (x) (g x y)) (lambda () y))))
 (lambda (y) (g (lambda () y) y))
 ;; when used twice, do not inline:  (XX wording: inline==interpolate?)
 > (t '(lambda (y) ((lambda (x) (g x x y)) (lambda () y))))
 (lambda (y) ((lambda (x) (g x x y)) (lambda () y)))
 )
