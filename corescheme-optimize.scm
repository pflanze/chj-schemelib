;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         corescheme
         test)


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


(def (corescheme-optimize s)
     (let ((r (parameterize ((current-optimizing? #t))
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
    ;; remove needless lambda wrappers: (lambda (x y) (f x y))
    (if (corescheme-app? expr*)
        (with. corescheme-app expr*
               (if (and (possibly-vars-equal? args
                                              vars
                                              arg->maybe-var
                                              id)
                        (or (corescheme-ref? proc)
                            (and (corescheme-lambda? proc)
                                 (not (.references? proc vars)))))
                   proc
                   (return-s*)))
        (return-s*))))

(def.* (corescheme-app.optimize s)
  ;; just passing through for now. Why is passing-through code so
  ;; verbose...
  (let ((proc* (.optimize proc))
        (args* (map .optimize args)))
    (corescheme-app proc* args*)))

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
 > (def t (=>* (source.corescheme globals: '(f g h .>>=))
               corescheme-optimize
               .scheme))

 ;; remove needless lambda wrappers:
 > (t '(lambda (x y) (f x y)))
 f
 > (%try (t '(lambda (x #!optional y) (f x y))))
 ;; in future: (lambda (x #!optional y) (f x y))
 (exception text: "name does not match (possibly-source-of symbol?): #!optional\n")
 ;; can't optimize this since it would change evaluation order:
 > (t '(lambda (x y) ((f) x y)))
 (lambda (x y) ((f) x y))
 ;; lambda instead of symbol in call position:
 > (t '(lambda (y) ((lambda (x) (.>>= (g x) h)) y)))
 (lambda (x) (.>>= (g x) h))
 > (t '(lambda (q) (lambda (y) ((lambda (x) (.>>= (g x q) h)) y))))
 (lambda (q) (lambda (x) (.>>= (g x q) h)))
 ;; lambda that captures a variable of its parent lambda thus can't be
 ;; optimized: (could interpolate y though instead to get rid of it)
 > (t '(lambda (y) ((lambda (x) (.>>= (g x y) h)) y)))
 (lambda (y) ((lambda (x) (.>>= (g x y) h)) y))
 )
