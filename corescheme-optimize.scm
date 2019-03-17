;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         corescheme
         test)

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


(def. (corescheme.optimize s)
  s)


(def.* (corescheme-lambda.optimize s)
  (let ((expr* (.optimize expr)))
    ;; remove needless lambda wrappers: (lambda (x y) (f x y))
    (if (corescheme-app? expr*)
        (with. corescheme-app expr*
               (if (and (corescheme-ref? proc)
                        (possibly-vars-equal? args
                                              vars
                                              arg->maybe-var
                                              id))
                   proc
                   s))
        s)))



(TEST
 > (def t (=>* (source.corescheme globals: '(f))
               .optimize
               .scheme))

 ;; remove needless lambda wrappers:
 > (t '(lambda (x y) (f x y)))
 f
 > (%try (t '(lambda (x #!optional y) (f x y))))
 ;; in future: (lambda (x #!optional y) (f x y))
 (exception text: "name does not match (possibly-source-of symbol?): #!optional\n")
 > (t '(lambda (x y) ((f) x y)))
 (lambda (x y) ((f) x y))
 )
