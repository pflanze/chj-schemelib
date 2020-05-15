;;; Copyright 2016-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         test)

(export (macro if-let*)
        (macro if-let)
        (macro when-let*)
        (macro when-let)
        (macro and-let)
        (macro if-letv)
        #!optional
        if-let*-expand
        if-let-expand)

(include "cj-standarddeclares.scm")


;; There is an if-let in Clojure: https://clojuredocs.org/clojure.core/if-let
;;   XX check if it'd doing the same thing.

;; See this in Rust: [iflet - a Rust macro to avoid nested if let](https://github.com/saghm/iflet)

;; XX study this in Swift: [Optional Chaining](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/OptionalChaining.html)

;; Some Schemer(s) here use(s) the name let-if for the single-binding
;; if-let variant here like (if-let (a 2) 3), and let-and for the
;; multi-binding if-let variant:
;; https://news.ycombinator.com/item?id=13213304

;; Also see https://news.ycombinator.com/item?id=17491228 (Fun with
;; Macros: If-Let and When-Let)



(def (if-let*-expand COND elsekey [(source-of list?) assignments] yes no)
     (with-gensym
      NO
      `(let ((,NO (lambda () ,no)))
         ,(fold-right (lambda (assignment yes)
                        (mcase assignment
                               (`(`var `test-expr)
                                (assert* symbol? var)
                                `(,COND (,test-expr => (lambda (,var) ,yes))
                                        (,elsekey (,NO))))))
                      yes
                      (source-code assignments)))))

(defmacro (if-let* assignments yes #!optional no)
  (if-let*-expand `cond #t assignments yes no))


(TEST
 > (if-let* ((a 2)) 3)
 3
 > (if-let* ((a 2)) a)
 2
 > (if-let* ((a 2)) a 4)
 2
 > (if-let* ((a #f)) a 4)
 4
 > (if-let* ((a 2)
             (b 3))
            (list a b)
            4)
 (2 3)
 > (def (f x)
        (if-let* ((a (and x (* x x)))
                  (b (dec a))
                  (c (< b 10))
                  (d (list a b)))
                 d
                 'no))
 > (f #f)
 no
 > (f 10)
 no
 > (f 2)
 (4 3)
 > (%try (if-let* ((GEN:-11015 #f)
                   (b 3))
                  (list a b)
                  GEN:-11015))
 (exception text: "Unbound variable: GEN:-11015\n"))



;; same as if-let* (and still currently short-cuts evaluation the same
;; way, i.e. currently doesn't "allow" for parallel evaluation,
;; although should we keep that open? Or different forms for that?
;; Probably?)  but don't make the variables visible to "subsequent"
;; terms, only to `yes`

(def (if-let-expand COND elsekey assignments yes no)
     (assert*
      list? assignments
      (lambda (assignments*)
        (if (and (length-= assignments* 2)
                 (not (pair? (source-code (car assignments*)))))

            ;; single-binding variant
            (mcase assignments
                   (`(`var `test)
                    (assert* symbol? var) ;; XX cj-typed ?
                    `(cond (,test => (lambda (,var) ,yes))
                           (,elsekey ,no))))
       
            ;; multi-binding variant
            (let* ((assignments**
                    (map (lambda (assignment)
                           (mcase assignment
                                  (`(`var `test-expr)
                                   (assert* symbol? var
                                            (lambda (var*)
                                              (values var
                                                      test-expr
                                                      (gensym var*)))))))
                         assignments*)))
              (with-gensym
               NO
               `(let ((,NO (lambda () ,no)))
                  ,(fold-right (lambda-values
                                ((var test-expr tmpvar) yes)
                                `(,COND (,test-expr => (lambda (,tmpvar) ,yes))
                                        (,elsekey (,NO))))
                               `(let ,(map (lambda-values
                                            ((var test-expr tmpvar))
                                            `(,var ,tmpvar))
                                           assignments**)
                                  ,yes)
                               assignments**))))))))

(defmacro (if-let assignments yes #!optional no)
  ;; if no is #f, just pass it on as code, and it will result in #f, too ":)"
  (if-let-expand `cond `#t assignments yes no))

(defmacro (when-let assignments . body)
  (if-let-expand `cond `#t assignments `(begin ,@body) `#f))

(defmacro (when-let* assignments . body)
  (if-let*-expand `cond `#t assignments `(begin ,@body) `#f))


(TEST
 > (if-let ((a 2)) 3)
 3
 > (if-let ((a 2)) a)
 2
 > (if-let ((a 2)) a 4)
 2
 > (if-let ((a #f)) a 4)
 4
 > (if-let ((a #f)) a)
 #f
 > (if-let ((a 2)
            (b 3))
           (list a b)
           4)
 (2 3)
 > (if-let ((a 2)
            (b #f))
           (list a b)
           5)
 5
 > (if-let ((a #f)
            (b 3))
           (list a b)
           5)
 5

 > (%try (let ((x 10))
           (if-let ((GEN:a-10740 (and x (* x x)))
                    (GEN:b-10741 (dec GEN:a-10740))
                    (GEN:c-10742 (< GEN:b-10741 10))
                    (d (list GEN:a-10740 GEN:b-10741)))
                   d
                   'no)))
 (exception text: "Unbound variable: GEN:a-10740\n")
 > (def (f x)
        (let ((a 100)
              (b x))
          (if-let ((a (and x (* x x)))
                   (b (dec a))
                   (c (< b 10))
                   (d (list a b)))
                  d
                  'no)))
 > (f #f)
 no
 > (f 10)
 no
 > (f 2)
 (100 2)
 > (%try (if-let ((GEN:-11015 #f)
                  (b 3))
                 (list a b)
                 GEN:-11015))
 (exception text: "Unbound variable: GEN:-11015\n")

 ;; when-let:
 > (def c 0)
 > (def (f x y)
        (when-let ((y x)
                   (y* y))
                  (inc! c)
                  (inc! c)
                  y*))
 > (f #f 'there)
 #f
 > (f 'hi #f)
 #f
 > c
 0
 > (f 'hi 'there)
 there
 > c
 2
 ;; when-let*
 > (def c 0)
 > (def (f x y)
        (when-let* ((y x)
                    (y* y))
                   (inc! c)
                   (inc! c)
                   y*))
 > (f #f 'there)
 #f
 > (f 'hi #f)
 hi
 > c
 2
 > (f 'hi 'there)
 hi
 > c
 4)



;; single-binding variants:

(TEST
 > (if-let (a 2) 3)
 3
 > (if-let (a 2) a)
 2
 > (if-let (a 2) a 4)
 2
 > (if-let (a #f) a 4)
 4)

;; Todo?: make single-binding if-let* variant as well? Not sure. I've
;; only made |let| (let (x 1) x) single-variant, too, so far.


;; rename to when-let ?
(defmacro (and-let assignments yes)
  (if-let-expand `cond `#t assignments yes #f))



(defmacro (if-letv bind yes no)
  (mcase bind
         (`(`vars `expr)
          (with-gensym
           VS
           `(if-let ((,VS ,expr))
                    (letv (,vars ,VS)
                          ,yes)
                    ,no)))))

(TEST
 > (if-letv ((a b) (values 1 2))
            b
            'no)
 2
 > (if-letv ((a b) #f)
            b
            'no)
 no)

