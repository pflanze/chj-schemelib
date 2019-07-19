;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy)

(export (macro try))

(include "cj-standarddeclares.scm")


(def (try:run proc/cont handler-for)
     (continuation-capture
      (lambda (return)
        (let ((orig-handler (current-exception-handler)))
          (with-exception-handler
           (lambda (e)
             (cond ((handler-for e)
                    => (lambda (handler)
                         (continuation-graft return handler)))
                   (else
                    (orig-handler e))))
           (lambda ()
             (proc/cont return)))))))

(def ((try:return-expander C) stx)
     ;; C is symbol holding the continuation
     (cj-sourcify-deep
      (match* stx
              ((_ expr)
               `(continuation-return ,C ,expr)))
      stx))

(TEST
 > (cj-desourcify ((try:return-expander 'FOO)
                   (quote-source (try:return helloworld))))
 (continuation-return FOO helloworld))


(defmacro (try form1 . forms)
  (if (pair? forms)
      (let ((catchform (last forms))
            (body (butlast forms)))
        (mcase
         catchform
         (`(catch . `catch-clauses)
          (with-gensyms
           (E C)
           `(try:run
             (lambda (,C)
               (##define-syntax try:return (try:return-expander ',C))
               ,form1
               ,@body)
             (lambda (,E)
               (cond
                ,@(map (lambda (catch-clause)
                         (mcase
                          catch-clause
                          (`(`perhaps-typed-var . `body)
                           (if (typed? perhaps-typed-var)
                               `((,(typed.predicate perhaps-typed-var)
                                  ,E)
                                 (let ((,(typed.var perhaps-typed-var)
                                        ,E))
                                   (lambda () ,@body)))
                               ;; catch-all: [check that
                               ;; there's only one such
                               ;; clause?]
                               `(#t
                                 (let ((,perhaps-typed-var
                                        ,E))
                                   (lambda () ,@body)))))))
                       catch-clauses)
                (else
                 #f))))))))
      (source-error stx "need at least 1 body form and the catch form")))



(TEST
 > (define TEST:equal? syntax-equal?)
 > (expansion (try (raise val)
                   77
                   (catch ([list? e] 111))))
 (try:run (lambda (GEN:C-5873) (raise val) 77)
          (lambda (GEN:E-5872)
            (cond ((list? GEN:E-5872) (let ((e GEN:E-5872)) (lambda () 111)))
                  (else #f))))
 > (expansion (try (raise val)
                   (try:return 88)
                   77
                   (catch ([list? e] 111))))
 (try:run (lambda (GEN:C-6810)
            (raise val)
            (continuation-return GEN:C-6810 88)
            77)
          (lambda (GEN:E-6809)
            (cond ((list? GEN:E-6809) (let ((e GEN:E-6809)) (lambda () 111)))
                  (else #f)))))

(TEST
 > (def (t val)
        (with-exception-handler
         (lambda (e)
           (list 'orig-handler e))
         (lambda ()
           (try (raise val)
                77
                (catch ([list? e] 111))))))
 > (t 10)
 77
 > (t '(10))
 111

 > (def (t val)
        (with-exception-handler
         (lambda (e)
           (list 'orig-handler e))
         (lambda ()
           (try (raise val)
                77
                (catch (e 111))))))
 > (t 10)
 111
 > (t '(10))
 111

 > (def (t val)
        (with-exception-handler
         (lambda (e)
           (list 'orig-handler e))
         (lambda ()
           (try (raise val)
                77
                (catch)))))
 > (t 10)
 77
 > (t '(10))
 77

 ;; The return feature:
 > (def (t val)
        (with-exception-handler
         (lambda (e)
           (list 'orig-handler e))
         (lambda ()
           (try (raise val)
                (try:return 88)
                77
                (catch ([list? e] 111))))))
 > (t 10)
 88
 > (t '(10))
 111)

