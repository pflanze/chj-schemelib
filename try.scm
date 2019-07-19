;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy)

(export (macro try))

(defmacro (try form1 . forms)
  (if (pair? forms)
      (let ((catchform (last forms))
            (body (butlast forms)))
        (mcase
         catchform
         (`(catch . `catch-clauses)
          (with-gensyms
           (C E ORIG)
           `(continuation-capture
             (lambda (,C)
               (let ((,ORIG (current-exception-handler)))
                 (with-exception-handler
                  (lambda (,E)
                    (cond
                     ,@(map (lambda (catch-clause)
                              (mcase
                               catch-clause
                               (`(`perhaps-typed-var . `body)
                                (if (typed? perhaps-typed-var)
                                    `((,(typed.predicate perhaps-typed-var)
                                       ,E)
                                      (continuation-graft
                                       ,C
                                       (let ((,(typed.var perhaps-typed-var)
                                              ,E))
                                         (lambda () ,@body))))
                                    ;; catch-all: [check that
                                    ;; there's only one such
                                    ;; clause?]
                                    `(#t
                                      (continuation-graft
                                       ,C
                                       (let ((,perhaps-typed-var
                                              ,E))
                                         (lambda () ,@body))))))))
                            catch-clauses)
                     (else
                      (,ORIG ,E))))
                  (lambda ()
                    ,form1
                    ,@body)))))))))
      (source-error stx "need at least 1 body form and the catch form")))


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
 77)

