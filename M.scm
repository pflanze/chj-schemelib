;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy)

(export (macro M)
        (macro M*)
        (macro MF))


;; Quick new idea for (pattern) matching, just...  work on the
;; non-.show representation. Will only work for readable
;; representations though. Isn't this just a Scheme inconsistency? Has
;; Clojure fixed this? But could easily change to take the head as the
;; constructor and derive the predicate name from it by simply
;; appending |?|, or perhaps look up some class property for that.

;; Extend with constant matching?  Actually, the .show way would also
;; work for distinguishing constants `'foo` from variables `foo`!
;; Wow. So don't give Clojure's way priority, right?


(defmacro (M expr . clauses)
  (with-gensym
   V
   `(let ((,V ,expr))
      (xcond ,@(map (lambda (clause)
                      (assert* pair? clause
                               (lambda (clause)
                                 (let-pair
                                  ((test body) clause)
                                  (let ((test* (source-code test)))
                                    (mcase
                                     test
                                     ((pair-of (possibly-source-of symbol?)
                                               (possibly-source-of symbol?))
                                      `((pair? ,V)
                                        (let-pair ((,(car test*)
                                                    ,(cdr test*))
                                                   ,V)
                                                  ,@body)))
                                     ((vector-of (possibly-source-of symbol?))
                                      `((and (vector? ,V)
                                             (= (vector-length ,V)
                                                ,(vector-length test*)))
                                        (let-vector (,(vector->list test*)
                                                     ,V)
                                                    ,@body)))
                                     (null?
                                      `((null? ,V)
                                        ,@body))))))))
                    clauses)))))

(defmacro (M* . clauses)
  (with-gensym V
               `(lambda (,V)
                  (M ,V ,@clauses))))

(defmacro (MF expr . clauses)
  `(M (force ,expr) ,@clauses))


(TEST
 > (def t (M* ((some . other) (vector some other))
              ([some other such] (list other some such))
              ([] 'emptyvec)
              ;; ((some other such) (list other some such))
              ;; ((some other . such) (list other some such))
              ;; sgh how to do values? meh. rlly missing,consistency,sgh?.
              (() 'none)))
 > (%try (t 1))
 (exception text: "no match\n")
 > (t (list 3 4 5))
 [3 (4 5)]
 > (t (list))
 none
 > (t (vector))
 emptyvec
 > (t (vector 'a 'b 10))
 (b a 10)
 > (%try (t (vector 'a 'b)))
 (exception text: "no match\n"))



