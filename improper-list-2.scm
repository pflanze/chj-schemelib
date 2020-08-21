;;; Copyright 2011-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require optim-values)

(export improper-mapfold)



;; Doing kind of map and fold at the same time.
;; (fn state v) -> (values state* v*)
(define (improper-mapfold fn state l #!optional (tail '()))
  (let rec ((l l))
    (cond ((null? l)
	   (values state
		   tail))
	  ((pair? l)
	   ;; (let*-values
	   ;;  (((state* l*) (rec (cdr l)))
	   ;;   ((state** v*) (fn state* (car l))))
	   ;;  (values state**
	   ;;          (cons v* l*)))
           ;; -- don't have let*-values yet --
           (%call-with-values
            (lambda () (rec (cdr l)))
            (lambda (state* l*)
              (%call-with-values
               (lambda () (fn state* (car l)))
               (lambda (state** v*) (begin (values state** (cons v* l*))))))))
	  (else
	   (fn state l)))))


