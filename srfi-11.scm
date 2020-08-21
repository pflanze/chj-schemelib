;;; Copyright 2010-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 simple-match-1
	 optim-values
	 (list-util let-pair)
         (improper-list-2 improper-mapfold)
	 test
	 ;; (cj-env-1 inc dec) cj-source, sigh
	 (fixnum inc dec))


(export (macros let-values
                let*-values))


(include "cj-standarddeclares.scm")


(define-macro* (let*-values bindforms* . body)
  (match-list*
   bindforms*
   (bindforms
    (fold-right (lambda (bindform body)
		  (match-list*
		   bindform
		   ((vars expr)
		    (sourcify
		     `(%call-with-values (lambda ()
					  ,expr)
			(lambda ,vars
			  ,body))
		     vars))))
		`(begin ,@body)
		bindforms))))

(TEST
 > (expansion#let*-values (((a b) (values 1 2)) ((c d) (values (inc b) (inc a)))) c)
 (%call-with-values
  (lambda () (values 1 2))
  (lambda (a b)
    (%call-with-values
     (lambda () (values (inc b) (inc a)))
     (lambda (c d) (begin c)))))
 > (let*-values (((a b) (values 1 2)) ((c d) (values (inc b) (inc a)))) c)
 3)


(define-macro* (let-values bindforms* . body)
  (match-list*
   bindforms*
   (bindforms
    (let-pair
     ((varmap body)
      (fold-right
       (lambda (bindform varmap.body)
	 (with-pair
	  varmap.body

	  (match-list*
	   bindform
	   ((vars expr)
	    (let*-values
		(((varmap VARS) (improper-mapfold
				 (lambda (varmap v)
				   (let ((V (assert* symbol? v
						     gensym)))
				     (values (cons (cons v V)
						   varmap)
					     V)))
				 varmap 
				 (source-code vars))))
	      (cons varmap
		    (lambda (varmap)
		      (sourcify
		       `(%call-with-values (lambda ()
					     ,expr)
					   (lambda ,VARS
					     ,(body varmap)))
		       vars))))))))
       (cons '()
	     (lambda (varmap)
	       `((lambda ,(map car varmap)
		   ,@body)
		 ,@(map cdr varmap))))
       bindforms))

     (body varmap)))))


(TEST
 > (require (cj-symbol)))
(TEST
 > (let-values (((a . b) (values 1 2))) a)
 1
 > (let-values (((a . b) (values 1 2))) b)
 (2)
 ;; > (let ((one 1)) (let-values (((one . rest) (values 100 200)) ((two . rest) (values 2))) (vector one two rest)))
 ;; *** ERROR IN (console)@31.69 -- Duplicate parameter in parameter list
 ;; 1> 
 ;; heh
 > (let ((one 1)) (let-values (((one . rest) (values 100 200)) ((two . rest2) (values (inc one)))) (vector one two rest rest2)))
 #(100 2 (200) ())

 > (define TEST:equal? syntax-equal?)
 > (expansion#let-values (((one . rest) (values 100 200)) ((two . rest2) (values (inc one)))) (vector one two rest rest2))
 (%call-with-values
  (lambda () (values 100 200))
  (lambda (GEN:one4532 . GEN:rest4531)
    (%call-with-values
     (lambda () (values (inc one)))
     (lambda (GEN:two4530 . GEN:rest24529)
       ((lambda (one rest two rest2) (vector one two rest rest2))
	GEN:one4532
	GEN:rest4531
	GEN:two4530
	GEN:rest24529))))))

