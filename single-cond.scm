;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy)

(export (macro single-cond))

(include "cj-standarddeclares.scm")


(def (single-cond:error signalsum mask+tests)
     (error
      (strings-join
       (cons "single-cond: more than one test is true:"
	     (map cdr
		  (filter (lambda-pair ((mask test))
				  (not (zero? (bitwise-and mask signalsum))))
			  mask+tests)))
       " ")))

;; XX currently doesn't handle => 
(defmacro (single-cond . conds)
  (def (cont/else conds maybe-else)
       (let ((mask+var+test+body
	      (map/iota (lambda (c i)
			  (mcase c
				 (`(`test . `body)
				  (values (arithmetic-shift 1 i)
					  (gensym)
					  test
					  body))))
			conds)))
	 (with-gensyms
	  (SIGNALSUM)
	  `(let ,(map (lambda-values ((mask var test body))
				`(,var (if ,test ,mask 0)))
		      mask+var+test+body)
	     (let ((,SIGNALSUM (fx+ ,@(map snd mask+var+test+body))))
	       (if (eq? ,SIGNALSUM 0)
		   ,@(or maybe-else `((void)))
		   (case ,SIGNALSUM
		     ,@(map (lambda-values ((mask var test body))
				      `((,mask) ,@body))
			    mask+var+test+body)
		     (else
		      (single-cond:error
		       ,SIGNALSUM
		       ',(map (lambda-values ((mask var test body))
					`(,mask
					  .
					  ,(object->string (cj-desourcify test))))
			      mask+var+test+body))))))))))
  (let ((perhaps-els (last conds)))
    (mcase perhaps-els
	   (`(else . `body)
	    (cont/else (butlast conds)
		       body))
	   (else
	    (cont/else conds #f)))))


(TEST
 > (let ((v "fun")) (single-cond ((number? v) 'num) ((string? v) 'str)))
 str
 > (let ((v 'fun)) (single-cond ((number? v) 'num) ((string? v) 'str) (else 'other)))
 other
 > (let ((v 4.3)) (single-cond ((number? v) 'num) ((string? v) 'str)))
 num
 > (let ((v 5)) (single-cond ((number? v) 'num) ((even? v) 'even)))
 num
 > (%try-error (let ((v 6))
		 (single-cond ((number? v) 'num)
			      ((string? v) 'str)
			      ((even? v) 'even))))
 [error
  "single-cond: more than one test is true: (number? v) (even? v)"])


