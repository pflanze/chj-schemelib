;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy)

(export (macro single-cond))

(include "cj-standarddeclares.scm")


(def (single-cond:error error-show-strings
			signalsum mask+tests)
     (let ((conds (map cdr
		       (filter (lambda-pair
				((mask test))
				(not (zero? (bitwise-and mask signalsum))))
			       mask+tests))))
       (error (if (null? error-show-strings)
		  (strings-join
		   (cons "single-cond: more than one test is true:"
			 conds)
		   " ")
		  (strings-join
		   (list "single-cond: for"
			 (if (= (length error-show-strings) 2)
			     "this value:"
			     "these values:")
			 (string-append
			  (strings-join error-show-strings " ")
			  (strings-join (cons ", more than one test is true:"
					      conds)
					" ")))
		   " ")))))

;; XX currently doesn't handle => 
(defmacro (single-cond . options+conds)
  
  (def (cont/else conds maybe-else error-shows)
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
		       (list ,@(fold-right (lambda (error-show tail)
					     (cons*
					      (string-append
					       (object->string
						(cj-desourcify error-show))
					       ":")
					      `(object->string ,error-show)
					      tail))
					   '()
					   error-shows))
		       ,SIGNALSUM
		       ',(map (lambda-values ((mask var test body))
					     `(,mask
					       .
					       ,(object->string (cj-desourcify test))))
			      mask+var+test+body))))))))))

  (let ()

    (def (cont/options conds error-shows)
	 (let ((perhaps-els (last conds)))
	   (mcase perhaps-els
		  (`(else . `body)
		   (cont/else (butlast conds)
			      body
			      error-shows))
		  (else
		   (cont/else conds #f error-shows)))))

    ;; XX REALLY need a proper keyword parsing library, TODO!
    (let lp ((l options+conds)
	     (error-shows '()))

      (if (null? l)
	  (cont/options l error-shows)
	  (let-pair ((a l*) l)
		    (let ((a* (source-code a)))
		      (if (keyword? a*)
			  (case a*
			    ((error-show:)
			     (if (null? l*)
				 (source-error a "expecting value after keyword")
				 (let-pair ((b l**) l*)
					   (lp l** (cons b error-shows)))))
			    (else
			     (source-error a "unknown keyword argument" a)))
			  (cont/options l error-shows))))))))


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
  "single-cond: more than one test is true: (number? v) (even? v)"]

 > (%try-error (let ((v 6))
		 (single-cond error-show: v
			      ((number? v) 'num)
			      ((string? v) 'str)
			      ((even? v) 'even))))
 [error
  "single-cond: for this value: v: 6, more than one test is true: (number? v) (even? v)"])


;; XX these should be moved out as lib with the keyword parsing stuff
(TEST
 > (with-exception-catcher
    source-error-message
    (& (eval '(let ((v 6))
		(single-cond ewfef: ef
			     ((number? v) 'num)
			     ((string? v) 'str)
			     ((even? v) 'even))))))
 "unknown keyword argument"
 > (with-exception-catcher
    source-error-message
    (& (eval '(let ((v 6))
		(single-cond error-show:)))))
 "expecting value after keyword")
