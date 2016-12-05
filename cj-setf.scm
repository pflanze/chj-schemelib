;;; Copyright 2010-2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.



(require cj-match
	 cj-phasing
	 easy)


(export vector-inc!
	vector-dec!
	vector-add!
	(macro INC!
	  DEC!
	  SET!))


;; move to cj-env (to inc!, dec!)? -- AH there is also oo-vector-lib.scm

(def (vector-inc! v i)
     (vector-set! v i
		  (inc (vector-ref v i))))

(def (vector-dec! v i)
     (vector-set! v i
		  (dec (vector-ref v i))))

(def (vector-add! v i x)
     (vector-set! v i
		  (+ (vector-ref v i) x)))



(compile-time
 (def (substitute-ref-to sym* op!)
      (assert*
       symbol? sym*
       (lambda (sym)
	 (let ((s (symbol.string sym)))
	   (if (string-ends-with? s "-ref")
	       (symbol-append (substring s 0 (- (string-length s) 4))
			      "-"
			      op!)
	       (source-error sym* "symbol does not end in |-ref|"))))))
 
 (def (inc-dec!-expand e op!)
      (mcase e
	     (pair?
	      (let ((a (car (source-code e))))
		(mcase a
		       (symbol?
			`(,(substitute-ref-to (source-code a) op!)
			  ,@(cdr (source-code e))))))))))

(defmacro (INC! e)
  (inc-dec!-expand e 'inc!))

(defmacro (DEC! e)
  (inc-dec!-expand e 'dec!))

(defmacro (SET! e v)
  (mcase e
	 (pair?
	  (let ((a (car (source-code e))))
	    (mcase a
		   (symbol?
		    `(,(substitute-ref-to (source-code a) 'set!)
		      ,@(cdr (source-code e))
		      ,v)))))))

