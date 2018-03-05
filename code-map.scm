;;; Copyright 2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require test
	 (string-util-3 string.replace-substring)
	 easy-1)

(export (macro template)
	(macro code-map-substrings)
	#!optional
	symbol.replace-substrings
	code-symbol-substring-replace)



(def. (symbol.replace-substrings s substr withstr)
  (string.symbol
   (string.replace-substring (symbol.string s) substr withstr)))

(def (code-symbol-substring-replace code replacements)
     (let replace ((code code))
       (let ((code* (source-code code))
	     (S (C sourcify _ code)))
	 (mcase code
		((either symbol? string?)
		 (S (fold (lambda (replacement code*)
			    (let-pair ((subsymbol newsubsymbol) replacement)
				      (.replace-substrings
				       code*
				       (symbol.string subsymbol)
				       (symbol.string
					(source-code newsubsymbol)))))
			  code*
			  replacements)))
		(pair?
		 (S (improper-map replace code*)))
		(vector?
		 (S (vector-map replace code*)))
		(else
		 code)))))


(defmacro (template subsymbols code)
  (assert*
   list? subsymbols
   (lambda (subsymbols*)
     `(lambda ,subsymbols
	(code-symbol-substring-replace
	 (u8vector->object ',(object->u8vector code))
	 ;; build alist:
	 (list ,@(map (lambda (subsymbol)
			(assert* symbol? subsymbol
				 (lambda_
				  `(cons ',subsymbol ,subsymbol))))
		      subsymbols*)))))))

(TEST
 > (insert-result-of
    (cons `begin
	  (map (template
		(<X>)
		(def. (<X>vector.code-symbol-substring-map-test x)
		  (cons "hello <X> <X>" '<X>)))
	       '(u32 u8))))
 > (.code-symbol-substring-map-test (u8vector 1))
 ("hello u8 u8" . u8)
 > (.code-symbol-substring-map-test (u32vector 1))
 ("hello u32 u32" . u32))

(defmacro (code-map-substrings binds code)
  (assert*
   list? binds
   (lambda (binds)
     (let* ((bind-> (lambda (f)
		      (mcase-lambda
		       (`(`key `vals-code)
			(assert*
			 symbol? key
			 (lambda_
			  (f key vals-code)))))))
	    (keys (map (bind-> (lambda (key vals) key)) binds))
	    (vals-codes (map (bind-> (lambda (key vals-code) vals-code)) binds)))
       `(insert-result-of
	 (cons `begin
	       (map (template ,keys
			      ,code)
		    ,@vals-codes)))))))

(TEST
 > (code-map-substrings
    ((<X> '(s32 s8))
     (<Y> '(signed-32 signed-8)))
    (def. (<X>vector.doit x) (cons "hello<X> <Y>" '<Y>)))
 > (.doit (s8vector 1))
 ("hellos8 signed-8" . signed-8)
 > (.doit (s32vector 1))
 ("hellos32 signed-32" . signed-32))

