;;; Copyright 2014-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Code templates:

;; A code template consists of named template arguments (symbols), and
;; the code containing these symbols either directly (as in |eq?|) or
;; as substrings in symbols or strings. Replacements are done on an
;; s-expression basis, not Scheme AST. Also, substring matches in
;; symbols and strings are replaced, too[1]. Therefore, template
;; variable names have to be chosen carefully to avoid accidental
;; matches. It is suggested to delimit variable names in some way,
;; like $T$ or <T>.

;; [1] One could change the implementation to only replace matches if
;; surrounded by non word (as in [a-zA-Z0-9_]) characters or the end
;; of the string/symbol or surrounded by $ on both ends (in which case
;; the $ are part of the match). But once $ are used, why not make
;; them part of the variable name and leave it all simple? It is then
;; easier to search for them via e.g. text editor functionality, too.

;; There are 3 forms:

;; (template ARGS . CODE) returns a function that when called returns
;; CODE with the elements in ARGS replaced by the respective value
;; given to the function.

;; (deftemplate (NAME . ARGS) . CODE) defines a macro NAME (and
;; corresponding NAME-expand), that when called does the same as
;; |template|, but includes the result in the surrounding code.

;; (template-map ((ARG0 VALS) ...) . CODE) maps over CODE and VALS
;; (length VALS) times, replacing ARG0 with each value in return;
;; i.e. instantiates the template multiple times directly in place.


(require test
	 (string-util-3 string.replace-substring)
	 easy-1)

(export (macro template)
	(macro deftemplate)
	(macro template-map)
	#!optional
	symbol.replace-substrings
	code-symbol-or-substring-replace)



(def. (symbol.replace-substrings s substr withstr)
  (string.symbol
   (string.replace-substrings (symbol.string s) substr withstr)))

(TEST
 > (symbol.replace-substrings 'VECTORVECTOR "VECTOR" "vec")
 vecvec
 > (symbol.replace-substrings 'VECTOR.VECTOR "VECTOR" "vec")
 vec.vec)

(def (code-symbol-or-substring-replace code replacements)
     (let replace ((code code))
       (let ((code* (source-code code))
	     (S (C sourcify _ code)))
	 (cond
	  ((and (symbol? code*)
		(assq code* replacements))
	   => (lambda (r)
		;; replace symbol with the actual value directly
		;; without going through -> string -> symbol
		;; conversion
		(S (cdr r))))
	  (else
	   (mcase code
		  ((either symbol? string?)
		   (S (fold (lambda (replacement code*)
			      (let-pair ((subsymbol newsubsymbol) replacement)
					(.replace-substrings
					 code*
					 (symbol.string subsymbol)
					 (.string
					  (source-code newsubsymbol)))))
			    code*
			    replacements)))
		  (pair?
		   (S (improper-map replace code*)))
		  (vector?
		   (S (vector-map replace code*)))
		  (else
		   code)))))))


(defmacro (template subsymbols code0 . coder)
  (assert*
   list? subsymbols
   (lambda (subsymbols*)
     (let ((code (if (pair? coder)
		     (sourcify `(,(sourcify `begin code0) ,code0 ,@coder)
			       code0)
		     code0)))
     
       `(lambda ,subsymbols
	  (code-symbol-or-substring-replace
	   (u8vector->object ',(object->u8vector code))
	   ;; build alist:
	   (list ,@(map (lambda (subsymbol)
			  (assert* symbol? subsymbol
				   (lambda_
				    `(cons ',subsymbol ,subsymbol))))
			subsymbols*))))))))

(defmacro (deftemplate name+subsymbols code0 . coder)
  (mcase name+subsymbols
	 (`(`name . `subsymbols)
	  `(defmacro (,name ,@subsymbols)
	     ((template ,subsymbols ,code0 ,@coder) ,@subsymbols)))))


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

(defmacro (template-map binds code0 . coder)
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
			      ,code0
			      ,@coder)
		    ,@vals-codes)))))))

(TEST
 > (deftemplate (mydeft FOO)
     (def (hello)
	  "FOO world"))
 > (mydeft Hi)
 > (hello)
 "Hi world"
 > (template-map
    ((<X> '(s32 s8))
     (<Y> '(signed-32 signed-8)))
    (def. (<X>vector.doit x) (cons "hello<X> <Y>" '<Y>)))
 > (.doit (s8vector 1))
 ("hellos8 signed-8" . signed-8)
 > (.doit (s32vector 1))
 ("hellos32 signed-32" . signed-32))

