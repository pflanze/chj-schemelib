;;; Copyright 2006-2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;;;XXX sort out what I still want/need
(require fixnum
	 (cj-gambit-sys max-fixnum min-fixnum)
	 (srfi-1 cons*)
	 (cj-string-flatten flat-append-strings)
	 cj-env
	 (cj-functional applying)
	 (list-util-1 map/iota)
	 test)

;; (compile #f);; since it's only a macro.

;; (exports-macros
;;  define-constant-from-C
;;  maybe-define-constant-from-C
;;  HACK_maybe-define-constant-from-C
;;  define-struct-field-accessors
;;  define-struct-accessors
;;  define-struct-from-c
;;  )



(include "cj-standarddeclares.scm")

;; XX move to one of the string libs?
(define (substring-after str ch)
  ;; strip everything before the last occurrence of char ch
  (let ((len (string-length str)))
    (let lp ((i (dec len)))
      (if (negative? i)
	  str
	  (let ((c (string-ref str i)))
	    (if (char=? c ch)
		(substring str (inc i) len)
		(lp (dec i))))))))

(TEST
 > (substring-after "foo:" #\:)
 ""
 > (substring-after "foo:bar" #\:)
 "bar"
 > (substring-after "foo:bar:baz" #\:)
 "baz"
 > (substring-after ":baz" #\:)
 "baz"
 > (substring-after "baz" #\:)
 "baz")


;; Note: it seems CPP doesn't support number constants exceeding 32
;; bits, hence can't check for fixnum range using something like '#if
;; (" namestr " > " max-fixnum-str ")'? And instead can just be
;; 'trusted'? (Or what? See commit msg.)
(define (code:constant-from-C namestr)
  (string-append "___RESULT= ___FIX(" (substring-after namestr #\:) ");"))


(define define-constant-from-C-code
  (lambda (name)
    `(define ,name
       (##c-code ,(code:constant-from-C (symbol->string name))))))

(define-macro* (define-constant-from-C name)
  (assert* symbol? name
	   define-constant-from-C-code))


;; define the constant to be either false, if it doesn't exist in C
;; space, or the integer value if it does:

(define maybe-define-constant-from-C-code
  (lambda (name)
    (let ((namestr (symbol->string name)))
      `(define ,name
	 (##c-code ,(string-append "
#ifdef " namestr "
" (code:constant-from-C namestr) "
#else
___RESULT= ___FAL;
#endif
"))))))

(define-macro* (maybe-define-constant-from-C name)
  (assert* symbol? name
	   maybe-define-constant-from-C-code))



;; hehe this one does not even need the client modul to be compiled :)
;; (but I really wrote it since I couldn't figure out how to get at
;; the O_DIRECTORY definition in C)

(define-macro* (HACK_maybe-define-constant-from-C name)
  (assert*
   symbol? name
   (lambda (name)
     (let* ((namestr (symbol->string name))
	    (p (open-process (list path: "perl"
				   arguments:
				   (list "-w"
					 (string-append "-MFcntl=" namestr)
					 "-e"
					 (string-append "print " namestr)))))
	    (num (read p))
	    (status (process-status p)))
       (close-port p)
       (if (= status 0)
	   (if (integer? num)
	       `(define ,name ,num)
	       (error "HACK_maybe-define-constant-from-C: perl returned non-integer value:" num))
	   (error "HACK_maybe-define-constant-from-C: perl exited with status:" status))))))

(define code:define-struct-field-accessors
  ;; assumes that structname is also it's typename
  (lambda (structname fieldtype fieldname mutable?
		      #!key
		      c-field-prefix)
    (let ((c-fieldname-str (string-append
			    (or c-field-prefix "")
			    (symbol->string fieldname))))
      `(begin
	 (define ,(symbol-append structname "-" fieldname)
	   (c-lambda (,structname)
		     ,fieldtype
		     ,(string-append "___result= ___arg1->"
				     c-fieldname-str
				     ";")))
	 ,@(if mutable?
	       `((define ,(symbol-append structname "-" fieldname "-set!")
		   (c-lambda (,structname ,fieldtype)
			     void
			     ,(string-append "___arg1->"
					     c-fieldname-str
					     "=___arg2;"))))
	       '())))))

(define (code:define-struct-accessors #!key
				      structname
				      c-field-prefix
				      fielddefs)
  `(begin
     ,@(map (lambda (fielddef)
	      (apply code:define-struct-field-accessors
		     (append (cons structname fielddef)
			     (list c-field-prefix: c-field-prefix))))
	    fielddefs)))

(define-macro* (define-struct-field-accessors . args)
  (apply code:define-struct-field-accessors args))

;; (define-macro* (define-struct-accessors . args)
;;   ;; really difficult dsssl stufff
;;   (apply (lambda (structname #!key c-field-prefix )
;; 	   (code:define-struct-accessors structname: structname
;; 					 c-field-prefix: c-field-prefix
;; 					 fielddefs: fielddefs))
;; 	 args))
;;XX messed up re fielddefs. Unused.

(define fielddef:fieldtype
  (applying (lambda (fieldtype fieldname mutable?)
	      fieldtype)))

(define code:define-struct-from-c
  (lambda (structname
	   #!key
	   c-release-function ;; string, required, is not autogenerated
	   c-field-prefix ;; string, optional
	   #!rest
	   fielddefs)
    (let ((structnamestr (symbol->string structname))
	  (arglist (map cadr fielddefs)))
      `(begin
	 (c-declare ,(flat-append-strings "
static
struct "structnamestr"*
___make_"structnamestr" () {
    struct "structnamestr" *p= calloc(1,sizeof(struct "structnamestr"));
    return p;
}"))
	 (c-define-type ,structname
			(pointer (struct ,structnamestr)
				 ,structname
				 ,(or c-release-function
				      (error "missing c-release-function keyword argument"))))
	 (define (,(symbol-append 'make- structname) ,@arglist)
	   (or ((c-lambda ,(map fielddef:fieldtype
				fielddefs)
			  ,structname
			  ,(flat-append-strings "
struct "structnamestr" *p= ___make_"structnamestr"();
___result_voidstar=p;
if (p) {
"
(map/iota (lambda (l n)
	    (apply
	     (lambda (fieldtype fieldname mutable?)
	       (let ((fieldnamestr (string-append
				    (or c-field-prefix "")
				    (symbol->string fieldname)))
		     (nstr (number->string n)))
		 (list
		  "    p->"fieldnamestr"=___arg"nstr";\n")))
	     l))
	  fielddefs
	  1)
"}
")) ,@arglist)
	       (error "can't allocate:" ',structname)))
	 ,(code:define-struct-accessors
c-field-prefix: c-field-prefix
structname: structname
fielddefs: fielddefs)))))


(define-macro* (define-struct-from-c . args)
  (pp-through
   (apply code:define-struct-from-c args)))
