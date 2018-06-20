;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 (fixnum-more fixnum-natural0?)
	 cj-symbol)

(export (macro memcmp:@string=?) memcmp:@string=?-function
	(macro memcmp:string=?) memcmp:string=?-function
	(macro memcmp:@substring=?) memcmp:@substring=?-function
	(macro memcmp:substring=?) memcmp:substring=?-function)

(compile-time
 (assert (mod:compiled?)))


(include "cj-standarddeclares.scm")


(define-macro* (use-memcmp)
  (if (mod:compiled?)
      `(c-declare "
#include <string.h>
")
      `(begin)))

(define-macro* (memcmp:@string=? s1 s2)
  (if (mod:compiled?)
      `(##c-code "
int l1= ___INT(___STRINGLENGTH(___ARG1));
int l2= ___INT(___STRINGLENGTH(___ARG2));

if (l1==l2) {
    ___RESULT= memcmp(___BODY(___ARG1), ___BODY(___ARG2), l1*4)==0
                 ? ___TRU : ___FAL;
} else {
    ___RESULT= ___FAL;
}
" ,s1 ,s2)
      `(memcmp:@string=?-function ,s1 ,s2)))

(define (memcmp:string=?-error s1 s2)
  (error "memcmp:string=?: need two strings, got:" s1 s2))

(define-macro* (memcmp:string=? s1 s2)
  (if (mod:compiled?)
      (with-gensyms
       (S1 S2)
       `(let ((,S1 ,s1)
	      (,S2 ,s2))
	  (if (and (string? ,S1)
		   (string? ,S2))
	      (memcmp:@string=? ,S1 ,S2)
	      (memcmp:string=?-error ,S1 ,S2))))
      `(memcmp:string=?-function ,s1 ,s2)))


(use-memcmp)

(define (memcmp:@string=?-function s1 s2)
  (memcmp:@string=? s1 s2))

(define (memcmp:string=?-function s1 s2)
  (memcmp:string=? s1 s2))


(define-macro* (memcmp:@substring=? s1 i1 s2 i2 len)
  (if (mod:compiled?)
      `(##c-code "
___WCHAR* s1= ___CAST(___WCHAR*,___BODY(___ARG1));
int i1= ___INT(___ARG2);
___WCHAR* s2= ___CAST(___WCHAR*,___BODY(___ARG3));
int i2= ___INT(___ARG4);
int len= ___INT(___ARG5);

int l1= ___INT(___STRINGLENGTH(___ARG1));
int l2= ___INT(___STRINGLENGTH(___ARG3));

if (((i1 + len) <= l1) && ((i2 + len) <= l2)) {
    ___RESULT= memcmp(&(s1[i1]), &(s2[i2]), len*4)==0
                 ? ___TRU : ___FAL;
} else {
    ___RESULT= ___FAL;
}
" ,s1 ,i1 ,s2 ,i2 ,len)
      `(memcmp:@substring=?-function ,s1 ,i1 ,s2 ,i2 ,len)))

(define (memcmp:@substring=?-function s1 i1 s2 i2 len)
  (memcmp:@substring=? s1 i1 s2 i2 len))


(define (memcmp:substring=?-error s1 i1 s2 i2 len)
  (error "memcmp:substring=?: need string, natural0 (start), string, natural0 (start), natural0 (len):" s1 i1 s2 i2 len))

(define-macro* (memcmp:substring=? s1 i1 s2 i2 len)
  (if (mod:compiled?)
      (with-gensyms
       (S1 I1 S2 I2 LEN)
       `(let ((,S1 ,s1)
	      (,I1 ,i1)
	      (,S2 ,s2)
	      (,I2 ,i2)
	      (,LEN ,len))
	  (if (and (string? ,S1)
		   (fixnum-natural0? ,I1)
		   (string? ,S2)
		   (fixnum-natural0? ,I2)
		   (fixnum-natural0? ,LEN))
	      ;; overflow check is being done in memcmp:@substring=?
	      (memcmp:@substring=? ,S1 ,I1 ,S2 ,I2 ,LEN)
	      (memcmp:substring=?-error ,S1 ,I1 ,S2 ,I2 ,LEN))))
      `(memcmp:substring=?-function ,s1 ,i1 ,s2 ,i2 ,len)))


(define (memcmp:substring=?-function s1 i1 s2 i2 len)
  (memcmp:substring=? s1 i1 s2 i2 len))
