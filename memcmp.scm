;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 cj-symbol)

(export (macro memcmp:@string=?) memcmp:@string=?-function
	(macro memcmp:string=?) memcmp:string=?-function)

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


(compile-time
 (assert (mod:compiled?)))

(use-memcmp)

(define (memcmp:@string=?-function s1 s2)
  (memcmp:@string=? s1 s2))

(define (memcmp:string=?-function s1 s2)
  (memcmp:string=? s1 s2))


