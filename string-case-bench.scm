;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 string-case)

(include "cj-standarddeclares.scm")


(def (t1 v)
     (declare (fixnum) (not safe)) ;; should be part of the lib instead
     (string-case v
		  (("ho") 'ho)
		  (("hi") 'yes)
		  (("hello world") 'theworld)
		  (("hello lovely world how are you today? it's been a long way.")
		   'theworld2)
		  (else 'nomatch)))

(def (t2 v)
     (cond ((string=? v "ho") 'ho)
	   ((string=? v "hi") 'yes)
	   ((string=? v "hello world") 'theworld)
	   ((string=? v "hello lovely world how are you today? it's been a long way.")
	    'theworld2)
	   (else 'nomatch)))



(c-declare "
#include <string.h>
")
(def (@memcmp:string=? s1 s2)
     (##c-code "
int l1= ___INT(___STRINGLENGTH(___ARG1));
int l2= ___INT(___STRINGLENGTH(___ARG2));

if (l1==l2) {
    ___RESULT= memcmp(___BODY(___ARG1), ___BODY(___ARG2), l1*4)==0
                 ? ___TRU : ___FAL;
} else {
    ___RESULT= ___FAL;
}
" s1 s2))

(def (t3 v)
     (cond ((@memcmp:string=? v "ho") 'ho)
	   ((@memcmp:string=? v "hi") 'yes)
	   ((@memcmp:string=? v "hello world") 'theworld)
	   ((@memcmp:string=? v "hello lovely world how are you today? it's been a long way.")
	    'theworld2)
	   (else 'nomatch)))


(def (string-case-bench str n)
     (equal?* (time (repeat n (t1 str)))
	      (time (repeat n (t2 str)))
	      (time (repeat n (t3 str)))))

