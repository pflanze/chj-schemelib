;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require)

(export @memcmp:string=?)


(c-declare "
#include <string.h>
")
(define (@memcmp:string=? s1 s2)
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

