;;; Copyright 2014-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require oo-vector-lib)

(export string->u8vector ;; XX eliminate for cj-u8vector-util ?
	(method string.u8vector)
	(method u8vector.string)
	;; and then all those from def-oo-vector-lib-for
	)

(include "cj-standarddeclares.scm")

(def-oo-vector-lib-for string)


;; And random other stuff as well, uh, clean up ?

;; there's a more efficient string->u8vector in cj-u8vector-util
(def string->u8vector (comp list->u8vector
			    (cut map char->integer <>)
			    string->list))

(def. string.u8vector string->u8vector)

;; also see u8vector->string in cj-u8vector-util
(def. (u8vector.string v)
  (let* ((len (u8vector-length v))
	 (o (##make-string len)))
    (declare (fixnum) (not safe))
    ;; ^ Safe as u8 values are always in the range of valid unicode
    ;; characters. (Right?..) But then there's not even a
    ;; ##integer->char so perhaps that's not where the speedup is
    ;; coming from.
    (for..< (i 0 len)
	    (string-set!
	     o i
	     (integer->char (u8vector-ref v i))))
    o))



;; Tests...

(TEST
 > (string-ref* "abc" 0)
 #\a
 > (string-ref* "abc" 1)
 #\b
 > (string-ref* "abc" -1)
 #\c
 > (string-ref* "abc" -2)
 #\b
 > (with-exception-catcher range-exception? (thunk (string-ref* "abc" -4)))
 #t
 )
