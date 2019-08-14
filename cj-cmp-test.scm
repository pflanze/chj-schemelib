;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         cj-cmp
         test-logic
         test-random)

(include "cj-standarddeclares.scm")


(define (old-@string-cmp v1 v2)
  (cond ((string<? v1 v2)
         'lt)
        ((string<? v2 v1)
         'gt)
        (else
         'eq)))

(define (old-char-cmp a b)
  (if (char=? a b)
      'eq
      (if (char>? a b)
	  'gt
	  'lt)))

(TEST
 > (repeat 10000
           (let ((s0 (short-random-string))
                 (s1 (short-random-string)))
             (assert (eq? (string-cmp s0 s1)
                          (old-@string-cmp s0 s1)))))
 #!void
 > (repeat 1000
           (let ((v0 (random-char))
                 (v1 (random-char)))
             (assert (eq? (char-cmp v0 v1)
                          (old-char-cmp v0 v1)))))
 #!void)
