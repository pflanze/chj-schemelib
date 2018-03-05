;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy)

(export (method u8vector.integer-unsigned-le)
	(method u8vector.integer-unsigned-be))

;; XX oh, also see u8vector->integer in cj-u8vector-util.scm

;; XX make faster via casting in C?

;; u8vector.natural0-be ?

(def. (u8vector.integer-unsigned-le v)
  (let ((len (u8vector-length v)))
    (let lp ((i 0)
	     (fact 1)
	     (x 0))
      (if (< i len)
	  (lp (inc i)
	      (* fact 256)
	      (+ x (* fact (u8vector-ref v i))))
	  x))))

(def. (u8vector.integer-unsigned-be v)
  (let ((len (u8vector-length v)))
    (let lp ((i (dec len))
	     (fact 1)
	     (x 0))
      (if (>= i 0)
	  (lp (dec i)
	      (* fact 256)
	      (+ x (* fact (u8vector-ref v i))))
	  x))))


(TEST
 > (def vs (list (u8vector 10)
		 (u8vector 10 1)
		 (u8vector 1 10)
		 (u8vector 255 254 255 255)
		 (u8vector 1 0 0 0 0)
		 (u8vector 0 0 0 0 1)))
 > (map .integer-unsigned-be vs)
 (10
  2561
  266
  4294901759 ;; #xfffeffff
  4294967296 ;; #x0100000000
  1)
 > (map .integer-unsigned-le vs)
 (10
  266
  2561
  4294967039 ;; #xfffffeff
  1
  4294967296 ;; #x0100000000
  ))

