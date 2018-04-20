;;; Copyright 2014-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy)

(export hexdigit
	(method integer.parse-hexdigit
		char.parse-hexdigit
		u8vector.hex-string
		string.parse-hex))


(define (hexdigit digit)
  (if (< digit 10)
      (integer->char (+ digit (char->integer #\0)))
      (integer->char (+ (- digit 10) (char->integer #\A)))))


;; stupid name?
(def. (integer.parse-hexdigit x)
  (cond ((<= (char->integer #\0)
	     x
	     (char->integer #\9))
	 (- x (char->integer #\0)))
	((<= (char->integer #\a)
	     x
	     (char->integer #\f))
	 (+ 10 (- x (char->integer #\a))))
	((<= (char->integer #\A)
	     x
	     (char->integer #\F))
	 (+ 10 (- x (char->integer #\A))))
	(else
	 ;; better print char-encoded ?
	 (error "not a character representing a hex digit:" x))))

(def. (char.parse-hexdigit c)
  (integer.parse-hexdigit (char.integer c)))

(TEST
 > (.parse-hexdigit #\9)
 9
 > (.parse-hexdigit #\f)
 15
 > (%try-error (.parse-hexdigit #\g))
 #(error "not a character representing a hex digit:" 103))


(def. (u8vector.hex-string v)
  (let* ((len (u8vector-length v))
	 (o (##make-string (* 2 len))))
    (for..< (i 0 len)
	    (let* ((x (u8vector-ref v i))
		   (ii (* 2 i)))
	      (string-set!
	       o ii
	       (hexdigit (arithmetic-shift x -4)))
	      (string-set!
	       o (inc ii)
	       (hexdigit (bitwise-and x 15)))))
    o))

(TEST
 > (.hex-string (u8vector 16))
 "10"
 > (.hex-string (u8vector 255))
 "FF"
 > (.hex-string (u8vector 255 10))
 "FF0A")


(def. (string.parse-hex v)
  ;; to u8vector, ok?
  (let* ((len (string-length v)))
    (assert (even? len))
    (let* ((len (arithmetic-shift len -1))
	   (o (##make-u8vector len)))
      (for..< (i 0 len)
	      (let* ((ii (* 2 i)))
		(u8vector-set!
		 o i
		 (+ (arithmetic-shift (char.parse-hexdigit
				       (string-ref v ii)) 4)
		    (char.parse-hexdigit (string-ref v (inc ii)))))))
      o)))

(TEST
 > (string.parse-hex "abcd0107")
 #u8(171 205 1 7)
 > (string.parse-hex "FFfe")
 #u8(255 254))

