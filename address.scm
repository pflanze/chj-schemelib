;;; Copyright 2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 jclass
	 test
	 (cj-test %try);; should move
	 )

(export maybe-parse-common-hex-string
	(method natural0.common-hex-string)
	(class address32)
	maybe-address32*
	address32*)


;; "0x4802_A000"
(def (maybe-parse-common-hex-string s)
     (let redo ((l (string.list s)))
       (case (first l)
	 ((#\0) (redo (rest l)))
	 ((#\x) (string->number (list->string (filter (lambda (c)
							(not (char=? c #\_)))
						      (rest l)))
				16))
	 (else #f))))

(TEST
 > (number->string (maybe-parse-common-hex-string "0x4802_A000") 16)
 "4802a000"
 > (number->string (maybe-parse-common-hex-string "0x4802_a_000") 16)
 "4802a000"
 > (number->string (maybe-parse-common-hex-string "x4802_a_000") 16)
 "4802a000"
 )

(def. (natural0.common-hex-string v #!optional maybe-bits)
  (string-append "0x"
		 (let ((s (number->string v 16)))
		   (if maybe-bits
		       (let* ((len (string-length s))
			      (formatlen (arithmetic-shift (+ maybe-bits 3) -2))
			      (fill-len (- formatlen len)))
			 (assert (>= fill-len 0))
			 (string-append (make-string fill-len #\0) s))
		       s))))


(jclass (address32 #(uint32? value))
	(def-method (show v)
	  `(address32* ,(.common-hex-string value 32))))

(def (maybe-address32* str)
     (cond ((maybe-parse-common-hex-string str) => address32)
	   (else #f)))

(def (address32* str)
     (or (maybe-address32* str)
	 (error "not an address string:" str)))

(TEST
 > (address32* "0x480ca000")
 #((address32) 1208786944)
 > (.show #)
 (address32* "0x480ca000")
 > (address32* "0x00020000")
 #((address32) 131072)
 > (.show #)
 (address32* "0x00020000")
 > (%try (address32* "480ca000"))
 (exception text: "not an address string: \"480ca000\"\n"))


