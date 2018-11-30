;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (cj-env IF)
	 (fixnum inc))

(export random-hexstring
	random-filename
	random-char-integer
	random-char
	random-string
	
	#!optional
	expensive-valid-char?)

(include "cj-standarddeclares.scm")


;; XX not fork safe

(define (random-hexstring len)
  (let* ((s (number->string (random-integer (expt 16 len)) 16))
	 (l (string-length s)))
    (if (= l len)
	s
	(string-append (make-string (- len l) #\0) s))))

(define (random-filename)
  ;; 128 bits
  (random-hexstring 32))


;; find out which integers represent valid chars:

(define (expensive-valid-char? i)
  (with-exception-catcher
   (lambda (e)
     (if (range-exception? e)
	 #f
	 (raise e)))
   (lambda ()
     (integer->char i))))

(IF #f
    (define (char-boundaries)
      (integer->char 0) ;; assert that's valid
      (delay
	(let* ((max-i 9999999999)
	       (stop 'stop))
	  (let lp-valid ((start-valid 0))
	    ;;(warn "start-valid" start-valid)
	    (let lp ((i (inc start-valid)))
	      (if (< i max-i)
		  (if (expensive-valid-char? i)
		      (lp (inc i))
		      (let ()
			;;(warn "end-valid" i)
			(cons (cons start-valid i)
			      (delay
				(let lp ((i (inc i)))
				  (if (< i max-i)
				      (if (expensive-valid-char? i)
					  (lp-valid i)
					  (lp (inc i))
					  )
				      stop))))))
		  (cons (cons start-valid #f)
			stop))))))))

;; > (def bs (char-boundaries))
;; > (stream-ref bs 1)
;; > (S bs)
;; ((0 . 55296)
;;  (57344 . 1114112)
;;  .
;;  [(debuggable-promise) #<procedure #545> #f #<continuation #546>])

;; ^ exclusive the end points.

;; 0 .. #\ud7ff
;; #\ue000 .. #\U0010ffff 

;; https://en.wikipedia.org/wiki/Unicode

;; Unicode defines a codespace of 1,114,112 code points in
;; the range 0hex to 10FFFFhex

;; Code points in the range U+D800–U+DBFF (1,024 code points) are
;; known as high-surrogate code points, and code points in the range
;; U+DC00–U+DFFF (1,024 code points) are known as low-surrogate code
;; points. A high-surrogate code point followed by a low-surrogate
;; code point form a surrogate pair in UTF-16 to represent code points
;; greater than U+FFFF. These code points otherwise cannot be used
;; (this rule is ignored often in practice especially when not using
;; UTF-16).


(define (random-char-integer)
  (declare (fixnum))
  (let ((i (random-integer 300)))
    (if (zero? i)
	0
	(if (< i 40)
	    (+ 57344 (random-integer 1056768))
	    (random-integer 55296)))))

(define (random-char)
  (integer->char (random-char-integer)))

(define (random-string len)
  (declare (fixnum))
  (if (and (fixnum? len)
	   (>= len 0))
      (let ((str (##make-string len)))
	(let lp ((i 0))
	  (if (< i len)
	      (begin
		(string-set! str i (random-char))
		(lp (inc i)))
	      str)))
      (error "not a natural0: " len)))

