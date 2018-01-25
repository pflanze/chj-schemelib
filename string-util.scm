;;; Copyright 2010, 2011 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require test
	 (list-util let-pair)
	 (string-util-1 string-split))

(export lists-join
	strings-join
	string-escape
	first-line)


(define (lists-join ls seplis #!optional (tail '()))
  (let rec ((ls ls))
    (if (null? ls)
	tail
	(let-pair
	 ((l ls*) ls)
	 (if (null? ls*)
	     (append l (rec ls*))
	     (append l seplis (rec ls*)))))))

(TEST
 > (lists-join '((a b) (d) (e f)) '(1 2))
 (a b 1 2 d 1 2 e f)
 )

(define (strings-join strs str)
  (list->string (lists-join (map string->list strs) (string->list str))))


(define (string-escape str chars #!optional (escape-char #\\))
  (strings-join (string-split str
			      (let ((cs (cons escape-char chars)))
				(lambda (c)
				  (memq c cs)))
			      #t)
		(string escape-char)))

(TEST
 > (string-escape "Foo|bar" '(#\x))
 "Foo|bar"
 > (string-escape "Foo|bar" '(#\|))
 "Foo\\|bar"
 > (string-escape "|Foo||b\\ar|" '(#\|))
 "\\|Foo\\|\\|b\\\\ar\\|")


(define (first-line str)
  (let ((len (string-length str)))
    (let lp ((i 0))
      (if (< i len)
	  (let ((c (string-ref str i)))
	    (case c
	      ((#\newline #\return) (substring str 0 i))
	      (else (lp (+ i 1)))))
	  str))))

(TEST
 > (first-line "foo")
 "foo"
 > (first-line "foo\n")
 "foo"
 > (first-line "foo\nbar")
 "foo")

