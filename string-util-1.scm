;;; Copyright 2010, 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require test
	 ;;(list-util-1 list-split)
	 )

(export string-contains-char?
	string-split)

(include "cj-standarddeclares.scm")


(define (char-or-pred.pred char-or-pred)
  (cond ((char? char-or-pred)
         (lambda (c)
           (eq? c char-or-pred)))
        ((procedure? char-or-pred)
         char-or-pred)
        (else
         (error "expecting char or pred:" char-or-pred))))


(define (string-contains-char? str char-or-pred)
  (let ((len (string-length str))
        (pred (char-or-pred.pred char-or-pred)))
    (let lp ((i 0))
      (and (< i len)
	   (or (pred (string-ref str i))
	       (lp (inc i)))))))

(TEST
 > (string-contains-char? "Hello" char-newline?)
 #f
 > (string-contains-char? "Hello\n" char-newline?)
 #t
 )


;; (define (string-split str char-or-pred)
;;   (map list->string (list-split (string->list str) char-or-pred)))

(define (string-split str char-or-pred #!optional retain-matches?)
  (let ((len (string-length str))
	(pred (char-or-pred.pred char-or-pred)))
    (let lp ((i (dec len))
	     (prev-position len)
	     (strs '()))
      (if (>= i 0)
	  (if (pred (string-ref str i))
	      (lp (dec i)
		  i
		  (cons (substring str
				   (if retain-matches?
				       i
				       (inc i))
				   prev-position)
			strs))
	      (lp (dec i)
		  prev-position
		  strs))
	  (cons (substring str 0 prev-position) strs)))))

(TEST
 > (string-split "Foo|bar|baz|" #\x)
 ("Foo|bar|baz|")
 > (string-split "Foo|bar|baz|" #\|)
 ("Foo" "bar" "baz" "")
 > (string-split "|bar|baz|" #\|)
 ("" "bar" "baz" "")
 > (string-split "||baz|" #\|)
 ("" "" "baz" "")
 > (string-split "||baz|" (lambda (c) (case c ((#\| #\a) #t) (else #f))))
 ("" "" "b" "z" "")
 > (string-split "||baz|" (lambda (c) (case c ((#\| #\a) #t) (else #f))) #t)
 ("" "|" "|b" "az" "|"))

