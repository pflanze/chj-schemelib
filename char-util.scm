;;; Copyright 2013-2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require C
	 ;; (cj-functional either) circular dependency
	 (list-util let-pair) ;; for either
	 (fixnum inc)
	 test)

(export char=?/ ;; ?
	char-one-of?/
	char-digit?
	char-alpha-lc?
	char-alpha-uc?
	char-alpha?
	char-alphanumeric?
	char-numeric+?
	char-alphanumeric+?
	char-whitespace?
	char-in-range?
	char-hexdigit?
	
	#!optional
	on-char)

;;XX to avoid circular dependency on cj-functional
(define (either . fs)
  (if (null? fs)
      (lambda x
	#f)
      (let-pair ((f fs*) fs)
		((lambda (r)
		   (lambda x
		     (or (apply f x)
			 (apply r x))))
		 (apply either fs*)))))



(define (char=?/ c)
  (C char=? _ c))

(define (char-one-of?/ str)
  (let ((strlen (string-length str)))
    (lambda (c)
      (let lp ((i 0))
	(and (< i strlen)
	    (or (char=? c (string-ref str i))
		(lp (inc i))))))))

(TEST
 > ((char-one-of?/ "Halo") #\_)
 #f
 > ((char-one-of?/ "Halo") #\o)
 #t
 > ((char-one-of?/ "Halo") #\O)
 #f
 )


(define (on-char fn)
  (lambda (c)
    (fn (char->integer c))))

(define char-digit?
  (on-char (C <= (char->integer #\0) _ (char->integer #\9))))

(define char-alpha-lc?
  (on-char (C <= (char->integer #\a) _ (char->integer #\z))))

(define char-alpha-uc?
  (on-char (C <= (char->integer #\A) _ (char->integer #\Z))))

(define char-alpha?
  (either char-alpha-lc? char-alpha-uc?))

(define char-alphanumeric?
  (either char-digit? char-alpha? (char=?/ #\_)))

(define char-numeric+?
  ;; f and d are synonyms for e in exponent notation in Scheme?
  (either char-digit? (char-one-of?/ "+-.defi")))

(define char-alphanumeric+?
  (either char-alpha? (char=?/ #\_) char-numeric+?))


(TEST
 > (every char-alphanumeric? (string->list "abc "))
 #f
 > (every char-alphanumeric? (string->list "abc_123_A"))
 #t
 )

;; XX unify with |u8-whitespace?| (but make it a dependency or? Only
;; have those once please, really. Make u8* a dependency on us,
;; rather?)
(define (char-whitespace? char)
  (case char
    ((#\space #\newline #\tab #\page #\return)
     #t)
    (else #f)))


(define (char-in-range? fromchar tochar)
  (let ((from (char->integer fromchar))
	(to (char->integer tochar)))
    (lambda (v)
      (<= from (char->integer v) to))))

(define char-hexdigit?
  (either char-digit?
	  (char-in-range? #\a #\f)
	  (char-in-range? #\A #\F)))

(TEST
 > (char-hexdigit? #\x)
 #f
 > (char-hexdigit? #\f)
 #t
 > (char-hexdigit? #\F)
 #t
 > (char-hexdigit? #\9)
 #t
 )
