;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (lib.test)
	 (lib.list-util)
	 (lib.srfi-11))


;;;
;;;; string quoting
;;;

(define (shell-quote str)
  (list->string
   (cons #\'
	 (let rec ((l (string->list str)))
	   (if (null? l)
	       '(#\')
	       (let-pair
		((c l*) l)
		(case c
		  ((#\') (cons #\'
			       (cons #\\
				     (cons #\'
					   (cons #\'
						 (rec l*))))))
		  (else
		   (cons c (rec l*))))))))))

(TEST
 > (shell-quote "")
 "''"
 > (shell-quote "he'llo")
 "'he'\\''llo'"
 > (shell-quote "'")
 "''\\'''"
 )

(define (perl-quote str)
  (list->string
   (cons #\'
	 (let rec ((l (string->list str)))
	   (if (null? l)
	       '(#\')
	       (let-pair
		((c l*) l)
		(case c
		  ((#\') (cons #\\
			       (cons #\'
				     (rec l*))))
		  ((#\\) (cons #\\
			       (cons #\\
				     (rec l*))))
		  (else
		   (cons c (rec l*))))))))))

(TEST
 > (perl-quote "")
 "''"
 > (perl-quote "he'llo")
 "'he\\'llo'"
 > (perl-quote "'")
 "'\\''"
 > (perl-quote "hel\\o")
 "'hel\\\\o'"
 )
