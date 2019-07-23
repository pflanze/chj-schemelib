;;; Copyright 2010-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require fixnum
	 test
	 list-util
	 srfi-11)


(export shell-quote
	perl-quote
	singlequote-javascript
	write-string)


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
 > (shell-quote "ab")
 "'ab'"
 > (shell-quote "a b")
 "'a b'"
 > (shell-quote "a 'b")
 "'a '\\''b'"
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


(define singlequote-javascript perl-quote) ;; XXX really correct?


;; (define (scheme-quote str)
;;   ;; unlike |write| or |object->string|, this doesn't turn non-ascii
;;   ;; characters into unicode escape sequences.
;;   )

;; unlike |write|, this doesn't turn non-ascii characters into unicode
;; escape sequences.
(define (write-string v p)
  (let ((len (string-length v)))
    (display "\"" p)
    (let lp ((i 0))
      (when (< i len)
            (let ((c (string-ref v i)))
              (case c
                ((#\" #\\)
                 (display #\\ p)
                 (display c p))
                ((#\newline)
                 (display "\n" p))
                ((#\return)
                 (display "\r" p))
                ((#\tab)
                 (display "\t" p))
                (else
                 ;; XX well, probably not even faster than doing a list
                 ;; based scheme-quote huh. locking and all. Stupid.
                 (display c p)))
              (lp (inc i)))))
    (display "\"" p)))
