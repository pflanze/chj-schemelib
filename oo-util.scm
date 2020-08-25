;;; Copyright 2013-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require dot-oo
	 (cj-math integer)
	 (string-util-2 string-reverse
                        string-findpos
                        string-rfindpos)
	 (cj-functional list-of)
	 (values values->vector values->list letv)
	 cj-env
	 show
	 (cj-source show-source-location show-location-location
		    show-procedure-location)
	 debuggable-promise
	 string-util-1
	 string-util-2
	 ;; string-util-3 -- cycle
	 string-util-4)

(possibly-use-debuggable-promise)


(define. (any.cons s v)
  (cons v s))

(define inexact exact->inexact)
;; exact? is not a proper predicate (OK?), it throws exceptions for
;; non-numbers, so don't define. the following:
(define exact.inexact exact->inexact)
;; This is fine:
(define. number.inexact exact->inexact)

(define. values.vector values->vector)

(define. values.list values->list)

(define. number.string number->string)
(define. string.maybe-number string->number)

(define. (string.number s)
  (or (string->number s)
      (error "not a number string:" s)))

(TEST
 > (%try-error (string.number "0 "))
 #(error "not a number string:" "0 ")
 > (string.maybe-number "0 ")
 #f)


(define. symbol.string symbol->string)
(define. string.symbol string->symbol)

(define. keyword.string keyword->string)
(define. string.keyword string->keyword)

(define. keyword.symbol keyword->symbol)
(define. symbol.keyword symbol->keyword)

(define char-list? (list-of char?))

(define. char-list.string list->string)

(define char-list+? (both pair? (list-of char?)))

(define. (char-list+.show l show)
  `(.list ,(char-list.string l)))

(define. string.list string->list)

;; XX where to move these? don't want to depend on stream.scm?
(define (string->stream s)
  (let ((len (string-length s)))
    (let rec ((i 0))
      (delay
	(if (< i len)
	    (cons (string-ref s i)
		  (rec (fx+ i 1)))
	    '())))))

(define. string.stream string->stream)

(define. char.integer char->integer)
(define. integer.char integer->char)

(define. string.append string-append)

(define. string.length string-length)

(define. integer.length integer-length)

(define. string.reverse string-reverse)

(define. string.findpos string-findpos)
(define. string.rfindpos string-rfindpos)


(TEST
 > (.string 234)
 "234"
 > (.integer #\f)
 102
 ;; hh interesting: actually *can't* avoid the dot notation?
 > (string #\f)
 "f"
 > (integer 3.3)
 3
 ;; or well I could?
 ;; but:
 > (.list "ab")
 (#\a #\b)
 > (list "ab")
 ("ab")
 ;; ^ special because it's a vararg 'function' ?
 ;; ah yeah?: tuple->list ? ? args->list. hm.
 ;; but args doesn't work; when getting 1 and should dispatch on it.
 )

;; could use ->list or ->string as the generics names. hm  ?

(TEST
 > (.append "a" "b")
 "ab")

;;a way to avoid conflicts in any case, yeah: I mean, just use the
;;.append name globally with that; no issue when wanting to use it in
;;future again for sth different.


(define. source.symbol-append source:symbol-append)
;; well, really a case where I'd want it to de-source on *any*
;; argument. lol, wrappers
(define. symbol.symbol-append source:symbol-append)
(define. string.symbol-append source:symbol-append)


(define. (any.maybe-location v)
  (error "no .maybe-location method defined for:" v))

(define. location.maybe-location identity)
(define. source.maybe-location source-location)
(define. (procedure.maybe-location [procedure? p])
  (##procedure-locat p))

(define. (any.location v)
  (or (.maybe-location v)
      (error "does not contain location information:" v)))

(define. any.show-location
  (compose show-location-location .location))

;; (define. location.show-location show-location-location) use any.show-location
(define. source.show-location show-source-location)
(define. procedure.show-location show-procedure-location)


;; make |show-def| more general:
(current-show-def .show-location)
;; XX hmm, rename show-def to show-location, though? This now prefers
;; to show input location over source location or monad context for
;; parse1 failures.

;; ------------------------------------------
;; Check values for truthness the Perl way:

(define. (any.perl-true? x)
  #t)

(define number-zero? (both number? zero?))

(define. (number-zero.perl-true? x)
  #f)

(define. (false.perl-true? x)
  #f)

(define. (string.perl-true? str)
  (cond ((%string-empty? str) #f)
	;; calc> :l if ("0 ") { "ja" } else { "nein" }
	;; ja
	;; calc> :l if ("00") { "ja" } else { "nein" }
	;; ja
	;; calc> :l if ("0") { "ja" } else { "nein" }
	;; nein
	((string=? str "0") #f)
	(else
	 #t)))


(TEST
 > (.perl-true? "")
 #f
 > (.perl-true? "0")
 #f
 > (.perl-true? "1")
 #t
 > (.perl-true? "0E0")
 #t
 > (.perl-true? "0 but true")
 #t
 > (.perl-true? '||)
 #t
 > (.perl-true? 3)
 #t
 > (.perl-true? 0)
 #f
 > (.perl-true? 0.)
 #f
 )

;; ------------------------------------------


(insert-result-of
 `(begin
    ,@(map (lambda (sym)
	     (define (minus-to-dot str)
	       (letv ((function-start function-end) (string-split-once str #\- #t))
		     (string-append function-start
				    "."
				    function-end)))
	     `(define. ,(string->symbol (minus-to-dot (symbol->string sym))) ,sym))
	   '(
	     ;; string-util-1
	     string-split

	     ;; string-util-2
	     string-trim-left
	     string-trim-right
	     string-trimlines-right
	     string-multiply
	     string-starts?
	     string-starts-ci?
	     string-contains
	     string-contains-ci
	     string-contains?
	     string-contains-ci?
	     string-find-char
	     string-rfind-char
	     string-split-1 ;; vs. string-split-once ?
	     string-split-once
	     ;;	string-reverse
	     ;;	string-map
	     string-any
	     string-downcase string-lc
	     string-upcase string-uc
	     string-upcase-first string-ucfirst
	     string-pad-left
	     string-ends-with?
	     string-starts-with? ;;XX vs string-starts? ?

	     ;; string-util-3
	     ;;string-contains-char? -- can't because of dependency cycle
	     ;; string.replace-substring
	     ;; string.replace-substring-ci
	     ;; string.replace-substrings
	     ;; string.replace-substrings-ci
	     ;; string.maybe-replace-substring
	     ;; string.maybe-replace-substring-ci
	     ;; string.maybe-replace-substrings
	     ;; string.maybe-replace-substrings-ci
	     ;; string.drop
	     ;; string.take
	     ;; string.any
	     ;; string.natural0
	     ;; string.natural
 
	     ;; string-util-4
	     string-empty?
	     string-every
	     string-first-line

	     ))))
