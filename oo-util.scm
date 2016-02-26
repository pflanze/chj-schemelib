(require easy
	 dot-oo;; included in easy?
	 )

(define inexact exact->inexact)
(define. exact.inexact exact->inexact) ;; ah, use the above?

(define. values.vector values->vector)

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

(define. (keyword.symbol v)
  (string.symbol (keyword.string v)))
(define. (symbol.keyword v)
  (string.keyword (symbol.string v)))

(define. list.string list->string) ;;hm not in general? but hm 'actually' ok? not?

(define. string.list string->list)

(define. char.integer char->integer)
(define. integer.char integer->char)

(define. string.append string-append)

(define. string.length string-length)

(define. string.reverse string-reverse)

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


;; hm wrappers hm. (lost)


(define (source-symbol-append . vals)
  (apply symbol-append (map source-code vals)))

(define. source.symbol-append source-symbol-append)
;; well, really a case where I'd want it to de-source on *any*
;; argument. lol, wrappers
(define. symbol.symbol-append source-symbol-append)
(define. string.symbol-append source-symbol-append)


;; ------------------------------------------
;; Check values for truthness the Perl way:

(define. (anything.perl-true? x)
  #t)

(define number-zero? (both number? zero?))

(define. (number-zero.perl-true? x)
  #f)

(define. (false.perl-true? x)
  #f)

(define. (string.perl-true? str)
  (cond ((string-empty? str) #f)
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

