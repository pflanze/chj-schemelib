(define false? not) ;; so as to be able to use "false." as OO prefix
(define (anything? x) #t)


(define. number.string number->string)
(define. symbol.string symbol->string)
(define. list.string list->string) ;;hm not in general? but hm 'actually' ok? not?
(define. list.u8vector list->u8vector)

;;XX better place in a lib [odd, why not have already?]
(define string->u8vector (compose* list->u8vector
				   (cut map char->integer <>)
				   string->list))
(define. string.u8vector string->u8vector)

(define. string.list string->list)

(define. char.integer char->integer)

(define. string.append string-append)

(define. string.number string->number)

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
 "ab"
 > (.u8vector (map .integer (.list "foo")))
 #u8(102 111 111)
 )

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

