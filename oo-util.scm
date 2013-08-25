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

