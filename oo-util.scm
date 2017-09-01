
(require easy-1
	 dot-oo ;; included in easy?
	 (cj-math integer)
	 (string-util-2 string-reverse)
	 (cj-functional list-of)
	 (srfi-11 values->vector values->list) ;; included in easy?
	 cj-env
	 (cj-source-quasiquote quasiquote-source)
	 cj-stream ;; (only optionally? (lazily? well.))
	 )


(define inexact exact->inexact)
(define. exact.inexact exact->inexact) ;; ah, use the above?

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


(define. source.symbol-append source:symbol-append)
;; well, really a case where I'd want it to de-source on *any*
;; argument. lol, wrappers
(define. symbol.symbol-append source:symbol-append)
(define. string.symbol-append source:symbol-append)


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

;; Lazy:

(insert-result-of
 `(begin
    ,@(map (lambda (method-name)
	     (quasiquote-source
	      (define. (,(source:symbol-append 'promise method-name) v)
		(,method-name (force v)))))
	   '(
	     .length ;; vs. lazy-pair-or-null.length which only works on streams
	     .car
	     .cdr
	     .first
	     .rest
	     ;; .cadr
	     ;; .cddr  ah won't work, need to dispatch to deeper-forcing ones
	     ;; .ref ;; hmm there is a list.ref, will be slow
	     ;;.equal nope, instead lazy-pair-or-null.equal? can work
	     ))))

;; methods after forcing:

(define. pair-or-null.length improper-length)
;;(define. pair-or-null.list identity) ;; well... dangerous.

(define. pair.car car)
(define. pair.cdr cdr)
(define. pair.first first)
(define. pair.rest rest)
;; (define. pair.)
;; (define. pair.)

(define. list.list identity)
;; ^ but actual forcing of a stream happens by dispatching to
;; lazy-pair-or-null.list (which should have, and has, priority,
;; hence, this one is only called for non-lazy inputs).


;; lazy-to-lazy methods:

(define (lazy-pair-or-null? v)
  ;; *only* for lazy inputs
  (and (promise? v)
       (pair-or-null? (force v))))


;; XX should we just fall back on one generic method (hey, like in
;; fperl!), that forces then dispatches to the eager method, I mean,
;; then signalizes that it is to return a lazy result? I.e. dispatch
;; to a different namespace?)

(define. (lazy-pair-or-null.filter/tail s pred tail)
  (stream-filter/tail pred s tail))

(define. (lazy-pair-or-null.for-each s proc . ss)
  (apply stream-for-each proc s ss))

(define. (lazy-pair-or-null.fold-right l fn start)
  (stream-fold-right fn start l))

;; stream:fold-right  -- hmm, bad name anyway

(define. (lazy-pair-or-null.map/tail s func tail)
  (stream-map/tail func s tail))

(define. (lazy-pair-or-null.map s f . ss)
  (apply stream-map f s ss))

(define. (lazy-pair-or-null.improper-map func s)
  (stream-improper-map func s))

(define. (lazy-pair-or-null.list s)
  (stream->list s))

(define. lazy-pair-or-null.drop
  stream-drop)

(define. lazy-pair-or-null.take stream-take)

(define. lazy-pair-or-null.sublist stream-sublist)

(define. lazy-pair-or-null.length stream-length) ;; XX should really be a stream-improper-length !

(define. lazy-pair-or-null.difference stream-difference)

(define. lazy-pair-or-null.show-difference show-stream-difference)

(define. lazy-pair-or-null.equal? stream-equal?)

(define. (lazy-pair-or-null.filter s pred #!optional (tail '()))
  (stream-filter pred s tail))

(define. (lazy-pair-or-null.fold l fn start)
  (stream-fold fn start l))

(define. lazy-pair-or-null.append-optimized stream-append-optimized)

(define. lazy-pair-or-null.append/2 stream-append/2)

(define. lazy-pair-or-null.append stream-append)

(define. (lazy-pair-or-null.union s less? . ss)
  (apply stream-union less? s ss))

(define. (lazy-pair-or-null.uniq s equal? #!optional (tail '()))
  (stream-uniq equal? s tail))

(define. (lazy-pair-or-null.uniq-count s equal? #!optional (tail '()))
  (stream-uniq-count equal? s tail))

(define. (lazy-pair-or-null.cmp-union s cmp . ss)
  (apply cmp-stream-union cmp s ss))

(define. (lazy-pair-or-null.group s equal? #!optional (tail '()))
  (stream-group equal? s tail))

(define. (lazy-pair-or-null.cmp-group s cmp #!optional (tail '()))
  (cmp-stream-group cmp s tail))

(define. (lazy-pair-or-null.chop/map s n f #!optional (tail '()))
  (stream-chop/map n s f tail))

(define. (lazy-pair-or-null.chop n s #!optional (tail '()))
  (stream-chop n s tail))

(define. (lazy-pair-or-null.zip s . ss)
  (apply stream-zip s ss))

(define. (lazy-pair-or-null.zip2 s1 s2)
  (stream-zip2 s1 s2))

(define. (lazy-pair-or-null.drop-while l pred)
  (stream-drop-while pred l))

(define. lazy-pair-or-null.ref stream-ref)

(define. lazy-pair-or-null.xone stream-xone)
;; ^ you really want to use xxone though, which I don't have for
;; stream! XX todo. xone is evil.

(define. (lazy-pair-or-null.every lis1 pred . lists)
  (apply stream-every pred lis1 lists))

(define. lazy-pair-or-null.min&max stream-min&max)

(define. (lazy-pair-or-null.map/iota lis fn)
  (stream-map/iota fn lis))

(define. lazy-pair-or-null.sum stream-sum)

