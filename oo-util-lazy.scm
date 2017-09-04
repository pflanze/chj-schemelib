
(require easy-1
	 dot-oo ;; included in easy?
	 ;; oo-util ;; ?
	 (cj-source-quasiquote quasiquote-source)
	 stream ;; (only optionally? (lazily? well.))
	 (code-map code-map-substrings)
	 (oo-vector-lib sum)
	 )

;; Methods on lazy data

(insert-result-of
 `(begin
    ,@(map (lambda (method-name)
	     (quasiquote-source
	      (define. (,(source:symbol-append 'promise method-name) v)
		(,method-name (force v)))))
	   '(
	     .length ;; vs. stream.length which only works on streams
	     .car
	     .cdr
	     .first
	     .rest
	     ;; .cadr
	     ;; .cddr  ah won't work, need to dispatch to deeper-forcing ones
	     ;; .ref ;; hmm there is a list.ref, will be slow
	     ;;.equal nope, instead stream.equal? can work
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
;; stream.list (which should have, and has, priority,
;; hence, this one is only called for non-lazy inputs).


;; lazy-to-lazy methods:

(define (stream? v)
  ;; *only* for lazy inputs
  (and (promise? v)
       (pair-or-null? (force v))))


;; XX should we just fall back on one generic method (hey, like in
;; fperl!), that forces then dispatches to the eager method, I mean,
;; then signalizes that it is to return a lazy result? I.e. dispatch
;; to a different namespace?)


;; for easy matching below. Consistency, you know..
(define list-drop drop)
(define list-take take)
(define list-sublist sublist)
(define list-length length)
(define list-sum sum)
(define list-append append)
(define list-append/2 append)
;;(define list-xone xone)
(define list-min&max stream-min&max) ;; XX add non-forcing instead? consistency forever? evil now
(define list-min stream-min)
(define list-max stream-max)

(define list-map map)
;;(define list-filter filter) ah, no
(define (list-filter pred lis #!optional (tail '()))
  (let rec ((l lis))
    (if (pair? l)
	(let-pair ((a l*) l)
		  (If (pred a)
		      (cons a (rec l*))
		      (rec l*)))
	tail)))

(define list-filter-map filter-map)

(define list-first first)
(define list-second second)
(define list-third third)
(define list-fourth fourth)
(define list-fifth fifth)
(define list-sixth sixth)
(define list-seventh seventh)
(define list-eighth eighth)
(define list-ninth ninth)
(define list-tenth tenth)

(code-map-substrings
 ((lazy- '(lazy- ||))
  (stream- '(stream- list-)))
 (begin

   (define. (stream.filter/tail s pred tail)
     (stream-filter/tail pred s tail))

   (define. (stream.for-each s proc . ss)
     (apply stream-for-each proc s ss))

   (define. (stream.fold-right l fn start)
     (stream-fold-right fn start l))

   ;; stream:fold-right  -- hmm, bad name anyway

   (define. (stream.map/tail s func tail)
     (stream-map/tail func s tail))

   (define. (stream.map s f . ss)
     (apply stream-map f s ss))

   (define. (stream.filter-map s f . ss)
     (apply stream-filter-map f s ss))

   (define. (stream.improper-map func s)
     (stream-improper-map func s))

   (define. (stream.list s)
     (stream->list s))

   (define. stream.drop
     stream-drop)

   (define. stream.take stream-take)

   (define. stream.sublist stream-sublist)

   (define. stream.length stream-length) ;; XX should really be a stream-improper-length !

   (define. stream.difference stream-difference)

   (define. stream.show-difference show-stream-difference)

   (define. stream.equal? stream-equal?)

   (define. (stream.filter s pred #!optional (tail '()))
     (stream-filter pred s tail))

   (define. (stream.fold l fn start)
     (stream-fold fn start l))

   (define. stream.append-optimized stream-append-optimized)

   (define. stream.append/2 stream-append/2)

   (define. stream.append stream-append)

   (define. (stream.union s less? . ss)
     (apply stream-union less? s ss))

   (define. (stream.uniq s equal? #!optional (tail '()))
     (stream-uniq equal? s tail))

   (define. (stream.uniq-count s equal? #!optional (tail '()))
     (stream-uniq-count equal? s tail))

   (define. (stream.cmp-union s cmp . ss)
     (apply cmp-stream-union cmp s ss))

   (define. (stream.group s equal? #!optional (tail '()))
     (stream-group equal? s tail))

   (define. (stream.cmp-group s cmp #!optional (tail '()))
     (cmp-stream-group cmp s tail))

   (define. (stream.chop/map s n f #!optional (tail '()))
     (stream-chop/map n s f tail))

   (define. (stream.chop n s #!optional (tail '()))
     (stream-chop n s tail))

   (define. (stream.zip s . ss)
     (apply stream-zip s ss))

   (define. (stream.zip2 s1 s2)
     (stream-zip2 s1 s2))

   (define. (stream.drop-while l pred)
     (stream-drop-while pred l))

   (define. stream.ref stream-ref)

   (define. (stream.every lis1 pred . lists)
     (apply stream-every pred lis1 lists))

   (define. stream.min&max stream-min&max)
   ;; are these evil, since |min| and |max| assume n-ary interfaces?:
   ;; (At least it should be unambiguous though as only work on
   ;; numbers, not lists, as elements.)
   (define. stream.min stream-min)
   (define. stream.max stream-max)

   (define. (stream.map/iota lis fn)
     (stream-map/iota fn lis))

   (define. stream.sum stream-sum)

   ;; srfi-1

   (define. stream.second stream-second)
   (define. stream.third stream-third)
   (define. stream.fourth stream-fourth)
   (define. stream.fifth stream-fifth)
   (define. stream.sixth stream-sixth)
   (define. stream.seventh stream-seventh)
   (define. stream.eighth stream-eighth)
   (define. stream.ninth stream-ninth)
   (define. stream.tenth stream-tenth)
   ))


(TEST
 > (.car (.cdr (stream-iota 10)))
 1
 > (.sum (iota 10))
 45
 > (.sum (stream-iota 10))
 45
 > (.sum (.map (iota 10) inc))
 55
 > (.sum (.map (stream-iota 10) inc))
 55
 > (.max '(1 9 4))
 9
 > (.max (stream-iota 10 5))
 14

 ;; add more extensive testing..
 )


