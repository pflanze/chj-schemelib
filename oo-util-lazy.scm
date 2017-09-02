
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


;; for easy matching below. Consistency, you know..
(define list-drop drop)
(define list-take take)
(define list-sublist sublist)
(define list-length length)
(define list-sum sum)
(define list-append append)
(define list-append/2 append)
(define list-xone xone)
(define list-min&max stream-min&max) ;; XX add non-forcing instead? consistency forever? evil now
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

   (define. (lazy-pair-or-null.filter-map s f . ss)
     (apply stream-filter-map f s ss))

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

   (define. (lazy-pair-or-null.every lis1 pred . lists)
     (apply stream-every pred lis1 lists))

   (define. lazy-pair-or-null.min&max stream-min&max)

   (define. (lazy-pair-or-null.map/iota lis fn)
     (stream-map/iota fn lis))

   (define. lazy-pair-or-null.sum stream-sum)

   ;; srfi-1

   (define. lazy-pair-or-null.second stream-second)
   (define. lazy-pair-or-null.third stream-third)
   (define. lazy-pair-or-null.fourth stream-fourth)
   (define. lazy-pair-or-null.fifth stream-fifth)
   (define. lazy-pair-or-null.sixth stream-sixth)
   (define. lazy-pair-or-null.seventh stream-seventh)
   (define. lazy-pair-or-null.eighth stream-eighth)
   (define. lazy-pair-or-null.ninth stream-ninth)
   (define. lazy-pair-or-null.tenth stream-tenth)
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
 ;; add more extensive testing..
 )


