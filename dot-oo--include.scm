;; included in dot-oo, hence no require form. pseudo export form:
(export dot-oo:method-key-maybe-ref-i
	dot-oo:method-table-set!
	dot-oo:new-method-table)


;; Method tables consist of a vector in a box (so that replaceable) in
;; the format (4 is a faster divisor than 3):

;; (vector prefix1 prefix2 prefix3
;;         pred1 pred2 pred3
;;         method1 method2 method3
;;         reserved reserved reserved)


;; XX move?

;; should allow anything that works for vector access. Then could do
;; unsafe. What about wrap-around?
;; > (fx+ max-fixnum 1)
;; *** ERROR IN (console)@17.1 -- FIXNUM overflow
;; (fx+ 2305843009213693951 1)
;; OK.
(define (fx.inc n)
  (fx+ n 1))


(define (vector-copy! fromvec from to tovec tofrom)
  (let* ((offset (fx- tofrom from)))
    (for..< (i from to)
	    (vector-set! tovec (fx+ i offset)
			 (vector-ref fromvec i)))))

(TEST
 > (define v0 (vector 'a 'b 'c 'd))
 > (define v1 (vector 'x 'y 'z))
 > (vector-copy! v1 1 3 v0 1)
 > v0
 #(a y z d))

;; /move


(define (dot-oo:method-key-maybe-ref-i vec n key)
  (let ((end n))
    (let lp ((i 0))
      (if (< i end)
	  (if (eq? (vector-ref vec i) key)
	      i
	      (lp (fx.inc i)))
	  #f))))

(define (dot-oo:method-type-maybe-ref-method vec n obj)
  (declare (not safe))
  (let ((end (fx+ n n)))
    (let lp ((i n))
      (if (fx< i end)
	  (if
	   ( ;; XX should function call be safe here?
	    (vector-ref vec i)
	    obj)
	   (vector-ref vec (fx+ i n))
	   (lp
	    ;; (fx.inc i) but unsafely here:
	    (fx+ i 1)))
	  #f))))

(define (dot-oo:method-table-maybe-ref-method tbl obj)
  (let* ((vec (unbox tbl))
	 (n (arithmetic-shift (vector-length vec) -2)))
    (dot-oo:method-type-maybe-ref-method vec n obj)))


;; Disable all assertments for production mode? Can we have full testing instead?
(define-typed (dot-oo:copy-rail! raili
				 old #(natural0? oldn)
				 new #(natural0? newn)
				 ;; and these are indexes within an individual rail:
				 #(natural0? oldfrom)
				 #(natural0? oldto)
				 #(natural0? newfrom))
  (assert (<= oldto oldn))
  (assert (<= (fx+ newfrom (fx- oldto oldfrom)) newn))
  (assert (<= oldfrom oldto)) ;; XX could relax?
  (let ((oldbase (fx* raili oldn)))
    (vector-copy! old
		  (fx+ oldbase oldfrom)
		  (fx+ oldbase oldto)
		  new
		  (fx+ (fx* raili newn) newfrom))))

(define (dot-oo:copy-rail raili old oldn new newn
			  oldfrom oldto newfrom)
  (let ((new (vector-copy new)))
    (dot-oo:copy-rail! raili old oldn new newn
		       oldfrom oldto newfrom)
    new))


(TEST
 > (define orig '#(baz 0 0 0 0 0 0 0 0 0 0 0))
 > (dot-oo:copy-rail
    0
    '#(a foo baz a? foo? baz? a.bar foo.bar baz.bar 0 0 0)
    3
    orig
    3
    0
    1
    1)
 ;; rail 0 is [a foo baz]. 0..1 is a, out 1 
 #(baz a 0 0 0 0 0 0 0 0 0 0)
 > (dot-oo:copy-rail
    0
    '#(a foo baz a? foo? baz? a.bar foo.bar baz.bar 0 0 0)
    3
    orig
    3
    0
    2
    1)
 #(baz a foo 0 0 0 0 0 0 0 0 0)
 > (%try (dot-oo:copy-rail
	  0
	  '#(a foo baz a? foo? baz? a.bar foo.bar baz.bar 0 0 0)
	  3
	  orig
	  3
	  0
	  2
	  2))
 (exception text: "assertment failure: (<= (fx+ newfrom (fx- oldto oldfrom)) newn) (<= (fx+ 2 (fx- 2 0)) 3)\n")
 > (dot-oo:copy-rail
    1
    '#(a foo baz a? foo? baz? a.bar foo.bar baz.bar 0 0 0)
    3
    orig
    3
    0
    2
    1)
 #(baz 0 0 0 a? foo? 0 0 0 0 0 0)
 > (dot-oo:copy-rail
    2
    '#(a foo baz a? foo? baz? a.bar foo.bar baz.bar 0 0 0)
    3
    orig
    3
    0
    2
    1)
 #(baz 0 0 0 0 0 0 a.bar foo.bar 0 0 0)
 > (dot-oo:copy-rail
    3
    '#(a foo baz a? foo? baz? a.bar foo.bar baz.bar 0 0 0)
    3
    orig
    3
    0
    2
    1)
 #(baz 0 0 0 0 0 0 0 0 0 0 0)
 > (%try (dot-oo:copy-rail
	  4
	  '#(a foo baz a? foo? baz? a.bar foo.bar baz.bar 0 0 0)
	  3
	  orig
	  3
	  0
	  2
	  1))
 (exception
  text:
  "(Argument 2) Out of range\n(vector-ref '#(a foo baz a? foo? baz? a.bar foo.bar baz.bar 0 0 0) 12)\n"))



(both-times
 (define (nonempty-symbol? v)
   (and (symbol? v)
	(not (zero? (string-length (symbol->string v)))))))

(define-typed (dot-oo:method-table-set old #(nonempty-symbol? prefix) pred meth)

  (define (finish new n)
    (vector-set! new 0 prefix)
    (vector-set! new n pred)
    (vector-set! new (fx+ n n) meth)
    new)

  (let* ((oldlen (vector-length old))
	 (oldn (arithmetic-shift oldlen -2)))
    (cond ((dot-oo:method-key-maybe-ref-i old oldn prefix)
	   => (lambda (oldi)
		(let* ((n oldn)
		       (new (make-vector oldlen)))

		  (define (do-rail raili)
		    (let ((oldi* (fx.inc oldi)))
		      ;; copy part after found position (unshifted):
		      (dot-oo:copy-rail! raili old oldn new n
					 oldi* oldn oldi*)
		      ;; copy part before found position towards the end
		      ;; to make space for new row at the beginning:
		      (dot-oo:copy-rail! raili old oldn new n 0 oldi 1)))

		  (do-rail 0)
		  (do-rail 1)
		  (do-rail 2)
		  ;; (don't bother about reserved fields)
		  (finish new n))))
	  (else
	   ;; prepend to the top
	   (let* ((n (fx.inc oldn))
		  (new (make-vector (arithmetic-shift n 2))))
	     (dot-oo:copy-rail! 0 old oldn new n 0 oldn 1)
	     (dot-oo:copy-rail! 1 old oldn new n 0 oldn 1)
	     (dot-oo:copy-rail! 2 old oldn new n 0 oldn 1)
	     ;; (don't bother about reserved fields)
	     (finish new n))))))

(TEST
 > (dot-oo:method-table-set (vector) 'foo 'foo? 'foo.bar)
 #(
   foo
   foo?
   foo.bar
   0)
 > (dot-oo:method-table-set # 'baz 'baz? 'baz.bar)
 #(
   baz foo
   baz? foo?
   baz.bar foo.bar
   0 0)
 > (dot-oo:method-table-set # 'foo 'foo? 'foo.bar)
 #(
   foo baz
   foo? baz?
   foo.bar baz.bar
   0 0)
 > (define a
     (dot-oo:method-table-set # 'a 'a? 'a.bar))
 > a
 #(
   a foo baz
   a? foo? baz?
   a.bar foo.bar baz.bar
   0 0 0)
 > (dot-oo:method-table-set a 'foo 'foo? 'foo.bar)
 #(
   foo a baz
   foo? a? baz?
   foo.bar a.bar baz.bar
   0 0 0)
 > (dot-oo:method-table-set a 'baz 'baz? 'baz.bar)
 #(
   baz a foo
   baz? a? foo?
   baz.bar a.bar foo.bar
   0 0 0))


(define (dot-oo:method-table-set! tbl prefix pred meth)
  (set-box! tbl (dot-oo:method-table-set (unbox tbl) prefix pred meth))
  ;; still return it to allow for easier 'define-once' approach:
  tbl)


(define (dot-oo:new-method-table)
  (box '#()))


(define (dot-oo:show-method-table t)
  (let* ((v (unbox t))
	 (n (arithmetic-shift (vector-length v) -2)))
    (unfold (C >= _ n)
	    (lambda (i)
	      (list (vector-ref v i)
		    (vector-ref v (+ i n))
		    (vector-ref v (+ i (* 2 n)))))
	    inc 0)))
