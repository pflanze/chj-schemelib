(require test easy)

(def (sum nums)
     (fold + 0 nums))

(code-map-substrings
 ((VECTOR '(
	    string
	    vector
	    f32vector
	    f64vector
	    u8vector s8vector
	    u16vector s16vector
	    u32vector s32vector
	    u64vector s64vector)))
 
 (begin
   (def. VECTOR.ref VECTOR-ref)
   (def. VECTOR.set! VECTOR-set!)
   (def. VECTOR.length VECTOR-length)
   (IF (not (eq? 'VECTOR 'vector))
       ;; Don't generate vector.append method as long as structs are
       ;; vectors and there's no hierarchy for dot-oo methods!  XX
       ;; well, same problem for any of the methods if the same name
       ;; is to be used with a struct.
       (def. VECTOR.append VECTOR-append))
   (def. VECTOR.list VECTOR->list)
   (def. list.VECTOR list->VECTOR)

   (IF (not (eq? 'VECTOR 'string))
       (begin
	 ;; Heh these are still using the R5RS number operations
	 ;; generics. Rely on the host system to optimize these.
	 (def (VECTOR-inc! v i)
	      (VECTOR-set! v i
			   (+ (VECTOR-ref v i) 1)))
	 (def. VECTOR.inc! VECTOR-inc!)

	 (def (VECTOR-dec! v i)
	      (VECTOR-set! v i
			   (- (VECTOR-ref v i) 1)))
	 (def. VECTOR.dec! VECTOR-dec!)

	 ;; could also write (def .sum (C .fold _ + 0)) but then it wouldn't
	 ;; be properly extensible?
	 (def. (VECTOR.sum v)
	   (VECTOR.fold v + 0))))
   
   ;; Could abstract most code into a separate routine that takes a
   ;; make-vector argument, and uses object ops for the rest, but
   ;; those are not optimized at all yet.
   (def (VECTOR-map/iota f v)
	(let* ((len (VECTOR-length v))
	       (out (make-VECTOR len)))
	  (for..< (i 0 len)
		  (VECTOR-set! out i
			       (f (VECTOR-ref v i) i)))
	  out))
   (def. (VECTOR.map/iota v f)
     (VECTOR-map/iota f v))

   (def. (VECTOR.chop-both-ends v)
     (subVECTOR v 1 (dec (VECTOR.length v))))

   (def (VECTOR-for-each proc v)
	(let ((len (VECTOR-length v)))
	  (for..< (i 0 len)
		  (proc (VECTOR-ref v i)))))
   (def. (VECTOR.for-each v proc)
     (VECTOR-for-each proc v))

   ;; directly define dot-oo version as non-dot-oo version will be
   ;; n-ary:
   (def. (VECTOR.map v fn)
     (let* ((len (VECTOR-length v))
	    (res (make-VECTOR len)))
       (for..< (i 0 len)
	       (VECTOR-set! res i (fn (VECTOR-ref v i))))
       res))

   ;; n-ary, non-oo version:

   ;; These are already in vector-util!  Not removing them right now
   ;; for fear of dependencies.
   (define (VECTORs-map fn vecs accept-uneven-lengths?)
     (let* ((lens (map VECTOR-length vecs))
	    (len (apply min lens))
	    (cont (lambda ()
		    (let* ((res (##make-VECTOR len)))
		      (let lp ((i 0))
			(if (= i len)
			    res
			    (begin
			      (VECTOR-set! res
					   i
					   (apply fn
						  (map (lambda (vec)
							 (VECTOR-ref vec i))
						       vecs)))
			      (lp (inc i)))))))))
       (if accept-uneven-lengths?
	   (cont)
	   (let ((lenmax (apply max lens)))
	     (if (= len lenmax)
		 (cont)
		 (error "uneven lengths of input VECTORs (min max):"
			len lenmax))))))
   ;; ^XX really getting wasteful with duplication through code-map-substrings

   (define (VECTOR-map fn . vecs)
     (VECTORs-map fn vecs #f))

   (define (VECTOR-map* fn . vecs)
     (VECTORs-map fn vecs #t))


   (def (VECTOR-fold-right fn tail vec)
	(let ((len (VECTOR-length vec)))
	  (let rec ((i 0))
	    (if (= i len)
		tail
		(fn (VECTOR-ref vec i)
		    (rec (inc i)))))))
   (def. (VECTOR.fold-right vec fn tail)
     (VECTOR-fold-right fn tail vec))

   (def (VECTOR-fold fn tail vec)
	(let ((len (VECTOR-length vec)))
	  (let lp ((res tail)
		   (i 0))
	    (if (= i len)
		res
		(lp (fn (VECTOR-ref vec i)
			res)
		    (inc i))))))
   (def. (VECTOR.fold vec fn tail)
     (VECTOR-fold fn tail vec))

   ;; Ok this one isn't an OO function; but still fits nicely.
   (def (VECTORs-append strs)
	(let* ((out (##make-VECTOR (sum (map VECTOR-length strs)))))
	  (let lp ((strs strs)
		   (pos 0))
	    (if (null? strs)
		out
		(let-pair ((str strs*) strs)
			  (let ((len (VECTOR-length str)))
			    (for..< (i 0 len)
				    (VECTOR-set! out (+ pos i) (VECTOR-ref str i)))
			    (lp strs* (+ pos len))))))))
   ;; OO-version would be difficult, do what with the empty list? Well can do this:
   (def pair-with-car-VECTOR? (pair-with-car VECTOR?))
   (def. pair-with-car-VECTOR.append VECTORs-append)))



(TEST
 > (.chop-both-ends (u32vector 0 7 0))
 #u32(7)
 > (.chop-both-ends (u32vector 0 7))
 #u32()
 > (.u8vector (map .integer (.list "foo")))
 #u8(102 111 111)
 > (.append '#u8(1 2) '#u8(3 4))
 #u8(1 2 3 4)
 )

(TEST
 > (.fold '#(1 2 3) vector 'null)
 #(3 #(2 #(1 null)))
 > (.fold-right '#(1 2 3) vector 'null)
 #(1 #(2 #(3 null)))
 )


(TEST
 > (strings-append '())
 ""
 > (strings-append '("foo"))
 "foo"
 > (strings-append '("foo" "bar"))
 "foobar"
 > (strings-append '("" "bar"))
 "bar"
 ;;  > (%try-error (strings-append "foo"))
 ;; *** ERROR IN (console)@7.1 -- (Argument 2) LIST expected
 ;; (map '#<procedure #2 string-length> "foo")
 ;; > (strings-append '("" 1))
 ;; *** ERROR IN map -- (Argument 1) STRING expected
 ;; (string-length 1)
 > (.append '("" "bar"))
 "bar"
 > (.append (map .u8vector '("FOO" "BAR")))
 #u8(70 79 79 66 65 82)
 > (.sum (.u8vector "AB"))
 131
 )


(def string->u8vector (comp* list->u8vector
			     (cut map char->integer <>)
			     string->list))

(def. string.u8vector string->u8vector)




