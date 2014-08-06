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
   (def. VECTOR.append VECTOR-append)
   (def. VECTOR.list VECTOR->list)
   (def. list.VECTOR list->VECTOR)

   ;; Heh these are still using the R5RS number operations
   ;; generics. Rely on the host system to optimize these.
   (def. (VECTOR-inc! v i)
     (VECTOR-set! v i
		  (+ (VECTOR-ref v i) 1)))
   (def VECTOR.inc! VECTOR-inc!)

   (def. (VECTOR-dec! v i)
     (VECTOR-set! v i
		  (- (VECTOR-ref v i) 1)))
   (def VECTOR.dec! VECTOR-dec!)
   
   ;; Could abstract most code into a separate routine that takes a
   ;; make-vector argument, and uses object ops for the rest, but
   ;; those are not optimized at all yet.
   (def. (VECTOR.map/iota v f)
     (let* ((len (VECTOR.length v))
	    (out (make-VECTOR len)))
       (for..< (i 0 len)
	       (VECTOR-set! out i
			    (f (VECTOR-ref v i) i)))
       out))

   (def. (VECTOR.chop-both-ends v)
     (subVECTOR v 1 (dec (VECTOR.length v))))

   (def. (VECTOR.for-each v proc)
     (let ((len (VECTOR.length v)))
       (for..< (i 0 len)
	       (proc (VECTOR-ref v i)))))

   (def. (VECTOR.map v fn)
     (let* ((len (VECTOR.length v))
	    (res (make-VECTOR len)))
       (for..< (i 0 len)
	       (VECTOR-set! res i (fn (VECTOR-ref v i))))
       res))

   ;; Non-dot-oo versions of these are already in vector-util!

   (def. (VECTOR.fold-right vec fn tail)
     (let ((len (VECTOR.length vec)))
       (let rec ((i 0))
	 (if (= i len)
	     tail
	     (fn (VECTOR-ref vec i)
		 (rec (inc i)))))))

   (def. (VECTOR.fold vec fn tail)
     (let ((len (VECTOR.length vec)))
       (let lp ((res tail)
		(i 0))
	 (if (= i len)
	     res
	     (lp (fn (VECTOR-ref vec i)
		     res)
		 (inc i))))))

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
 )

