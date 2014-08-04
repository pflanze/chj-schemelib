(require test easy)


(def. f64vector.length f64vector-length)
(def. f64vector.list f64vector->list)
(def. list.f64vector list->f64vector)

(def. (f64vector.map/iota v f)
  (let* ((len (.length v))
	 (out (make-f64vector len)))
    (for..< (i 0 len)
	    (f64vector-set! out i
			    (f (f64vector-ref v i) i)))
    out))



(def. u32vector.length u32vector-length)

(def. (u32vector.chop-both-ends v)
  (subu32vector v 1 (dec (.length v))))

(TEST
 > (.chop-both-ends (u32vector 0 7 0))
 #u32(7)
 > (.chop-both-ends (u32vector 0 7))
 #u32())




(def. (f64vector.for-each v proc)
  (let ((len (f64vector.length v)))
    (for..< (i 0 len)
	    (proc (f64vector-ref v i)))))

;; (def (make-.map make-vector)
;;      (lambda (v fn)
;;        (let* ((len (.length v))
;; 	      (res (make-vector len)))
;; 	 (for..< (i 0 len)
;; 		 (.set! res (fn (.ref v i))))
;; 	 res)))

(defmacro (def-.map type)
  (let* ((type (source-code type))
	 (P (lambda (n)
	      (symbol-append type n))))
    `(def. ,(symbol-append type 'vector.map)
       (lambda (v fn)
	 (let* ((len (,(P 'vector-length) v))
		(res (,(symbol-append 'make- type 'vector) len)))
	   (for..< (i 0 len)
		   (,(P 'vector-set!) res i (fn (,(P 'vector-ref) v i))))
	   res)))))

(def-.map ||)
(def-.map u32)
(def-.map s32)
(def-.map u16)
(def-.map s16)
(def-.map u8)
(def-.map s8)
(def-.map f64)
(def-.map f32)


(def. f64vector.ref f64vector-ref)
(def. u8vector.ref u8vector-ref)
(def. s8vector.ref s8vector-ref)
(def. u16vector.ref u16vector-ref)
(def. s16vector.ref s16vector-ref)
(def. s32vector.ref s32vector-ref)
(def. u32vector.ref u32vector-ref)
(def. u64vector.ref u64vector-ref)
(def. s64vector.ref s64vector-ref)
(def. vector.ref vector-ref)

(def. f64vector.set! f64vector-set!)
(def. u8vector.set! u8vector-set!)
(def. s8vector.set! s8vector-set!)
(def. u16vector.set! u16vector-set!)
(def. s16vector.set! s16vector-set!)
(def. s32vector.set! s32vector-set!)
(def. u32vector.set! u32vector-set!)
(def. u64vector.set! u64vector-set!)
(def. s64vector.set! s64vector-set!)
(def. vector.set! vector-set!)

(def (u32vector-inc! v i)
     (u32vector-set! v i
		     (+ (u32vector-ref v i) 1)))

