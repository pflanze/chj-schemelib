;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; A mutated (vector based) ring buffer that overflows into (deletes)
;; the oldest entries.

(require easy
	 Maybe
	 test
	 local-test)


(def (ringbuffer:inc wp len)
     (let ((wp* (inc wp)))
       (if (= wp* len)
	   0
	   (-> (C < _ len) wp*))))


(defclass ((ringbuffer _ringbuffer)
	   [natural0? len]
	   [vector? storage]
	   [box? &rp]
	   [box? &wp]
	   [box? &empty?])

  (def (make-ringbuffer [natural0? len])
       (_ringbuffer len (make-vector len) (box 0) (box 0) (box #t)))

  (def (list.ringbuffer items)
       (let ((b (make-ringbuffer (length items))))
	 (for-each (C ringbuffer.push! b _)
		   items)
	 b))

  ;; clone
  (defmethod (copy s)
    (_ringbuffer len
		 (vector-copy storage)
		 (box-copy &rp)
		 (box-copy &wp)
		 (box-copy &empty?)))
  
  ;; The value added first is 'coming out' first, too.  XX test
  (defmethod (rlist s)
    (if (ringbuffer.empty? s)
	'()
	(let ((wp (unbox &wp))
	      (i (unbox &rp)))
	  (let lp ((res (cons (vector-ref storage i) '()))
		   (i (ringbuffer:inc i len)))
	    (if (= i wp)
		res
		(lp (cons (vector-ref storage i)
			  res)
		    (ringbuffer:inc i len)))))))

  (defmethod (list s)
    (reverse (ringbuffer.rlist s)))

  (defmethod (length s)
    (if (unbox &empty?)
	0
	(let ((d (- (unbox &wp) (unbox &rp))))
	  (if (<= d 0)
	      (+ len d)
	      d))))

  (defmethod (empty? s)
    (unbox &empty?))

  ;; XX really that complex? Really return #t for empty buffers?
  (defmethod (full? s)
    (if (ringbuffer.empty? s)
	(zero? len)
	(= (unbox &rp) (unbox &wp))))
  
  (defmethod (push! s val)
    (if (not (zero? len))
	(let* ((wp (unbox &wp))
	       (wp* (ringbuffer:inc wp len))
	       (was-full? (ringbuffer.full? s)))
	  (vector-set! storage wp val)
	  (box-set! &wp wp*)
	  (if (ringbuffer.empty? s)
	      (box-set! &empty? #f)
	      (if was-full?
		  (box-set! &rp wp*))))))

  (defmethod (Maybe-pop! s)
    (if (ringbuffer.empty? s)
	(Nothing)
	(let* ((rp (unbox &rp))
	       (val (vector-ref storage rp)))
	  (vector-set! storage rp #f) ;; release for gc
	  ;; ^ store (Nothing) and then simplify code?
	  (let ((rp* (ringbuffer:inc rp len)))
	    (box-set! &rp rp*)
	    (if (= rp* (unbox &wp))
		(box-set! &empty? #t)))
	  (Just val))))

  ;; optim the other way around to avoid alloc
  (defmethod (pop! s)
    (Maybe:if-let ((v (ringbuffer.Maybe-pop! s)))
		  v
		  (error "queue is empty"))))


(TEST
 > (def b (make-ringbuffer 3))
 > (.list b)
 ()
 > (.length b)
 0
 > (list (.empty? b) (.full? b))
 (#t #f)
 > (.push! b 1)
 > (list (.empty? b) (.full? b))
 (#f #f)
 > (.list b)
 (1)
 > (.length b)
 1
 > (.push! b 2)
 > (list (.empty? b) (.full? b))
 (#f #f)
 > (.list b)
 (1 2)
 > (.push! b 3)
 > (list (.empty? b) (.full? b))
 (#f #t)
 > (.list b)
 (1 2 3)
 > (.push! b 4)
 > (list (.empty? b) (.full? b))
 (#f #t)
 > (.list b)
 (2 3 4)
 > (.length b)
 3
 > (.pop! b)
 2
 > (list (.empty? b) (.full? b))
 (#f #f)
 > (.list b)
 (3 4)
 > (.length b)
 2
 > (show (.Maybe-pop! b))
 (Just 3)
 > (list (.empty? b) (.full? b))
 (#f #f)
 > (show (.Maybe-pop! b))
 (Just 4)
 > (list (.empty? b) (.full? b))
 (#t #f)
 > (show (.Maybe-pop! b))
 (Nothing)
 > (%try-error (.pop! b))
 [error "queue is empty"]
 )


;; This test suite is quite horribly contorted.

;; (Also test that slots are always deleted (not preventing GC)?)

(TEST
 > (def (r)
	(make-list! (random-natural0 100) (C random-natural 10000)))
 > (def (t)
	;; (warn "t")
	(let* ((vals (r))
	       (nvals (length vals))
	       (len (random-natural0 100))
	       (b (make-ringbuffer len))
	       (maxspace (min nvals len))

	       (initial-t
		(lambda ()
		  (local-TEST
		   > (.length b)
		   0
		   > (.empty? b)
		   #t
		   > (equal? (.full? b) (zero? len))
		   #t))))

	  (initial-t)
	  
	  ;; "prime" the ring buffer so that its internal pointers are
	  ;; not at the initial position:
	  (let ((vals (make-list! (random-natural0
				   (inc (* len 3)))
				  (C random-natural 10000))))
	    (for-each (C .push! b _) vals)
	    (for..< (i 0 (min (length vals) len))
		    (.pop! b) ))
	  
	  (initial-t)
	  

	  (let ((subvals (take vals maxspace)))

	    (for-each (C .push! b _) subvals)

	    (let ((c (.copy b)))
	      (local-TEST
	       > (list (equal? (.empty? b)
			       (null? subvals))
		       (or (equal? (.full? b) (= maxspace len))
			   (raise 'pls)))
	       (#t #t)
	       > (equal? (.list b) subvals)
	       #t
	       > (.push! b 111)
	       > (list (equal? (.empty? b)
			       (zero? len))
		       (or (equal? (.full? b)
				   (or (= maxspace len)
				       (= (inc maxspace) len)))
			   (raise 'xyz)))
	       (#t #t)
	       > (or (equal? (.rlist b)
			     (if (zero? len)
				 '()
				 (cons 111 (reverse (if (= maxspace len)
							(if (null? subvals)
							    '()
							    (rest subvals))
							subvals)))))
		     (raise 'n))
	       #t
	       > (equal? (.Maybe-pop! b)
			 (if (zero? len)
			     (Nothing)
			     (Just (if (or (null? subvals) (= len 1))
				       111
				       (if (= maxspace len)
					   (second subvals)
					   (first subvals))))))
	       #t
	       ;; pop all values except one before the '111, or all
	       ;; before the '111 in case maxspace==len; this will
	       ;; leave at least one value in the buffer (unless the
	       ;; buffer is of zero len)
	       > (for..< (i 0 (- maxspace 2))
			 (.pop! b))
	       > (or (equal? (.length b)
			     (if (or
				  (null? subvals)
				  ;; ^ 111 already gone above
				  (zero? len)
				  ;; if len is 1 then the pop above
				  ;; already removed the 111
				  (= len 1))
				 0
				 (if (= maxspace len)
				     1
				     (if (= maxspace 1)
					 1
					 2))))
		     (raise 'x))
	       #t
	       > (let ((v (.Maybe-pop! b)))
		   (or (equal? v
			       (if (<= 0 len 1)
				   (Nothing)
				   (if (or (= maxspace len)
					   (= maxspace 1))
				       (Just 111)
				       (if (null? subvals)
					   (Nothing)
					   (Just (last subvals))))))
		       (raise 'y)))
	       #t
	       ;; and definitely the last item or already nothing:
	       > (let ((v (.Maybe-pop! b)))
		   (or (equal? v
			       (if (or (null? subvals)
				       (<= 0 len 1)
				       (= maxspace len)
				       (= maxspace 1))
				   (Nothing)
				   (Just 111)))
		       (raise 'z)))
	       #t
	       > (list (.empty? b) (equal? (.full? b) (zero? len)))
	       (#t #t)
	       > (Nothing? (.Maybe-pop! b))
	       #t)
	      ;;(raise (continuation-capture id))
	      ))))
 > (repeat 10 (t)))


