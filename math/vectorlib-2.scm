


;;;XXXX  aufraumen  nur weg compil hier


(define. (Mc.mirror0 m)
  (letv ((s0 s1) (.sizes m))
	(let ((res (@make-Mc s0 s1)))
	  (parallel-for-all
	   2000
	   m
	   (s0 s1)
	   (i0 i1)
	   (.set! res i0 i1
		  (.ref m (- (dec s0) i0) i1)))
	  res)))
;;COPYPASTE
(define. (Mr.mirror0 m)
  (letv ((s0 s1) (.sizes m))
	(let ((res (@make-Mr s0 s1)))
	  (parallel-for-all
	   2000
	   m
	   (s0 s1)
	   (i0 i1)
	   (.set! res i0 i1
		  (.ref m (- (dec s0) i0) i1)))
	  res)))


(TEST
 > (.show (.mirror0 (Mc (Vc 1 2) (Vc 3 4))))
 (Mc (Vc 3.+0.i 4.+0.i) (Vc 1.+0.i 2.+0.i))
 )

