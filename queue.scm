;;; Copyright 2016-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; a functional queue implementation (cheaper, but less feature rich
;; than alternative wbtree.scm)

(require easy
	 test
	 Maybe
	 (cj-functional values-of))

(export (jclass queue)
	queue
	list.queue)

(jclass ((queue _queue)
	 ;; newest in front
	 forward
	 ;; oldest in front
	 backward)

	(def-method (enqueue q v) -> queue?
	  (_queue (cons v forward)
		  backward))

	(def-method (Maybe-dequeue q) -> (values-of queue? Maybe?)
	  (if (null? backward)
	      (if (null? forward)
		  (values q
			  (Nothing))
		  (let ((backward* (reverse forward)))
		    (values (_queue '()
				    (cdr backward*))
			    (Just (car backward*)))))
	      (values (_queue forward
			      (cdr backward))
		      (Just (car backward)))))

	(def-method (list q)
	  (append forward (reverse backward)))

	(def-method (rlist q)
	  (append backward (reverse forward)))

	(def-method (empty? q)
	  (and (null? forward)
	       (null? backward))))

(def (queue . items)
     (_queue items '()))

(def (list.queue items)
     (_queue items '()))

(TEST
 > (def q (queue 1 2 3))
 > (.empty? q)
 #f
 > (.list q)
 (1 2 3)
 > (.rlist q)
 (3 2 1)
 > (def q0 (.enqueue q 0))
 > (.list q0)
 (0 1 2 3)
 > (.rlist q0)
 (3 2 1 0)
 > q0
 #((queue) (0 1 2 3) ())

 > (def-values (q1 Mv) (.Maybe-dequeue q0))
 > Mv
 #((Just) 3)
 > (.list q1)
 (0 1 2)
 > (.rlist q1)
 (2 1 0)

 > (def-values (q2 Mv2) (.Maybe-dequeue q1))
 > Mv2
 #((Just) 2)
 > (.list q2)
 (0 1)
 > (.rlist q2)
 (1 0)
 > (def q3 (.enqueue q2 'n))
 > (.list q3)
 (n 0 1)
 > (.rlist q3)
 (1 0 n)

 > (def-values (q4 Mv4) (.Maybe-dequeue q3))
 > Mv4
 #((Just) 1)
 > (.list q4)
 (n 0)
 > (.rlist q4)
 (0 n)

 > (def-values (q5 Mv5) (.Maybe-dequeue q4))
 > Mv5
 #((Just) 0)
 > (.list q5)
 (n)
 > (.rlist q5)
 (n)
 > (.empty? q5)
 #f

 > (def-values (q6 Mv6) (.Maybe-dequeue q5))
 > Mv6
 #((Just) n)
 > (.list q6)
 ()
 > (.rlist q6)
 ()
 > (.empty? q6)
 #t

 > (def-values (q7 Mv7) (.Maybe-dequeue q6))
 > Mv7
 #((Nothing))
 > (.empty? q7)
 #t

 > (.list q3)
 (n 0 1))

