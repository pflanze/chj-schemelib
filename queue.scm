;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

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

(class queue
       (struct constructor-name: _queue
	       forward	;; newest in front
	       backward ;; oldest in front
	       )

       (method (enqueue q v) -> queue?
	       (let-queue ((fw ba) q)
			  (_queue (cons v fw)
				  ba)))

       (method (Maybe-dequeue q) -> (values-of queue? Maybe?)
	       (let-queue ((fw ba) q)
			  (if (null? ba)
			      (if (null? fw)
				  (values q
					  (Nothing))
				  (let ((ba* (reverse fw)))
				    (values (_queue '()
						    (cdr ba*))
					    (Just (car ba*)))))
			      (values (_queue fw
					      (cdr ba))
				      (Just (car ba))))))

       (method (list q)
	       (let-queue ((fw ba) q)
			  (append fw (reverse ba))))

       (method (rlist q)
	       (let-queue ((fw ba) q)
			  (append ba (reverse fw))))

       (method (empty? q)
	       (let-queue ((fw ba) q)
			  (and (null? fw)
			       (null? ba)))))

(def (queue . items)
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

 > (defvalues (q1 Mv) (.Maybe-dequeue q0))
 > Mv
 #((Just) 3)
 > (.list q1)
 (0 1 2)
 > (.rlist q1)
 (2 1 0)

 > (defvalues (q2 Mv2) (.Maybe-dequeue q1))
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

 > (defvalues (q4 Mv4) (.Maybe-dequeue q3))
 > Mv4
 #((Just) 1)
 > (.list q4)
 (n 0)
 > (.rlist q4)
 (0 n)

 > (defvalues (q5 Mv5) (.Maybe-dequeue q4))
 > Mv5
 #((Just) 0)
 > (.list q5)
 (n)
 > (.rlist q5)
 (n)
 > (.empty? q5)
 #f

 > (defvalues (q6 Mv6) (.Maybe-dequeue q5))
 > Mv6
 #((Just) n)
 > (.list q6)
 ()
 > (.rlist q6)
 ()
 > (.empty? q6)
 #t

 > (defvalues (q7 Mv7) (.Maybe-dequeue q6))
 > Mv7
 #((Nothing))
 > (.empty? q7)
 #t

 > (.list q3)
 (n 0 1))

