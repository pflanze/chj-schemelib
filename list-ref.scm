;; Unlike _-alist-ref doen't hard-code the pairings using pairs,
;; instead use parametrized accessors

;; Perhaps call it associative-list instead?

(require easy
	 Maybe
	 )

(def _list-ref:nothing (gensym))
  
(defmodule (<list-ref> key? .key .equal?)

  (export Maybe-ref
	  ref
	  set)

  (def (Maybe-ref lis #(key? key))
       (let lp ((l lis))
	 (cond ((pair? l)
		(let-pair ((a r) l)
			  (if (.equal? key (.key a))
			      (Just  a)
			      (lp r))))
	       ((null? l)
		(Nothing))
	       (else
		(error "improper list:" lis)))))

  (def (ref lis key #!optional (alternate _list-ref:nothing))
       (Maybe:cond ((Maybe-ref lis key) => identity)
		   (else
		    (if (eq? alternate _list-ref:nothing)
			(error "ref: value not found in list"
			       lis key)
			alternate))))

  ;; adapted copy from cj-alist.scm; should write that one in terms of
  ;; this of course.
  (def (set lis key+val)
       (let ((key (.key key+val)))
	 (if (key? key)
	     (let lp ((l lis))
	       (if (null? l)
		   ;; key not found, add entry to front
		   (cons key+val lis)
		   (let ((frame (car l)))
		     (if (.equal? (.key frame) key)
			 ;; replace, i.e. keep tail, replace current
			 ;; frame, add newer frames on top
			 (let ((tail (cons key+val (cdr l))))
			   (let rec ((l2 lis))
			     (let ((frame2 (car l2)))
			       (if (eq? frame2 frame)
				   ;; arrived at same place again
				   tail
				   (cons frame2
					 (rec (cdr l2)))))))
			 (lp (cdr l))))))
	     (error "wrong type of key:" key)))))

