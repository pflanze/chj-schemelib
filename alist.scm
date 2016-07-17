;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Unlike cj-alist.scm doen't hard-code the pairings using pairs,
;; instead use parametrized accessors

;; Perhaps call it associative-list instead?

(require easy
	 Maybe)

(def alist:nothing (gensym))

(defmodule (<alist> key? .key .equal?)

  (export alist
	  Maybe-ref
	  ref
	  set
	  delete)

  ;; for compat with typed-alist.scm
  (def alist list)

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

  (def (ref lis key #!optional (alternate alist:nothing))
       (Maybe:cond ((Maybe-ref lis key) => identity)
		   (else
		    (if (eq? alternate alist:nothing)
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
	     (error "wrong type of key:" key))))

  ;; call it remove or delete ? Actually srfi-1 calls it remove; would
  ;; have conflict here :), but might be an indication it should be
  ;; called remove really. XX?
  (def (delete lis #(key? key))
       (remove (lambda (v)
		 (.equal? (.key v) key))
	       lis)))

