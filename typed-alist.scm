;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Unlike cj-alist.scm doen't hard-code the pairings using pairs,
;; instead use parametrized accessors

;; Perhaps call it associative-list instead?

;; XX adapted COPY of alist.scm, gah

(require easy
	 typed-list
	 Maybe)

(def typed-alist:nothing (gensym))

;; gah, need pairing? as additional parameter over <alist>
(defmodule (<typed-alist> key? .key .equal? pairing?)

  (export ?
	  alist
	  Maybe-ref
	  ref
	  set
	  delete)

  (def ? (typed-list-of pairing?))
  
  (def (alist . pairings)
       (list->typed-list pairing? pairings))

  (def (Maybe-ref lis #(key? key))
       (let lp ((l lis))
	 (xcond ((typed-list-pair? l)
		 (typed-list:let-pair
		  ((a r) l)
		  (if (.equal? key (.key a))
		      (Just  a)
		      (lp r))))
		((typed-list-null? l)
		 (Nothing)))))

  (def (ref lis key #!optional (alternate typed-alist:nothing))
       (Maybe:cond ((Maybe-ref lis key) => identity)
		   (else
		    (if (eq? alternate typed-alist:nothing)
			(error "ref: value not found in list"
			       lis key)
			alternate))))

  ;; adapted copy from cj-alist.scm; should write that one in terms of
  ;; this of course.
  (def (set lis key+val)
       (let ((key (.key key+val)))
	 (if (key? key)
	     (let lp ((l lis))
	       (if (typed-list-null? l)
		   ;; key not found, add entry to front
		   (typed-list.cons lis key+val)
		   (typed-list:let-pair
		    ((frame r) l)
		    (if (.equal? (.key frame) key)
			;; replace, i.e. keep tail, replace current
			;; frame, add newer frames on top
			(let ((tail (typed-list.cons r key+val)))
			  (let rec ((l2 lis))
			    (typed-list:let-pair
			     ((frame2 r2) l2)
			     (if (eq? frame2 frame)
				 ;; arrived at same place again
				 tail
				 (typed-list.cons (rec r2) frame2)))))
			(lp r)))))
	     (error "wrong type of key:" key))))

  ;; call it remove or delete ? Actually srfi-1 calls it remove; would
  ;; have conflict here :), but might be an indication it should be
  ;; called remove really. XX?
  (def (delete lis #(key? key))
       ;; There's no typed-list.remove, as it has to dispatch anyway!
       ;; (wow)
       (.remove lis
		(lambda (v)
		  (.equal? (.key v) key)))))

