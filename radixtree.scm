;;; Copyright 2018-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 trie
         (oo-list-vector list.rvector))

(export (class radixtree)
	(method alist.radixtree))


(include "cj-standarddeclares.scm")


;; XX move

(def (vector-cmp-of cmp)
     (lambda (v1 v2) -> cmp?
       (let ((l1 (vector-length v1))
	     (l2 (vector-length v2)))
	 (let lp ((i 0))
	   (if (< i l1)
	       (if (< i l2)
		   (let ((r (cmp (vector-ref v1 i)
				 (vector-ref v2 i))))
		     (if (eq? r 'eq)
			 (lp (inc i))
			 r))
		   'lt)
	       'gt)))))

;; /move

;; compare entry key with entry key
(def radixtree-key-cmp (vector-cmp-of .cmp))

;; compare sublist of lookup key with entry key
(def (radixtree-lookup-key-cmp lis vec)
     (let ((vlen (vector-length vec)))
       (let lp ((i 0)
		(l lis))
	 (if (< i vlen)
	     (if (null? l)
		 'lt
		 (let-pair ((a l*) l)
			   (let ((r (.cmp a (vector-ref vec i))))
			     (if (eq? r 'eq)
				 (lp (inc i)
				     l*)
				 r))))
	     'eq))))

(TEST
 > (radixtree-lookup-key-cmp '(a b c) '[a b c])
 eq
 > (radixtree-lookup-key-cmp '(a b c d) '[a b c])
 eq
 > (radixtree-lookup-key-cmp '(a b) '[a b c])
 lt
 > (radixtree-lookup-key-cmp '(a b d) '[a b c])
 gt
 > (radixtree-lookup-key-cmp '(a b c) '[a b d])
 lt
 > (.cmp "a b c" "a b d")
 lt)



(defclass (radixtree [fixnum-natural0? min-length]
		     [fixnum-natural0? max-length]
		     ;; [fixnum-natural? num-values]
		     )
  extends: trie-alike

  (def (radixtree/entries-alist Maybe-value entries-alist min-length max-length)
       (radixtree Maybe-value
		  (alist.vectormap entries-alist radixtree-key-cmp)
		  min-length
		  max-length))
  
  (defmethod (show s show)
    `(radixtree/entries-alist
      ,(show Maybe-value)
      ,(show (vectormap.alist entries))
      ,min-length
      ,max-length))

  (defmethod (Maybe-ref-list s cs)
    (if (null? cs)
	Maybe-value
	(if-let ((i (vectormap.maybe-find entries cs radixtree-lookup-key-cmp)))
		(let ((s* (@vectormap.iref-value entries i))
		      (k (@vectormap.iref-key entries i)))
		  (radixtree.Maybe-ref-list s* (drop cs (vector-length k))))
		(Nothing)))))


(def. (trie.radixtree t)
  (letv
   ((keyvector node)
    (let rec ((t t)
	      (level 0)
	      (rkeys '()))
      ;; -> (values keyvector radixtree) ;; right for inclusion into
      ;; radixtree entries
      (let* ((entries (trie.entries t))
	     (entries-len (vectormap.length entries))
	     (val (trie.Maybe-value t)))

	(if (or (> entries-len 1) (Just? val))
	    ;; need to issue a node holding the branching point,
	    ;; or (/and) value, respectively
	    (values (list.rvector rkeys)
		    (let* ((m (vectormap.map
			       entries
			       (lambda (k t*)
				 (rec t* (inc level) (cons k '())))))
			   ;; XX optimize with a
			   ;; vectormap.fold, or what? Just
			   ;; deforestation, man?
			   (lens
			    (map (lambda-pair
				  ((k node))
				  (let ((keylen (vector-length k)))
				    (values (+ keylen
					       (radixtree.min-length node))
					    (+ keylen
					       (radixtree.max-length node)))))
				 (vectormap.alist m))))
		      (radixtree val
				 m
				 (if (Just? val)
				     0
				     (if (null? lens)
					 0
					 (list-min (map fst lens))))
				 (if (null? lens)
				     0
				     (list-max (map snd lens))))))
	    (xcase entries-len
		   ((0)
		    ;; "should never get a trie node at the end
		    ;; without value" aha, empty case, i.e. toplevel
		    ;; holder having no value
		    (assert (null? rkeys))
		    (values '[] ;; (list.rvector rkeys)
			    (radixtree val
				       empty-vectormap
				       0
				       0)))
		   ((1)
		    (rec (vectormap.@the-value entries)
			 (inc level)
			 (cons (vectormap.@the-key entries) rkeys))))))))

   (let ((keylen (vector-length keyvector)))
     (if (zero? keylen)
	 node
	 ;; need to make a holder node for the entry
	 (radixtree (Nothing)
		    (alist.vectormap (list (cons keyvector
						 node))
				     radixtree-key-cmp)
		    (+ keylen (radixtree.min-length node))
		    (+ keylen (radixtree.max-length node)))))))

(def. alist.radixtree (comp trie.radixtree alist.trie))


;; (def radixtree-vectormap? (sectioned-vector-of/2 vector? radixtree?))
;; but, can't (easily) use in entries field already defined in trie-alike


(TEST
 > (show (alist.radixtree '()))
 (radixtree/entries-alist (Nothing) (list) 0 0)
 > (show (alist.radixtree '(("a" . a) ("b" . b))))
 (radixtree/entries-alist
  (Nothing)
  (list (cons (vector #\a) (radixtree/entries-alist (Just 'a) (list) 0 0))
	(cons (vector #\b) (radixtree/entries-alist (Just 'b) (list) 0 0)))
  1 1)
 > (show (alist.radixtree '(("a" . a) ("ab" . b))))
 (radixtree/entries-alist
  (Nothing)
  (list (cons (vector #\a)
	      (radixtree/entries-alist
	       (Just 'a)
	       (list (cons (vector #\b)
			   (radixtree/entries-alist (Just 'b) (list) 0 0)))
	       0 1)))
  1 2)
 > (def r (alist.radixtree '(("a" . a) ("ab" . b))))
 > (.Maybe-ref r "a")
 [(Just) a]
 > (.Maybe-ref r "ab")
 [(Just) b]

 > (def r (radixtree/entries-alist
	   (Just 'a)
	   (list (cons (vector #\a #\b)
		       (radixtree/entries-alist (Just 'b)
						'()
						1000 1000)))
	   999
	   999))
 > (.Maybe-ref r "")
 [(Just) a]
 > (.Maybe-ref r "a")
 [(Nothing)]
 > (.Maybe-ref r "ab")
 [(Just) b])



(TEST
 > (def t (alist.trie '(("OPTIONS" . OPTIONS)
			("GET"     . GET)
			("HEAD"    . HEAD)
			("POST"    . POST)
			("PUT"     . PUT)
			("DELETE"  . DELETE)
			("TRACE"   . TRACE)
			("CONNECT" . CONNECT))))

 > (.Maybe-ref t "POST")
 [(Just) POST]
 > (.Maybe-ref t "PUT")
 [(Just) PUT]
 > (.Maybe-ref t "P")
 [(Nothing)]
 > (def r (trie.radixtree t))
 > (.Maybe-ref r "POST")
 [(Just) POST]
 > (.Maybe-ref r "PUT")
 [(Just) PUT]
 > (.Maybe-ref r "P")
 [(Nothing)]

 ;; more 'controlled' example:
 > (def t2alis '(("Q"  . Q)
		 ("QABC" . QABC)
		 ("QABDE" . QABDE)
		 ("QXYZ" . QXYZ)
		 ("R"  . R)))
 > (def t2 (alist.trie t2alis))
 > (map (lambda-pair ((k v))
		(equal? (.Maybe-ref t2 k) (Just v)))
	t2alis)
 (#t #t #t #t #t)
 > (.Maybe-ref t2 "QXY")
 [(Nothing)]
 > (.Maybe-ref t2 "QXYz")
 [(Nothing)]
 > (.Maybe-ref t2 "QABD2")
 [(Nothing)]
 > (.Maybe-ref t2 "QABDE2")
 [(Nothing)]
 > (.Maybe-ref t2 "S")
 [(Nothing)]
 > (.Maybe-ref t2 "")
 [(Nothing)]
 

 > (def r2 (trie.radixtree t2))
 > (map (lambda-pair ((k v))
		(equal? (.Maybe-ref r2 k) (Just v)))
	t2alis)
 (#t #t #t #t #t)
 > (.Maybe-ref r2 "QBX")
 [(Nothing)]
 > (.Maybe-ref r2 "QABD2")
 [(Nothing)]
 > (.Maybe-ref r2 "S")
 [(Nothing)]
 > (.Maybe-ref r2 "")
 [(Nothing)]

 > (equal? r2 (eval (show r2)))
 #t

 > (.min-length r2)
 1
 > (.max-length r2)
 5)


