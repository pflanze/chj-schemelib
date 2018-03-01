(require easy
	 test
	 alist
	 cj-seen)


(jclass (topo-relation [symbol? name]
		       [(list-of symbol?) deps]))

;; topo:Maybe-ref, topo:ref
(modimport/prefix topo: (<alist> symbol?
				 topo-relation.name
				 eq?))

(def topo? (list-of topo-relation?))
;; XX must .name be unique in each topo? ?

(def (topo.sort rs) -> topo?
     (letv
      ((processed? processed!) (make-seen?+!))
      (letv
       ((seen? seen!) (make-seen?+!))
       (let ((name.dep.relation
	      (lambda (name)
		(lambda (dep)
		  (Maybe:cond ((topo:Maybe-ref rs dep)
			       => identity)
			      (else
			       (error "in module, unknown dependency:"
				      name dep))))))
	     (out '())
	     (all-rs rs))
	 (let load ((rloadernames '())
		    (rs rs))
	   (for-each
	    (lambda (r)
	      (let-topo-relation
	       ((name deps) r)
	       (if (seen? name)
		   (unless (processed? name)
			   (error "cycle detected resolving:"
				  name rloadernames))
		   (begin
		     (seen! name)
		     (load (cons name rloadernames)
			   (map (name.dep.relation name) deps))
		     (push! out r)
		     (processed! name)))))
	    rs))
	 (reverse out)))))

(def (topo.sort* rs) -> (list-of symbol?)
     (map .name (topo.sort rs)))


(TEST
 > (def rs (list
	    (topo-relation 'a '(c))
	    (topo-relation 'b '())
	    (topo-relation 'c '())
	    (topo-relation 'd '())
	    (topo-relation 'e '(a c))
	    (topo-relation 'f '(b c d))
	    (topo-relation 'g '(e))
	    (topo-relation 'h '())
	    (topo-relation 'i '(h))))
 > (def sorted-ns (topo.sort* rs))
 > sorted-ns
 (c a b d e f g h i)
 > (def (c . ns)
	(let ((l (filter (lambda (n)
			   (memq n ns))
			 sorted-ns)))
	  (if (equal? ns l)
	      (if (equal? ns (reverse l))
		  'eq
		  'lt)
	      'gt)))
 > (c 'a 'e)
 lt
 > (c 'e 'a)
 gt
 > (c 'e 'h)
 lt ;; eq well. does e need h or something? h needs nothing, e needs a
    ;; c which don't need it. eq would be correct.
 > (c 'e 'e)
 gt ;; huh;  eq ;; ok?  
 > (c 'a 'c)
 gt ;; eq  well.
 > (c 'e 'g)
 lt
 > (c 'c 'g)
 lt ;; indirect dependency
 )

(TEST
 > (cmp-sort '(a b) c)
 (a b)
 > (cmp-sort '(a c) c)
 (c a)
 > (cmp-sort '(b f) c)
 (b f)
 > (cmp-sort '(f b) c)
 (b f)
 > (cmp-sort '(f b a) c)
 (a b f) ;; was (b f a) correct, too?

 > (topo.sort* (cons* (topo-relation 'a '(c)) ;;sameas before
		      (topo-relation 'b '(a))
		      (cddr rs)))
 (c a b d e f g h i)
 > (topo.sort* (cons* (topo-relation 'a '(c)) ;;sameas before
		      (topo-relation 'b '(e))
		      (cddr rs)))
 (c a e b d f g h i))


(TEST
 > (%try-error
    (topo.sort* (list (topo-relation 'a '(b)) (topo-relation 'b '(a)))))
 #(error "cycle detected resolving:" a (b a)))


;; XX add rule based tests


