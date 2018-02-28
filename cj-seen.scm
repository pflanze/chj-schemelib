(require easy-1
	 test)

(def _cj-seen:nothing (gensym))

(def (make-seen?! . args)
     (let ((t (apply make-table args)))
       (lambda (val)
	 (let ((v (table-ref t val _cj-seen:nothing)))
	   (if (eq? v _cj-seen:nothing)
	       (begin
		 (table-set! t val #t)
		 #f)
	       #t)))))

(TEST
 > (def s?! (make-seen?!))
 > (s?! 3)
 #f
 > (s?! 3)
 #t
 > (s?! 3)
 #t
 > (s?! 4)
 #f
 > (s?! 4)
 #t
 > (def s2 (make-seen?!))
 > (s2 "foo")
 #f
 > (s2 "foo")
 #t
 > (s2 3)
 #f)


;; bah, ~copy-paste
(def (make-seen?+! . args)
     (let ((t (apply make-table args)))
       (values
	;; seen?
	(lambda (val)
	  ;; don't actually need _cj-seen:nothing, huh
	  (table-ref t val #f))
	;; seen!
	(lambda (val)
	  (table-set! t val #t)))))

