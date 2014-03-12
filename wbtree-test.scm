;;; Copyright 2010 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(define treeparameter-string
  (make-treeparameter string-cmp
		      string?))

(define treeparameter-number
  (make-treeparameter number-cmp
		      number? ;; ##fixnum? but well not inlined anyway anymore
		      ))

(define-macro* ($s . body)
  `(let (($treeparameter treeparameter-string))
     ,@body))

(define-macro* ($sdefine var body)
  `(define ,var
     (let (($treeparameter treeparameter-string))
       ,body)))

(TEST
 > ($s(tree:members (tree:gt (list->tree '("a" "b" "f" "z" "e" "g")) "f")))
 ("g" "z")
 > ($s(tree:members (tree:gt (list->tree '("a" "b" "f" "z" "e" "g" "r" "c")) "f")))
 ("g" "r" "z")
 > ($s(tree:members (tree:gt (list->tree '("a" "b" "f" "z" "e" "g" "r" "c" "n")) "f")))
 ("g" "n" "r" "z")
 > ($s(tree:members (tree:gt (list->tree '("a" "b" "h" "f" "z" "e" "g" "r" "c" "n")) "f")))
 ("g" "h" "n" "r" "z")
 > ($s(tree:members (tree:gt (list->tree '("a" "b" "h" "f" "z" "e" "g" "r" "c" "n" "o")) "f")))
 ("g" "h" "n" "o" "r" "z")
 > ($s(tree:members (tree:lt (list->tree '("a" "b" "h" "f" "z" "e" "g" "r" "c" "n" "o")) "l")))
 ("a" "b" "c" "e" "f" "g" "h")
 > ($s(tree:members (tree:gt (list->tree '("a" "b" "h" "f" "z" "e" "g" "r" "c" "n" "o")) "l")))
 ("n" "o" "r" "z")
 > ($sdefine t (tree:add empty-tree "a"))
 > ($sdefine t (tree:add t "b")))

(IF use-trees-as-leafs?
    (TEST
     > t
     #(tree "a" 2 empty-tree #(tree "b" 1 empty-tree empty-tree)))
    (TEST
     > t
     #(tree "a" 2 empty-tree "b")))

(TEST
 > ($sdefine t1 t)
 > ($sdefine t (tree:add t "b"))
 > (equal? t1 t)
 #t
 > ($sdefine t (tree:add t "c"))
 > ($sdefine t (tree:add t "d"))
 > ($sdefine t (tree:add t "c")))

(IF use-trees-as-leafs?
    (TEST
     > t
     #(tree
       "b"
       4
       #(tree "a" 1 empty-tree empty-tree)
       #(tree "c" 2 empty-tree #(tree "d" 1 empty-tree empty-tree)))
     > ($sdefine t (tree:add t "d"))
     > t
     #(tree
       "b"
       4
       #(tree "a" 1 empty-tree empty-tree)
       #(tree "c" 2 empty-tree #(tree "d" 1 empty-tree empty-tree)))
     > ($sdefine t (tree:add t "e"))
     > t
     #(tree
       "c"
       5
       #(tree "b" 2 #(tree "a" 1 empty-tree empty-tree) empty-tree)
       #(tree "d" 2 empty-tree #(tree "e" 1 empty-tree empty-tree))))
    (TEST
     > t
     #(tree
       "b"
       4
       "a"
       #(tree "c" 2 empty-tree "d"))
     > ($sdefine t (tree:add t "d"))
     > t
     #(tree
       "b"
       4
       "a"
       #(tree "c" 2 empty-tree "d"))
     > ($sdefine t (tree:add t "e"))
     > t
     #(tree
       "c"
       5
       #(tree "b" 2 "a" empty-tree)
       #(tree "d" 2 empty-tree "e"))))


(TEST
 > ($s(tree:member? t "u"))
 #f
 > ($s(tree:member? t "a"))
 #t
 > ($s(tree:member? t "b"))
 #t
 > ($s(tree:member? t "e"))
 #t
 > ($s(tree:member? t "f"))
 #f
 > ($s(tree:members t))
 ("a" "b" "c" "d" "e")
 > ($sdefine t (tree:add t "da"))
 > ($s(tree:members t))
 ("a" "b" "c" "d" "da" "e")
 ;; aha ye: uniq:
 > ($s(treesort '("def" "abc" "a" "a" "f")))
 ("a" "abc" "def" "f")
 > (define t-input '("def" "abc" "a" "a" "f" "n" "abe" "abba" "berta" "zwerg" "Zwerg"))
 > ($sdefine t (list->tree t-input))
)

(IF use-trees-as-leafs?
    (TEST
     > t
     #(tree
       "abe"
       10
       #(tree
	 "abba"
	 4
	 #(tree "a" 2 #(tree "Zwerg" 1 empty-tree empty-tree) empty-tree)
	 #(tree "abc" 1 empty-tree empty-tree))
       #(tree
	 "f"
	 5
	 #(tree "def" 2 #(tree "berta" 1 empty-tree empty-tree) empty-tree)
	 #(tree "n" 2 empty-tree #(tree "zwerg" 1 empty-tree empty-tree)))))
    (TEST
     > t
     #(tree
       "abe"
       10
       #(tree
	 "abba"
	 4
	 #(tree "a" 2 "Zwerg" empty-tree)
	 "abc")
       #(tree
	 "f"
	 5
	 #(tree "def" 2 "berta" empty-tree)
	 #(tree "n" 2 empty-tree "zwerg")))))

(TEST
 ;; > (define t-input-sorted (sort t-input string<?))  but it contains a double. so instead:
 > (define t-input-sorted '("Zwerg" "a" "abba" "abc" "abe" "berta" "def" "f" "n" "zwerg"))
 > (equal? ($s (tree:members t)) t-input-sorted)
 #t
 > ($s(tree:rank t "a"))
 1
 > ($s (tree:maybe-ref&rank t "a"))
 ("a" . 1)
 > ($s(tree:index t 0))
 "Zwerg"
 > ($s(tree:index t 1))
 "a"
 ;; > (tree:rank t "b")
 ;; *** ERROR IN #<procedure #5>, "wbtree.scm"@436.10 -- This object was raised: not-found
 ;; 1> 
 > ($s(tree:max t))
 "zwerg"
 > ($s(tree:size t))
 10
 > ($s(tree:index t 9))
 "zwerg"
 ;; > (tree:index t 10)
 ;; *** ERROR IN (console)@27.1 -- This object was raised: not-found
 ;; 1> 
 > ($s (map (lambda (v)
	      (let ((p (tree:maybe-ref&rank t v)))
		(let ((a (tree:index t (cdr p)))
		      (b (tree:rank t (car p))))
		  (or (equal? v a)
		      (warn* "bug" v a))
		  (or (equal? (cdr p) b)
		      (warn* "bug" (cdr p) b))
		  (cons a b))))
	    t-input-sorted))
 (("Zwerg" . 0)
  ("a" . 1)
  ("abba" . 2)
  ("abc" . 3)
  ("abe" . 4)
  ("berta" . 5)
  ("def" . 6)
  ("f" . 7)
  ("n" . 8)
  ("zwerg" . 9))

 ;; set operations:
 > ($s(tree:members (tree:union (list->tree '("a" "b" "f"))(list->tree '("a" "z" "e" "g")))))
 ("a" "b" "e" "f" "g" "z")
 > ($s(tree:members (tree:difference (list->tree '("a" "b" "f"))(list->tree '("a" "z" "e" "g")))))
 ("b" "f")
 > ($s(tree:members (tree:difference (list->tree '("a" "b" "f"))(list->tree '("a" "b" "z" "e" "g")))))
 ("f")
 > ($s(tree:members (tree:difference (list->tree '("a" "b" "f"))(list->tree '("a" "b" "z" "e" "f" "g")))))
 ()
 > ($s(tree:members (tree:difference (list->tree '("a" "z" "e" "g"))(list->tree '("a" "b" "f")))))
 ("e" "g" "z")
 > ($s(tree:members (tree:intersection (list->tree '("a" "z" "e" "g"))(list->tree '("a" "b" "f")))))
 ("a")
 > ($s(tree:members (tree:intersection (list->tree '("z" "e" "g"))(list->tree '("a" "b" "f")))))
 ()
 > ($s(tree:members (tree:intersection (list->tree '("a" "z" "e" "g"))(list->tree '("a" "g" "n" "b" "f")))))
 ("a" "g")
 > ($s(tree:members (tree:intersection (list->tree '("a" "z" "e" "f" "g"))(list->tree '("a" "g" "n" "b" "f")))))
 ("a" "f" "g")
 > ($s (tree:members (tree:delete (list->tree '("a" "z" "e" "f" "g")) "f")))
 ("a" "e" "g" "z")
 > ($s (tree:members (tree:delete (list->tree '("a" "z" "e" "f" "g")) "ff")))
 ("a" "e" "f" "g" "z")
 )



(define treeparameter-pair-string
  (make-treeparameter (on car string-cmp)
		      pair?))


(define-macro* ($ . body)
  `(let (($treeparameter treeparameter-pair-string))
     ,@body))

(define-macro* ($define var body)
  `(define ,var
     (let (($treeparameter treeparameter-pair-string))
       ,body)))

(TEST
 > ($define t2 (list->tree '(("a". 1) ("z". 2) ("e". 3) ("f". 4) ("g". 5))))
 > t2
 #(tree
   ("f" . 4)
   5
   #(tree ("e" . 3) 2 ("a" . 1) empty-tree)
   #(tree ("g" . 5) 2 empty-tree ("z" . 2)))
 > ($ (tree:member? t2 '("a" . x)))
 #t
 > ($ (tree:member? t2 '("aa" . x)))
 #f
 > ($ (tree:maybe-ref t2 '("a" . x)))
 ("a" . 1)
 > ($ (tree:maybe-ref t2 '("aa" . x)))
 #f

 > ($ (tree:between t2 '("aa") '("x")))
 #(tree ("f" . 4) 3 ("e" . 3) ("g" . 5))
 > ($ (tree:between t2 '("a") '("x")))
 #(tree ("f" . 4) 3 ("e" . 3) ("g" . 5))
 > ($ (tree:between t2 '("") '("x")))
 #(tree ("f" . 4) 4 #(tree ("a" . 1) 2 empty-tree ("e" . 3)) ("g" . 5))
 > ($ (tree:between t2 '("a") '("g")))
 #(tree ("f" . 4) 2 ("e" . 3) empty-tree)
 > ($ (tree:members (tree:between t2 '("a") '("g"))))
 (("e" . 3) ("f" . 4))
 > ($ (tree:members (tree:between t2 '("") '("g"))))
 (("a" . 1) ("e" . 3) ("f" . 4))
 > ($ (tree:members (tree:between t2 '("") '("zz"))))
 (("a" . 1) ("e" . 3) ("f" . 4) ("g" . 5) ("z" . 2))
 > ($ (tree:members (tree:between t2 '("") '("z"))))
 (("a" . 1) ("e" . 3) ("f" . 4) ("g" . 5))
 
 )

(TEST
 > (define *stringstream1 (box (pseudorandomsource*->a-z-string-stream
				(make-pseudorandomsource 2910 734532)
				(make-pseudorandomsource 83249 982288)
				(make-range 2 4))))
 > (define *stringstream2 (box (pseudorandomsource*->a-z-string-stream
				(make-pseudorandomsource 121290 119072)
				(make-pseudorandomsource 32349 25288)
				(make-range 3 15))))
 > (define* (prandomtree *s n)
     (letv ((l rest) (stream-rtake+rest (unbox *s) n))
	   (set-box! *s rest)
	   (list->tree l)))
 > ($sdefine t1 (prandomtree *stringstream1 10000))
 > ($sdefine t2 (prandomtree *stringstream1 10000))
 > ($sdefine t3 (prandomtree *stringstream1 10000))
 ;; and one with different (more spaced when watched from the above perspective?) keys:
 ;; Use less elements to initialize it to attain about the same size (fewer duplicates)
 > ($sdefine t4 (prandomtree *stringstream2 5100))

 > ($s (equal? (F (trees:intersection-stream (list t1 t2 t3)))
	       (tree:members (RA tree:intersection t1 t2 t3))))
 #t
 > ($s (equal? (F (trees:intersection-stream (list t1 t2 t4)))
	       (tree:members (RA tree:intersection t1 t2 t4))))
 #t
 > (define* (tree3:intersection-list t1 t2 t3) (tree:members (RA tree:intersection t1 t2 t3)))
 > ($s (equal?* (tree3:intersection-list t1 t2 t3) (tree3:intersection-list t2 t1 t3) (tree3:intersection-list t3 t2 t1)))
 #t
 )

