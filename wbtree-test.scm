;;; Copyright 2010 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.

(require wbtree
	 define-macro-star
	 (cj-env IF)
	 test)

(define wbtreeparameter-string
  (wbtreeparameter string-cmp
		      string?))

(define wbtreeparameter-number
  (wbtreeparameter number-cmp
		      number? ;; ##fixnum? but well not inlined anyway anymore
		      ))

(define-macro* ($s . body)
  `(let (($wbtreeparameter wbtreeparameter-string))
     ,@body))

(define-macro* ($sdefine var body)
  `(define ,var
     (let (($wbtreeparameter wbtreeparameter-string))
       ,body)))

(TEST
 > ($s(wbtree:members (wbtree:gt (list->wbtree '("a" "b" "f" "z" "e" "g")) "f")))
 ("g" "z")
 > ($s(wbtree:members (wbtree:gt (list->wbtree '("a" "b" "f" "z" "e" "g" "r" "c")) "f")))
 ("g" "r" "z")
 > ($s(wbtree:members (wbtree:gt (list->wbtree '("a" "b" "f" "z" "e" "g" "r" "c" "n")) "f")))
 ("g" "n" "r" "z")
 > ($s(wbtree:members (wbtree:gt (list->wbtree '("a" "b" "h" "f" "z" "e" "g" "r" "c" "n")) "f")))
 ("g" "h" "n" "r" "z")
 > ($s(wbtree:members (wbtree:gt (list->wbtree '("a" "b" "h" "f" "z" "e" "g" "r" "c" "n" "o")) "f")))
 ("g" "h" "n" "o" "r" "z")
 > ($s(wbtree:members (wbtree:lt (list->wbtree '("a" "b" "h" "f" "z" "e" "g" "r" "c" "n" "o")) "l")))
 ("a" "b" "c" "e" "f" "g" "h")
 > ($s(wbtree:members (wbtree:gt (list->wbtree '("a" "b" "h" "f" "z" "e" "g" "r" "c" "n" "o")) "l")))
 ("n" "o" "r" "z")
 > ($sdefine t (wbtree:set empty-wbtree "a"))
 > ($sdefine t (wbtree:set t "b")))

(IF use-wbtrees-as-leafs?
    (TEST
     > t
     #((wbtree) "a" 2 empty-wbtree #((wbtree) "b" 1 empty-wbtree empty-wbtree)))
    (TEST
     > t
     #((wbtree) "a" 2 empty-wbtree "b")))

(TEST
 > ($sdefine t1 t)
 > ($sdefine t (wbtree:set t "b"))
 > (equal? t1 t)
 #t
 > ($sdefine t (wbtree:set t "c"))
 > ($sdefine t (wbtree:set t "d"))
 > ($sdefine t (wbtree:set t "c")))

(IF use-wbtrees-as-leafs?
    (TEST
     > t
     #(wbtree
       "b"
       4
       #((wbtree) "a" 1 empty-wbtree empty-wbtree)
       #((wbtree) "c" 2 empty-wbtree #((wbtree) "d" 1 empty-wbtree empty-wbtree)))
     > ($sdefine t (wbtree:set t "d"))
     > t
     #(wbtree
       "b"
       4
       #((wbtree) "a" 1 empty-wbtree empty-wbtree)
       #((wbtree) "c" 2 empty-wbtree #((wbtree) "d" 1 empty-wbtree empty-wbtree)))
     > ($sdefine t (wbtree:set t "e"))
     > t
     #(wbtree
       "c"
       5
       #((wbtree) "b" 2 #((wbtree) "a" 1 empty-wbtree empty-wbtree) empty-wbtree)
       #((wbtree) "d" 2 empty-wbtree #((wbtree) "e" 1 empty-wbtree empty-wbtree))))
    (TEST
     > t
     #((wbtree)
       "b"
       4
       "a"
       #((wbtree) "c" 2 empty-wbtree "d"))
     > ($sdefine t (wbtree:set t "d"))
     > t
     #((wbtree)
       "b"
       4
       "a"
       #((wbtree) "c" 2 empty-wbtree "d"))
     > ($sdefine t (wbtree:set t "e"))
     > t
     #((wbtree)
       "c"
       5
       #((wbtree) "b" 2 "a" empty-wbtree)
       #((wbtree) "d" 2 empty-wbtree "e"))))


(TEST
 > ($s(wbtree:member? t "u"))
 #f
 > ($s(wbtree:member? t "a"))
 #t
 > ($s(wbtree:member? t "b"))
 #t
 > ($s(wbtree:member? t "e"))
 #t
 > ($s(wbtree:member? t "f"))
 #f
 > ($s(wbtree:members t))
 ("a" "b" "c" "d" "e")
 > ($sdefine t (wbtree:set t "da"))
 > (equal? t ($s(wbtree:set t "da")))
 #t
 > (define e (with-exception-catcher identity (lambda () ($s(wbtree:add t "da")))))
 > (wbtree-duplicate-exception? e)
 #t
 > (wbtree-duplicate-exception-element e)
 "da"

 > ($s(wbtree:members t))
 ("a" "b" "c" "d" "da" "e")
 ;; aha ye: uniq:
 > ($s(wbtreesort '("def" "abc" "a" "a" "f")))
 ("a" "abc" "def" "f")
 > (define t-input '("def" "abc" "a" "a" "f" "n" "abe" "abba" "berta" "zwerg" "Zwerg"))
 > ($sdefine t (list->wbtree t-input))
)

(IF use-wbtrees-as-leafs?
    (TEST
     > t
     #(wbtree
       "abe"
       10
       #(wbtree
	 "abba"
	 4
	 #((wbtree) "a" 2 #((wbtree) "Zwerg" 1 empty-wbtree empty-wbtree) empty-wbtree)
	 #((wbtree) "abc" 1 empty-wbtree empty-wbtree))
       #(wbtree
	 "f"
	 5
	 #((wbtree) "def" 2 #((wbtree) "berta" 1 empty-wbtree empty-wbtree) empty-wbtree)
	 #((wbtree) "n" 2 empty-wbtree #((wbtree) "zwerg" 1 empty-wbtree empty-wbtree)))))
    (TEST
     > t
     #((wbtree)
       "abe"
       10
       #((wbtree)
	 "abba"
	 4
	 #((wbtree) "a" 2 "Zwerg" empty-wbtree)
	 "abc")
       #((wbtree)
	 "f"
	 5
	 #((wbtree) "def" 2 "berta" empty-wbtree)
	 #((wbtree) "n" 2 empty-wbtree "zwerg")))))

(TEST
 ;; > (define t-input-sorted (sort t-input string<?))  but it contains a double. so instead:
 > (define t-input-sorted '("Zwerg" "a" "abba" "abc" "abe" "berta" "def" "f" "n" "zwerg"))
 > (equal? ($s (wbtree:members t)) t-input-sorted)
 #t
 > ($s(wbtree:rank t "a"))
 1
 > ($s (wbtree:maybe-ref&rank t "a"))
 ("a" . 1)
 > ($s(wbtree:index t 0))
 "Zwerg"
 > ($s(wbtree:index t 1))
 "a"
 ;; > (wbtree:rank t "b")
 ;; *** ERROR IN #<procedure #5>, "wbwbtree.scm"@436.10 -- This object was raised: not-found
 ;; 1> 
 > ($s(wbtree:max t))
 "zwerg"
 > ($s(wbtree:size t))
 10
 > ($s(wbtree:index t 9))
 "zwerg"
 ;; > (wbtree:index t 10)
 ;; *** ERROR IN (console)@27.1 -- This object was raised: not-found
 ;; 1> 
 > ($s (map (lambda (v)
	      (let ((p (wbtree:maybe-ref&rank t v)))
		(let ((a (wbtree:index t (cdr p)))
		      (b (wbtree:rank t (car p))))
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
 > ($s(wbtree:members (wbtree:union (list->wbtree '("a" "b" "f"))(list->wbtree '("a" "z" "e" "g")))))
 ("a" "b" "e" "f" "g" "z")
 > ($s(wbtree:members (wbtree:difference (list->wbtree '("a" "b" "f"))(list->wbtree '("a" "z" "e" "g")))))
 ("b" "f")
 > ($s(wbtree:members (wbtree:difference (list->wbtree '("a" "b" "f"))(list->wbtree '("a" "b" "z" "e" "g")))))
 ("f")
 > ($s(wbtree:members (wbtree:difference (list->wbtree '("a" "b" "f"))(list->wbtree '("a" "b" "z" "e" "f" "g")))))
 ()
 > ($s(wbtree:members (wbtree:difference (list->wbtree '("a" "z" "e" "g"))(list->wbtree '("a" "b" "f")))))
 ("e" "g" "z")
 > ($s(wbtree:members (wbtree:intersection (list->wbtree '("a" "z" "e" "g"))(list->wbtree '("a" "b" "f")))))
 ("a")
 > ($s(wbtree:members (wbtree:intersection (list->wbtree '("z" "e" "g"))(list->wbtree '("a" "b" "f")))))
 ()
 > ($s(wbtree:members (wbtree:intersection (list->wbtree '("a" "z" "e" "g"))(list->wbtree '("a" "g" "n" "b" "f")))))
 ("a" "g")
 > ($s(wbtree:members (wbtree:intersection (list->wbtree '("a" "z" "e" "f" "g"))(list->wbtree '("a" "g" "n" "b" "f")))))
 ("a" "f" "g")
 > ($s (wbtree:members (wbtree:delete (list->wbtree '("a" "z" "e" "f" "g")) "f")))
 ("a" "e" "g" "z")
 > ($s (wbtree:members (wbtree:delete (list->wbtree '("a" "z" "e" "f" "g")) "ff")))
 ("a" "e" "f" "g" "z")
 )



(define wbtreeparameter-pair-string
  (wbtreeparameter (on car string-cmp)
		      pair?))


(define-macro* ($ . body)
  `(let (($wbtreeparameter wbtreeparameter-pair-string))
     ,@body))

(define-macro* ($define var body)
  `(define ,var
     (let (($wbtreeparameter wbtreeparameter-pair-string))
       ,body)))

(TEST
 > ($define t2 (list->wbtree '(("a". 1) ("z". 2) ("e". 3) ("f". 4) ("g". 5))))
 > t2
 #((wbtree)
   ("f" . 4)
   5
   #((wbtree) ("e" . 3) 2 ("a" . 1) empty-wbtree)
   #((wbtree) ("g" . 5) 2 empty-wbtree ("z" . 2)))
 > ($ (wbtree:member? t2 '("a" . x)))
 #t
 > ($ (wbtree:member? t2 '("aa" . x)))
 #f
 > ($ (wbtree:maybe-ref t2 '("a" . x)))
 ("a" . 1)
 > ($ (wbtree:maybe-ref t2 '("aa" . x)))
 #f

 > ($ (wbtree:between t2 '("aa") '("x")))
 #((wbtree) ("f" . 4) 3 ("e" . 3) ("g" . 5))
 > ($ (wbtree:between t2 '("a") '("x")))
 #((wbtree) ("f" . 4) 3 ("e" . 3) ("g" . 5))
 > ($ (wbtree:between t2 '("") '("x")))
 #((wbtree) ("f" . 4) 4 #((wbtree) ("a" . 1) 2 empty-wbtree ("e" . 3)) ("g" . 5))
 > ($ (wbtree:between t2 '("a") '("g")))
 #((wbtree) ("f" . 4) 2 ("e" . 3) empty-wbtree)
 > ($ (wbtree:members (wbtree:between t2 '("a") '("g"))))
 (("e" . 3) ("f" . 4))
 > ($ (wbtree:members (wbtree:between t2 '("") '("g"))))
 (("a" . 1) ("e" . 3) ("f" . 4))
 > ($ (wbtree:members (wbtree:between t2 '("") '("zz"))))
 (("a" . 1) ("e" . 3) ("f" . 4) ("g" . 5) ("z" . 2))
 > ($ (wbtree:members (wbtree:between t2 '("") '("z"))))
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
 > (define* (prandomwbtree *s n)
     (letv ((l rest) (stream-rtake+rest (unbox *s) n))
	   (set-box! *s rest)
	   (list->wbtree l)))
 > ($sdefine t1 (prandomwbtree *stringstream1 10000))
 > ($sdefine t2 (prandomwbtree *stringstream1 10000))
 > ($sdefine t3 (prandomwbtree *stringstream1 10000))
 ;; and one with different (more spaced when watched from the above perspective?) keys:
 ;; Use less elements to initialize it to attain about the same size (fewer duplicates)
 > ($sdefine t4 (prandomwbtree *stringstream2 5100))

 > ($s (equal? (F (wbtrees:intersection-stream (list t1 t2 t3)))
	       (wbtree:members (RA wbtree:intersection t1 t2 t3))))
 #t
 > ($s (equal? (F (wbtrees:intersection-stream (list t1 t2 t4)))
	       (wbtree:members (RA wbtree:intersection t1 t2 t4))))
 #t
 > (define* (wbtree3:intersection-list t1 t2 t3) (wbtree:members (RA wbtree:intersection t1 t2 t3)))
 > ($s (equal?* (wbtree3:intersection-list t1 t2 t3) (wbtree3:intersection-list t2 t1 t3) (wbtree3:intersection-list t3 t2 t1)))
 #t
 )

