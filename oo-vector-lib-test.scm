;;; Copyright 2014-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy-1
	 oo-lib-u32vector
	 oo-lib-u8vector
         oo-lib-s8vector
	 oo-lib-vector
	 oo-lib-string
	 oo-lib-s32vector
	 ;; alternatively could require oo-lib-all
	 test
         (range char.inc))


(TEST
 > (%try (.swap! (vector 10) 0 1))
 (exception text: "vector.swap!: i or j not a proper index: 0 1\n")
 > (.swap! (vector 10) 0 0)
 [10]
 > (%try (.swap! (vector) 0 0))
 (exception text: "vector.swap!: i or j not a proper index: 0 0\n")
 > (.swap! (vector 10 11) 0 0)
 [10 11]
 > (.swap! (vector 10 11) 0 1)
 [11 10]
 > (.swap! (vector 10 11) 1 0)
 [11 10]
 > (.swap! (vector 10 11 12) 1 0)
 [11 10 12]
 > (.swap! (vector 10 11 12) 1 2)
 [10 12 11]
 > (%try (.swap! (vector 10 11 12) -1 2))
 (exception text: "vector.swap!: i or j not a proper index: -1 2\n")
 > (%try (.swap! (vector 10 11 12) 1 3))
 (exception text: "vector.swap!: i or j not a proper index: 1 3\n")
 > (%try (.swap! (vector 10 11 12) 3 1))
 (exception text: "vector.swap!: i or j not a proper index: 3 1\n")
 > (%try (.swap! (vector 10 11 12) 1 2.))
 (exception text: "vector.swap!: i or j not a proper index: 1 2.\n")
 > (%try (.swap! (vector 10 11 12) 1 3/1))
 (exception text: "vector.swap!: i or j not a proper index: 1 3\n"))


(TEST
 > (.chop-both-ends (u32vector 0 7 0))
 #u32(7)
 > (.chop-both-ends (u32vector 0 7))
 #u32()
 > (.u8vector (map .integer (.list "foo")))
 #u8(102 111 111)
 > (.append '#u8(1 2) '#u8(3 4))
 #u8(1 2 3 4)
 )

(TEST
 > (.fold '#(1 2 3) vector 'null)
 #(3 #(2 #(1 null)))
 > (.fold-right '#(1 2 3) vector 'null)
 #(1 #(2 #(3 null)))
 )


(TEST
 > (strings-append '())
 ""
 > (strings-append '("foo"))
 "foo"
 > (strings-append '("foo" "bar"))
 "foobar"
 > (strings-append '("" "bar"))
 "bar"
 ;;  > (%try-error (strings-append "foo"))
 ;; *** ERROR IN (console)@7.1 -- (Argument 2) LIST expected
 ;; (map '#<procedure #2 string-length> "foo")
 ;; > (strings-append '("" 1))
 ;; *** ERROR IN map -- (Argument 1) STRING expected
 ;; (string-length 1)

 ;; Can't use .append any more since list.append overrides it now
 > (pair-with-car-string.append '("" "bar"))
 "bar"
 ;; and ditto:
 > (pair-with-car-u8vector.append (map .u8vector '("FOO" "BAR")))
 #u8(70 79 79 66 65 82)
 > (.sum (.u8vector "AB"))
 131
 )


(TEST
 > (.string (u8vector 65 66))
 "AB"
 ;; kinda pointless random tests
 > (def (t v)
	(assert (equal? v (string.u8vector (u8vector.string v)))))
 > (for-each (lambda (l) (t (random-u8vector (* l 13)))) (iota 7)))

(TEST
 > (.reverse (s32vector 43341 -3))
 #s32(-3 43341))


(TEST
 > (.first "Hello")
 #\H
 > (.last "Hello")
 #\o
 > (.rest "Hell")
 "ell"
 ;; Not the best error messages, well.. :
 > (%try (.first ""))
 (exception text: "string-first: string is empty\n")
 > (%try (.last ""))
 (exception text: "string-last: string is empty\n")
 > (%try (.rest ""))
 (exception text: "string-rest: string is empty\n"))

(TEST
 > (.filter/iota (vector 2 -4 5 8) (lambda (v i) (even? v)))
 #(2 -4 8)
 > (.filter/iota (vector 2 -4 5 8) (lambda (v i) (even? i)))
 #(2 5))

(TEST
 > (def l '())
 > (def v (vector 10 11 12))
 > (.for-each/iota v (lambda (x i)
		       (push! l (cons x i))))
 > l
 ((12 . 2) (11 . 1) (10 . 0)))

(TEST
 > (.filter "Hello, World." char-alpha?)
 "HelloWorld")


(TEST
 > (def a (vector 1 23 3))
 > (eq? a (vector-append a '[]))
 #f ;; in Gambit, at least, but IIRC the standard is even asking for this?
 > (equal? a (vector-append a '[]))
 #t
 > (eq? a (vector-append-optimized a '[]))
 #t
 > (def a "Hi")
 > (eq? a (string-append a ""))
 #f
 > (eq? a (string-append-optimized a ""))
 #t
 > (eq? a (string-append-optimized "" a))
 #t
 > (eq? a (string-append "" a))
 #f ;; ?
 )

(TEST
 > (show (.min&max '[10 9]))
 (values 9 10)
 > (show (.min&max '[-10 9]))
 (values -10 9)
 > (show (.min&max '[-10]))
 (values -10 -10)
 > (%try (.min&max '[]))
 (exception text: "vector-min&max: got empty vector\n")
 > (show (.min&max '[10 29 4]))
 (values 4 29))

(TEST
 > (show (.split-at '[a b c] 0))
 (values (vector) (vector 'a 'b 'c))
 > (show (.split-at '[a b c] 3))
 (values (vector 'a 'b 'c) (vector))
 > (show (.split-at '[a b c] 2))
 (values (vector 'a 'b) (vector 'c))
 > (%try (.split-at '[a b c] 4))
 (exception text: "vector-split-at: argument out of bounds: 4\n")
 > (%try (.split-at '[a b c] -1))
 (exception text: "n does not match exact-natural0?: -1\n"))

(TEST
 > ((s8vector-of-length 1) (s8vector))
 #f
 > ((s8vector-of-length 1) (s8vector 1))
 #t
 > ((s8vector-of-length 1) (u8vector 1))
 #f
 > (%try (s8vector-of-length -1))
 (exception text: "len does not match fixnum-natural0?: -1\n")
 > ((s8vector-of-length 0) (s8vector))
 #t)

(TEST
 > (def v '(a b c))
 > (def v '[a b c])
 > (.set v 1 'B)
 [a B c]
 > v
 [a b c]
 > (def v "[a b c]")
 > (%try (.set v 1 'B))
 (exception
  text: "(Argument 3) CHARACTER expected\n(string-set! \"[a b c]\" 1 'B)\n")
 > (.set v 1 #\B)
 "[B b c]"
 > v
 "[a b c]")

(TEST
 > (vector.insert (vector) 0 'a)
 #(a)
 > (vector.insert (vector 1 2) 0 'a)
 #(a 1 2)
 > (vector.insert (vector 1 2) 1 'a)
 #(1 a 2)
 > (vector.insert (vector 1 2) 2 'a)
 #(1 2 a))

(TEST
 > (define s "hello")
 > (.update! s 2 char.inc)
 "hemlo"
 > s
 "hemlo"
 > (define s "hello")
 > (.update s 3 char.inc)
 "helmo"
 > s
 "hello")

