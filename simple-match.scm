;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (lib.cj-source)
	 (lib.define-macro-star)
	 (lib.cj-phasing)
	 (lib.test)
	 (lib.improper-length))


;; definition of location-warn-to-string see cj-source

(TEST
 > (location-warn-to-string '#((console) 3) "hallo" 1)
 "*** WARNING IN (console)@4.1 -- hallo: 1\n"
 )

;; definitions of match* and match-list* see simple-match-1

(TEST
 > (match* '(1 2 3 4) ((a b . c) c) (s s))
 (3 4)
 > (match* '(1 2 3 4) ((a b . c) (vector a b c)) (s s))
 #(1 2 (3 4))
 > (match* '(1 2 3) ((a b . c) (vector a b c)) (s s))
 #(1 2 (3))
 > (match* '(1 2) ((a b . c) (vector a b c)) (s s))
 #(1 2 ())
 > (match* '(1) ((a b . c) (vector a b c)) (s s))
 (1)
)

(TEST
> (match* '() (() (list "one item" )))
("one item")
> (match* '(2) ((x) (list "one item" x)))
("one item" 2)
; > (match '() ((x) (list "one item" x)))
; *** ERROR IN (console)@25.1 -- no match for: ()
; 1> 
> (match* '(3 4) ((x) (list "one item" x)) ((x y) (list "two items" x y)))
("two items" 3 4)
> (match* '(3) ((x) (list "one item" x)) ((x y) (list "two items" x y)))
("one item" 3)
> (match* '() ((x) (list "one item" x)) ((x y) (list "two items" x y)) (() '(well)))
(well)
> (match* '(1 2 3 4) ((a b c) c) (s s))
(1 2 3 4)
> (match* '(1 2 3) ((a b c) c) (s s))
3
 )

