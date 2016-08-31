;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; sigh such a mess, so, here's the part of list-util that depends on
;; easy (cj-typed would have been enough though?)

(require easy-1 ;; not easy, so that we could still make this part of easy
	 (srfi-1 iota)
	 test)

(export ..
	..<)


;; odd I thought I had defined something like this already somewhere,
;; other name? or what?  from-to

;; forbid inexact values, since, after all:
;; > (vector-ref '#(1 2 3) 0.0)
;; *** ERROR IN (console)@31.1 -- (Argument 2) Exact INTEGER expected


(def (..< #(exact-integer? a) #(exact-integer? b))
     (if (<= a b)
	 (iota (- b a) a)
	 (error "..<: b is smaller than a:" a b)))

(def (.. #(exact-integer? a) #(exact-integer? b))
     (if (<= a b)
	 (iota (inc (- b a)) a)
	 (error "..: b is smaller than a:" a b)))

(TEST
 > (.. 1 3)
 (1 2 3)
 > (..< 1 3)
 (1 2)
 > (..< -1 1)
 (-1 0)
 > (%try-error (..< -1.0 1.0))
 #(error "a does not match exact-integer?:" -1.))

