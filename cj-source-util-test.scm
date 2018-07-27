;;; Copyright 2010-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require ;;cj-source-util
	 test)


(TEST
 > (define c (schemedefinition-arity-checker '(a b c . d)))
 > (c 2)
 not-enough
 > (c 3)
 ok
 > (c 4)
 ok
 > (c 500)
 ok
 > (define c (schemedefinition-arity-checker '(a b c #!rest d)))
 > (c 2)
 not-enough
 > (c 3)
 ok
 > (c 4)
 ok
 > (c 500)
 ok
 > (define c (schemedefinition-arity-checker '(a b c #!optional d)))
 > (c 2)
 not-enough
 > (c 3)
 ok
 > (c 4)
 ok
 > (c 5)
 too-many
 > (define c (schemedefinition-arity-checker '(a b c #!key d)))
 > (c 2)
 not-enough
 > (c 3)
 ok
 > (c 4)
 ok ;; uhm, actually invalid!
 > (c 5)
 ok
 > (c 6)
 too-many
 > (define c (schemedefinition-arity-checker '(a b c #!rest d)))
 > (c 2)
 not-enough
 > (c 3)
 ok
 > (c 4)
 ok
 > (c 5)
 ok
 > (define c (schemedefinition-arity-checker '(a b c #!key d #!rest e)))
 > (c 2)
 not-enough
 > (c 3)
 ok
 > (c 4)
 ok ;; uhm, actually invalid!
 > (c 5)
 ok
 > (c 6)
 ok)


(TEST
 > (define c (schemedefinition-arity:pattern->template '(a b c #!optional d)))
 > (define f (lambda (a b c #!optional d) d))
 > (safer-apply c f '(10 20 30 40) error values)
 40
 > (safer-apply c f '(10 20 30 40 50) vector values)
 #("too many arguments")
 > (safer-apply c f '(10 20 30) error values)
 #f)


;; compare with Gambits apply:
; > (apply inc '(1 2 3))
; *** ERROR IN (console)@64.1 -- Wrong number of arguments passed to procedure
; (inc 1 2 3)
; 1> 
; > (apply inc '())
; *** ERROR IN (console)@65.1 -- Wrong number of arguments passed to procedure
; (inc)
; 1> 
; > (apply inc '(1))
; 2


