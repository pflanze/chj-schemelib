;;; Copyright 2010-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require ;;cj-source-util
 test
 (test-lib-1 %try))


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
 #f

 ;; compare with Gambits apply:
 ;; > (apply inc '(1 2 3))
 ;; *** ERROR IN (console)@64.1 -- Wrong number of arguments passed to procedure
 ;; (inc 1 2 3)
 ;; 1> 
 ;; > (apply inc '())
 ;; *** ERROR IN (console)@65.1 -- Wrong number of arguments passed to procedure
 ;; (inc)
 ;; 1> 
 ;; > (apply inc '(1))
 ;; 2
 )


(TEST
 ;; combine optional with key
 > (schemedefinition-arity:pattern->template '(#!key))
 [up-to 0 0]
 > (schemedefinition-arity:pattern->template '(#!key a))
 [up-to 0 2]
 > (schemedefinition-arity:pattern->template '(a #!key b))
 [up-to 1 3]
 > (schemedefinition-arity:pattern->template '(a #!key b c))
 [up-to 1 5]
 > (schemedefinition-arity:pattern->template '(#!optional a))
 [up-to 0 1]
 > (schemedefinition-arity:pattern->template '(#!optional a #!key b))
 [up-to 0 3]
 ;; ^XX Idea: should provide a vector of validities perhaps. a number
 ;; (bits)? But then, checking really depends on whether keywords are
 ;; given--or, does it not??*!*
 > (schemedefinition-arity:pattern->template '(#!optional a #!key b c))
 [up-to 0 5]
 > (schemedefinition-arity:pattern->template '(o #!optional a #!key b c))
 [up-to 1 6]

 
 > (define (t a b c #!optional d #!key e f)
     (vector a b c optional: d key: e f))
 > (define c (schemedefinition-arity-checker '(a b c #!optional d #!key e f)))

 > (c 2)
 not-enough

 > (t 1 2 3)
 [1 2 3 optional: #f key: #f #f]
 > (c 3)
 ok

 > (t 1 2 3 4)
 [1 2 3 optional: 4 key: #f #f]
 > (c 4)
 ok

 > (%try (t 1 2 3 4 5))
 (exception
  text:
  "Wrong number of arguments passed to procedure\n(t 1 2 3 4 5)\n")
 > (%try (t 1 2 3 e: 5))
 (exception
  text:
  "Wrong number of arguments passed to procedure\n(t 1 2 3 e: 5)\n")
 > (c 5)
 ok ;; actually wrong

 > (t 1 2 3 4 e: 6)
 [1 2 3 optional: 4 key: 6 #f]
 > (c 6)
 ok

 > (c 7)
 ok ;; actually wrong

 > (t 1 2 3 4 e: 6 f: 8)
 [1 2 3 optional: 4 key: 6 8]
 > (c 8)
 ok
 > (c 9)
 too-many)


(TEST
 ;; The other way around isn't actually valid:

 ;; > (define (t a b c #!key d #!optional e)
 ;;     (vector a b c key: d optional: e))
 ;; Ill-placed #!optional

 > (%try (schemedefinition-arity-checker '(a b c #!key d #!optional e)))
 (exception
  text:
  "#!optional after #!key in argument list: (a b c #!key d #!optional e)\n"))

