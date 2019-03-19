;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         corescheme
         corescheme-to-scheme
         corescheme-optimize
         test)

"Some random additional tests for the corescheme libraries."

(TEST
 > (def simplify
        (=>* (source.corescheme globals: '(.>>= return * + - f))
             corescheme-optimize
             .scheme))

 > (defmacro (SIMPLIFY* expr)
     `(simplify (quote-source ,expr)))

 > (defmacro (SIMPLIFY expr)
     `(cj-desourcify (simplify (quote-source ,expr)))))


(TEST
 > (SIMPLIFY (lambda (x) (f x)))
 f
 > (SIMPLIFY (lambda (x) ((f f) x)))
 ;; because (f f) would run in different dynamic context
 (lambda (x) ((f f) x))
 > (SIMPLIFY (lambda (x) (x f)))
 (lambda (x) (x f))
 > (SIMPLIFY (let ((x 100)) (+ x x)))
 ((lambda (x) (+ x x)) 100)
 > (SIMPLIFY (let ((x 100)) (+ x 10)))
 (+ 100 10)
 > (SIMPLIFY (let ((x 100) (y 120)) (f (* x 10))))
 (f (* 100 10))
 > (SIMPLIFY (let ((x 100) (y 120)) (f (* x 10 y))))
 (f (* 100 10 120))
 > (SIMPLIFY (let ((x 100) (y 120)) (f (* x 10 y y))))
 ((lambda (x y) (f (* x 10 y y))) 100 120)
 ;; so, now want to simplify it partially, ok?
 )


(TEST
 > (SIMPLIFY (let ((x f)) 1))
 1
 > (SIMPLIFY (let ((x f)) x))
 f
 > (SIMPLIFY (let* ((x f) (y x)) x))
 f
 > (SIMPLIFY (let* ((x f) (y x)) y))
 f
 > (SIMPLIFY (let ((let 1)) let))
 1
 > (SIMPLIFY (let ((let 1)) (let)))
 (1)
 > (with-exception-catcher source-error-message
                           (& (SIMPLIFY (let* ((let 1) (lambda let)) (lambda () 2)))))
 "unquoted empty list treated as invalid function application"
 > (SIMPLIFY (let* ((let 1) (lambda let)) (lambda (quote x) 2)))
 (1 'x 2)
 > ((lambda (quote x) 'x) (lambda (x) (+ x 1)) 2)
 3
 > (SIMPLIFY ((lambda (quote x) 'x) (lambda (x) (+ x 1)) 2))
 ((lambda (x) (+ x 1)) 2) ;; XXX  apply simplification again!
 > (simplify (SIMPLIFY ((lambda (quote x) 'x) (lambda (x) (+ x 1)) 2)))
 (+ 2 1) ;; and then, constant folding hehe ?

 > (SIMPLIFY (let* ((let 1) (lambda let)) (lambda (f) 2)))
 (1 (f) 2)
 > (SIMPLIFY (let* ((let 1) (lambda let) (f 3)) (lambda (f) 2)))
 (1 (3) 2)
 > (SIMPLIFY f)
 f
 > (SIMPLIFY 'f)
 'f
 > (SIMPLIFY ''f)
 ''f
 > (SIMPLIFY '''f)
 '''f
 )


;; can I generate programs that  are   runnable?  (only 1 data type? but ops that are  order of application sensitive; / or string-append or such)
;; cons, list  


(TEST
 > (SIMPLIFY ((lambda (x f) (f x)) f 10))
 (10 f)
 > (SIMPLIFY ((lambda (x f) (lambda (f) (f x))) f 10))
 (lambda (f) (f f)) ;; and that is WRONG, needs renaming
 )

