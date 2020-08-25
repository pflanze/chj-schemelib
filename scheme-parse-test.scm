;;; Copyright 2018-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require test
	 scheme-parse
         cj-source)


(include "cj-standarddeclares.scm")


(TEST
 > (scheme-parse:lambda? '(lambda ()))
 #t
 > (cj-desourcify (scheme-parse:maybe-lambda-binds '(lambda (x))))
 (x)
 > (scheme-parse:lambda? '(##lambda (x)))
 #t
 ;; well:
 > (scheme-parse:lambda? '(lambda-values (x)))
 #f)

(TEST
 > (scheme-parse:maybe-lambda-exact-arity '(lambda ()))
 0
 > (scheme-parse:maybe-lambda-exact-arity '(lambda (x a b)))
 3
 > (scheme-parse:maybe-lambda-exact-arity '(lambda (x a . b)))
 #f
 > (scheme-parse:maybe-lambda-exact-arity '(lambda (x a #!optional b)))
 #f
 ;; well:
 > (scheme-parse:maybe-lambda-exact-arity '(lambda-pair (a)))
 #f)

