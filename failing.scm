;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Module versions to toggle predicates between normal and
;; exception-throwing modes

(require easy)

;; XX ugly. Because of the letrec like behaviour (assumption) in
;; define-module body (re exported symbols, but really all of them
;; right? because of my hacking around those Scheme/Gambit limits),
;; need to pass in all original symbols explicitely. And bind them to
;; different names.

(defmodule (<failing> failing
		      orig:equal?
		      orig:source-equal?)
  (export equal? source-equal?)
  
  (def equal? (failing orig:equal?))
  (def source-equal? (failing orig:source-equal?)))

(def <failing>*
     (lambda (failing)
       (<failing> failing
		  equal? source-equal?)))


(def (raise-failures f)
     (lambda args
       (or (apply f args)
	   (error "function returned false"))))


(def <failing-on> (<failing>* raise-failures))
(def <failing-off> (<failing>* identity))

