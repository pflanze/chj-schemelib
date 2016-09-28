;;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require test
	 predicates-1 ;; for re-exports only?
	 cj-typed)


(export box-of
	forced ;; rename to possibly-promise-of ?
	any? true/1
	false/2
	false? ;; == not
	true?
	true
	nonnegative-real?
	nonpositive-real?
	negative-real?
	inexact-real?
	exact-real?
	exact-integer?
	exact-number?
	pair-or-null?
	pair-with-car
	nonempty-string?
	improper*-map/tail ;; XX move
	improper*-map	   ;; dito
	string-of
	string-of-length
	improper-every	  ;; XX move
	improper-list-of  ;; hmm
	char-one-of	  ;; move to char lib?
	perhaps-source-of ;; XX rename to possibly-source-of ?
	source-of
	perhaps-source*-of ;; dito
	source*-of
	length-=
	length-<=
	length->=
	length-is ;; see also list-of/length  -- rename to list-of-length ?
	length=
	list-of-length
	lists?
	0..1? ;; see also rgb:0..1?
	in-signed-range?
	parameter?

	;; and the new meat:
	function?
	predicate?
	throwing
	function-of)


;; for now there's no difference (intent: pure functions, aside of
;; unsafe I/O through stderr, ok? No further typing then, either.)

(define function? procedure?)

;; ^ also, for (java etc.) translation, more information of course,
;; finally.

;; well, like:

;; a function that takes any value and returns a boolean.
(define predicate? function?)

;; XX this must be part of cj-typed of course if it is to be a
;; non-noop
(define-typed (throwing #(predicate? e?)
			#(predicate? t?))
  t?)



(define (function-of #(predicate? inputs?) #(predicate? output?))
  ;; (lambda vals
  ;;   (if (inputs? vals)
  ;; 	(let ((res ())))))

  ;; (lambda (fn)
  ;;   )

  ;; Ah stupid (again?), can't do this. Predicates are only used to
  ;; check the input, not to wrap it. Can't make a type-checking
  ;; wrapped version of fn. Really need the static declarations and
  ;; checking. For now:
  function?)

