;;; Copyright 2016 by Christian Jaeger, ch at christianjaeger ch

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Meta-knowledge about Scheme

(require)

(export self-quoting?
	perhaps-quote

	#!optional
	void?)


(define (void? v)
  ;; (eq? v (void))  or, since I don't have a void.show yet either:
  (eq? v #!void))

(define (self-quoting? v)
  ;; avoid depending on |either| from cj-functional ?
  (or (string? v)
      (number? v)
      (boolean? v)
      (eof-object? v)
      (void? v)
      (keyword? v)))

(define (perhaps-quote v)
  (if (self-quoting? v)
      v
      (list 'quote v)))


