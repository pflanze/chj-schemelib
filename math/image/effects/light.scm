;;; Copyright 2013-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 (cj-math square)
	 ;;XX?:
	 math/vectorlib)

;; create light effect

(declare (standard-bindings)
	 (extended-bindings)
	 (block))

;; light functions

;; individual gray channels

;; x and y stretch yeah rt

;; narrowness too tho

(define flight.0
  (lambda (s1)
    (lambda (i1)
      ;;(square (log (inc i1)))
      (square (- s1 i1)))))


(define (freqmod x)
  (freqmod@ x))

(define (flight s1)
  (let ((f1 (flight.0 s1)))
    (lambda (i1)
      (* (f1 i1) (freqmod i1) (- s1 i1)))))

;; (view-fn (flight 1) 0 1) +- good. 5 +-same. more->notsom


(define (flight2d s0 s1)
  (let ((f0 (compose sqrt (flight 1)))
	(f1 (compose sqrt (flight 1)))
	(scale0 (/ s0))
	(scale1 (/ s1)))
    (lambda (i0 i1)
      (* (f0 (* i0 scale0))
	 (f1 (* i1 scale1))))))

(define (Mr:flight2d s0 s1)
  (Mr:generate (flight2d s0 s1) s0 s1))
;; okay nj sorta neat eckegequetschte blase also  sphere zerquetscht  blabla
;; nid.

