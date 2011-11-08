;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (lib.monad.syntax))


;; IO monad

(define (io:>> a b)
  (lambda ()
    ;; leading back to using internal sequencing offered by the 'host
    ;; language' (and the thunk feature)
    (a)
    (b)))

(define (io:>>= a b)
  (lambda ()
    (let ((val (a)))
      ((b val)))))

(define (io:return val)
  (lambda ()
    val))



;; a small library of some 

(define (io:print str)
  (lambda ()
    (print str)))

(define (io:println str)
  (lambda ()
    (println str)))

(define (io:read-line)
  (lambda ()
    (read-line)))

(define io:read-line
  (lambda ()
   (lambda ()
     (read-line))))

(define io:read-line*
  (lambda ()
    (read-line)))

;; same as (define io:read-line* read-line)

(define io:noop
  (lambda ()
    (void)))
;; or, same thing,
(define io:noop*
  (io:return (void)))

;; run an IO monad:

(define (runio m)
  (read-line) ;; (workaround to drop empty first input line)
  (m))

