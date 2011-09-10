;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Debugging infrastructure for lazy code:

(##namespace ("debuggable#"))
(##include "~~lib/gambit#.scm")
(##namespace ("debuggable#" delay force))

(define-type debuggable-promise
  promise
  capturectx)

(define-macro* (delay expr)
  (define capturectx (gensym 'capturectx))
  `((let ()
      (##namespace (""))
      continuation-capture)
    (lambda (,capturectx)
      (debuggable#make-debuggable-promise
       (##delay
	,expr)
       ,capturectx))))

(define (@force1 v)
  (##force (debuggable-promise-promise v)))

(define (force1 v)
  (if (debuggable-promise? v)
      (@force1 v)
      v))

(define (force v)
  (if (debuggable-promise? v)
      (force (@force1 v))
      v))

(define (promise? v)
  (debuggable-promise? v))

(##namespace (""));; well. whatever jazz would use, if at all.
(##namespace ("debuggable#" delay force1 force promise? repl-within-debuggable))


