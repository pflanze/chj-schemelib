;;; Copyright 2014-2016 by Christian Jaeger, ch at christianjaeger ch

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.



;; part of the `fallible` module that can be implemented without `dot-oo`,
;; to satisfy `cj-typed`'s dependency

(require cj-struct
	 (cj-functional-2 =>*)
	 (cj-env symbol-value-or))

(export fallible?
	fallible-string)

(define-struct fallible
  constructor-name: fallible
  stack)

(define (fallible-show v)
  ;; XX but now need to rely on dot-oo or cycle workaround hack
  ;; *anyway*. Bah. TODO: avoid relying on cj-typed from dot-oo ? Or
  ;; split cj-typed ?
  (let ((.show (symbol-value-or
		'.show
		(lambda ()
		  (lambda (v)
		    ;;  in hack to workaround dependency cycle,
		    (error ".show not yet defined"))))))
    (map .show
	 (fallible-stack v))))

(define fallible-string
  (=>* fallible-show object->string))
