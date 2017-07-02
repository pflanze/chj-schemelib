;;; Copyright 2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Wrap Gambit's primordial-exception-handler so that the exception
;; value can be accessed (e.g. to inspect it in a way that doesn't
;; shorten the message).

(require)

(define current-exception (make-parameter #f))

(define (cj-exception-handler e)
  ;; Show every object with a serial number handle first, so that it
  ;; can be accessed as the original value.
  ;; (XX BAD: immediate objects are never freed from the serialization
  ;; table, right? But, shouldn't that be handled there instead?)
  (let ((p (console-port)))
    ;; right it's the console-port ?
    (display "cj-exception-handler: exception #" p)
    (display (object->serial-number e) p)
    (newline p)
    ;; (Call previous handler instead of the primordial one ?)
    (primordial-exception-handler e)))


(define (cj-exception-handler:activate!)
  (current-exception-handler cj-exception-handler))

(define (cj-exception-handler:activate-if-primordial!)
  (if (eq? (current-exception-handler) primordial-exception-handler)
      (begin
	(cj-exception-handler:activate!)
	#t)
      #f))

