;;; Copyright 2017-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Wrap Gambit's primordial-exception-handler so that the exception
;; value can be accessed (e.g. to inspect it in a way that doesn't
;; shorten the message).

(require (cj-source source-error? show-source-error
		    location-string)
	 (predicates-1 any?)
	 dot-oo
         (list-util let-pair)
         (cj-env when)
         (cj-env-2 unless) ;; what mess
         (string-util-4 string-empty?)
         ;; (oo-lib-string string.last)
         (cj-functional-2 =>))

(export current-exception
	write-exception-message
	cj-exception-handler:activate!
	cj-exception-handler:activate-if-primordial!
        (method any.maybe-exception-message)
        (method error-exception.maybe-exception-message))


;; Copy from oo-lib-string (oo-vector-lib) to allow this module to be
;; loaded from easy-2:
(define string-last
  (lambda (v)
    (let ((len (string-length v)))
      (if (zero? len)
          (error "string-last: string is empty")
          (string-ref v (dec len))))))


(define current-exception (make-parameter #f))

;; method that should return a list with the first item being a
;; message string without ":", a ": " is appended by the printer. And
;; then a list of values, optionally prepended by a keyword, which
;; will be written via write or pretty-print (i.e. no |show| call is
;; being applied, OK?)
(define. (any.maybe-exception-message _) #f)

;; And hey, why not start adding the method to Gambit's objects, too,
;; so that I can call .maybe-exception-message in other contexts, too?
(define. (error-exception.maybe-exception-message e)
  (cons (error-exception-message e)
        (error-exception-parameters e)))


;; without a newline afterwards, i.e. do not wrap arond lines, OK?
(define (write-exception-message v #!optional (p (current-output-port)))
  (let ((oldrt (output-port-readtable p)))
    ;; we don't want lots of output in error display:
    (output-port-readtable-set!
     p
     (=> oldrt
         (readtable-max-unescaped-char-set #\U0010ffff)
         (readtable-max-write-level-set 6)
         (readtable-max-write-length-set 10)))
    (cond ((and (pair? v) (list? v))
           (let-pair ((a v*) v)
                     (display a p)
                     (when (pair? v*)
                       (if (string? a)
                           (unless (string-empty? a)
                             ;; XX is this evil? For backwards
                             ;; compat with Gambit's behaviour
                             ;; of showing messages from
                             ;; |error|
                             (unless (eq? (string-last a) #\:)
                               (display #\: p))
                             (display #\space p))
                           (display #\space p))
                       (let lp ((l v*))
                         (when (pair? l)
                           (let-pair ((a r) l)
                                     (write a p)
                                     (when (pair? r)
                                       (display #\space p)
                                       (lp r))))))))
          ((string? v)
           (display v p))
          (else
           ;; just a safe fallback, don't use, OK?
           (write v p)))
    (output-port-readtable-set! p oldrt)))


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
    (cond ((source-or-location-error? e)
	   (show-source-or-location-error e p)
	   (##repl))
	  ((.maybe-exception-message e)
	   => (lambda (msg)
		(continuation-capture
		 (lambda (cont)
		   (display "*** ERROR IN " p)
		   (cond ((##continuation-creator cont)
			  => (lambda (creator)
                               (display creator p)
                               (display ", " p))))
                   (display (cond ((##continuation-locat cont)
                                   => location-string)
                                  (else
                                   "(continuation without location)"))
                            p)
		   (display " -- " p)
		   (write-exception-message msg p)
		   (newline p)
		   (##repl)))))
	  (else
	   (primordial-exception-handler e)))))


(define (cj-exception-handler:activate!)
  (current-exception-handler cj-exception-handler))

(define (cj-exception-handler:activate-if-primordial!)
  (if (eq? (current-exception-handler) primordial-exception-handler)
      (begin
	(cj-exception-handler:activate!)
	#t)
      #f))

(cj-exception-handler:activate-if-primordial!)
