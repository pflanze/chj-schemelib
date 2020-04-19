;; Added parts are
;;; Copyright 2018-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.

;; Parts copied from Gambit and modified are copyright by Marc Feeley,
;; for the license see Gambit Scheme distribution.


;; Since Gambit's built-ins don't offer predicates (it speficies
;; constructor: #f, and/or might use macros), define its internals
;; again. Copy-pastes from Gambit source, modified to use define-type,
;; and not omit constructors.

;; Use namespacing just to be sure, and because accessors already
;; exist as built-ins. This works thanks to using the same
;; |id|:. (Should probably really patch Gambit instead. Although the
;; automatic .show creation below would still be needed.)


(require cj-phasing
	 show
	 (cj-env symbol-append)
	 dot-oo
         define-macro-star
         test)

(export error-exception error-exception?
	no-such-file-or-directory-exception no-such-file-or-directory-exception?
	heap-overflow-exception heap-overflow-exception?
	stack-overflow-exception stack-overflow-exception?
	nonprocedure-operator-exception nonprocedure-operator-exception?
	wrong-number-of-arguments-exception wrong-number-of-arguments-exception?
	keyword-expected-exception keyword-expected-exception?
	unknown-keyword-argument-exception unknown-keyword-argument-exception?
	cfun-conversion-exception cfun-conversion-exception?
	sfun-conversion-exception sfun-conversion-exception?
	multiple-c-return-exception multiple-c-return-exception?
	number-of-arguments-limit-exception number-of-arguments-limit-exception?
	type-exception type-exception?
	os-exception os-exception?
	no-such-file-or-directory-exception no-such-file-or-directory-exception?

	(method error-exception.show)
	(method heap-overflow-exception.show)
	(method stack-overflow-exception.show)
	(method nonprocedure-operator-exception.show)
	(method wrong-number-of-arguments-exception.show)
	(method keyword-expected-exception.show)
	(method unknown-keyword-argument-exception.show)
	(method cfun-conversion-exception.show)
	(method sfun-conversion-exception.show)
	(method multiple-c-return-exception.show)
	(method number-of-arguments-limit-exception.show)
	(method type-exception.show)
	(method os-exception.show)
	(method no-such-file-or-directory-exception.show)

        (macro define-show-nonserializable)
        (methods input-port.show
                 output-port.show)
        input-port#
        output-port#)


(define-type gambit-error#exception
  id: 0bf9b656-b071-404a-a514-0fb9d05cf518
  ;; no need to construct top-level class, i.e. abstract
  constructor: #f
  extender: define-type-of-gambit-error#exception
  opaque:)

;; adapted from lib/_nonstd#.scm
(define-type-of-gambit-error#exception gambit-error#error-exception
  id: efe252c3-9391-4acf-993b-1ad2a9035636
  constructor: gambit-error#error-exception
  opaque:

  (message    unprintable: read-only:)
  (parameters unprintable: read-only:)
  )

(define error-exception gambit-error#error-exception)


;; For the other classes, use automatic transformation:

(compile-time
 (define (gambit-error:define-library-type-of-exception-expand
	  name
	  #!key
	  id
	  constructor
	  opaque ;; sigh, :opaque kind of flag, meh, have to give it
	  #!rest
	  r)

   (define (cont defs)
     (let ((fieldnames (map car defs))
	   (accessor
	    (lambda (fieldname)
	      (symbol-append name "-" fieldname)))
	   (S (gensym)))

       `(begin
	  (define-type-of-gambit-error#exception ,(symbol-append "gambit-error#" name)
	    id: ,id
	    constructor: ,name ;; make- ?
	    opaque:	       ;; ?
	    ;; (map (lambda (fieldname)
	    ;; 	      `(,fieldname unprintale: read-only:))
	    ;; 	    fieldnames)
	    ;; ^not accepted.
	    ,@fieldnames)
		    
	  (define. (,(symbol-append name '.show) ,S show)
	    (list ',name
		  ,@(map (lambda (fieldname)
			   `(show (,(accessor fieldname) ,S)))
			 fieldnames))))))
      
   (if opaque
       (cont (cons opaque r))
       (if (eq? (car r) opaque:)
	   (cont (cdr r))
	   (cont r)))))

(define-macro (define-library-type-of-exception . args)
  (apply gambit-error:define-library-type-of-exception-expand args))

(include "gambit-error--include.scm")

(TEST
 > (with-exception-catcher show (& (error "hum f" #f)))
 (error-exception "hum f" (list #f))
 > (with-exception-catcher show (& (car "hum f" #f)))
 (wrong-number-of-arguments-exception car (list "hum f" #f))
 > (define e (eval #))
 > (wrong-number-of-arguments-exception? e)
 > (show (wrong-number-of-arguments-exception-procedure e))
 car)



;; Various non-serializable show stuff:
;; sigh, automatic pls? Do I have another place already for this?

(define (show-nonserializable/typename typename#)
  (lambda (v show)
    `(,typename# ,(object->serial-number v))))

(define (unshow-nonserializable/type pred)
  (lambda (serial)
    (let ((v (serial-number->object serial)))
      (if (pred v)
          v
          (error "serial number does not represent an object of type:"
                 serial pred)))))

(define-macro* (define-show-nonserializable typename)
  "Define a typename.show, showing (typename# 1234)."
  ;; This is nothing new, todo: 'track down' the places where I did
  ;; this already manually.
  (assert* symbol? typename
           (lambda (typename)
             (let ((typename# (symbol-append typename "#"))
                   (pred (symbol-append typename "?")))
               `(begin
                  (define. ,(symbol-append typename ".show")
                    (show-nonserializable/typename ',typename#))
                  (define ,typename#
                    (unshow-nonserializable/type ,pred)))))))


(define-show-nonserializable input-port)
(define-show-nonserializable output-port)
