;;; Copyright 2010-2014 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; can't use require form here yet
;; (require
;; 	 ;; vector-util-1 ;; included directly
;; 	 ;; list-util-1 ;; improper-map now copied directly
;; 	 )


(define (source? o)
  (##source? o))

(define (check type? typename)
  (let ((msg (string-append "not a "typename" object:")))
    (lambda (proc)
      (lambda (o)
	(if (type? o)
	    (proc o)
	    (error msg o))))))

(define source-check (check source? "source"))

(define (source-code x)
  (if (##source? x)
      (##source-code x)
      (begin
	;;(warn "not source code:" x)
	x)))

(define (mk-source-code/? type?)
  (lambda (v)
    ;; need a copy of improper-map (~bootstrapping issue):
    (define (improper-map fn l #!optional (tail '()))
      (let rec ((l l))
	(cond ((null? l)
	       tail)
	      ((pair? l)
	       (cons (fn (car l))
		     (rec (cdr l))))
	      (else
	       (fn l)))))
    ;; /copy
    (improper-map (lambda (v)
		    (let ((c (source-code v)))
		      (if (type? c)
			  c
			  v)))
		  (source-code v))))

(define source-code/clean-keywords
  (mk-source-code/? keyword?))

(define (meta-object? v)
  (or (eq? v #!optional)
      (eq? v #!rest)
      (eq? v #!key)))

(define source-code/clean-meta-objects
  (mk-source-code/? meta-object?))


(define source-location (source-check ##source-locat))

(define (maybe-source-location v)
  (if (source? v)
      (##source-locat v)
      #f))


(define (location? o)
  ;; well.
  (and (vector? o)
       (= (vector-length o) 2)))

(define location-check (check location? "location"))

(define location-container (location-check ##locat-container))

(define (container->path container)
  (if (with-exception-catcher (lambda (e) #f) 
			      (lambda () (eval '##container->path-hook)))
      (or (##container->path-hook container)
          container)
    container))


;; he does it yet more nested (not sure it's warranted here):
(define location-position (location-check ##locat-position))

(define (position? o)
  ;; hmm
  (##fixnum? o))

(define position-check (check position? "position"))

(define position-line
  (position-check (lambda (pos)
		    (+ 1 (bitwise-and pos 65535)))))
(define position-column
  (position-check (lambda (pos)
		    (+ 1 (quotient pos 65536)))))


(define (make-position line column)
  (let ((l (- line 1))
	(c (- column 1)))
    (if (<= 0 l 65535)
	(if (<= 0 c)
	    (let ((r (bitwise-ior l
				  (arithmetic-shift c 16))))
	      (if (##fixnum? r)
		  r
		  (error "column out of range:" column)))
	    (error "column out of range:" column))
	(error "line out of range:" line))))

(define (make-location container position)
  ((position-check
    (lambda (position)
      (##make-locat container position)))
   position))

(define (make-source code locat)
  (let ((cont
	 (lambda (locat)
	   (##make-source code locat))))
    (if locat
	((location-check
	  cont)
	 locat)
	;; have to accept #f as locat, just as ##make-source
	(cont locat))))

(define (sourcify x src)
  ((source-check
    (lambda (src)
      (##sourcify x src)))
   src))

(define (cj-sourcify-deep s master)
    ;; need a copy of improper-map (~bootstrapping issue):
    (define (improper-map fn l #!optional (tail '()))
      (let rec ((l l))
	(cond ((null? l)
	       tail)
	      ((pair? l)
	       (cons (fn (car l))
		     (rec (cdr l))))
	      (else
	       (fn l)))))
    ;; /copy
    (##include "vector-util-1.scm")
  (let ((master-loc (source-location master)))
    (let rec ((s s))
      ((lambda (process)
	 (if (source? s)
	     (make-source (process (source-code s))
			  (source-location s))
	     (make-source (process s)
			  master-loc)))
       ;; "where process ="
       (lambda (c)
	 (cond ((pair? c)
		(improper-map rec c))
	       ((vector? c)
		;; quoted vectors (syntax for constants)
		(vector-map-1 rec c))
	       ((box? c)
		(box (rec (unbox c))))
	       ((or (##structure? c)
		    ;; some more?
		    )
		;; v- again, how to give location info/exceptions generally?
		(error "cj-sourcify-deep: type of this object unexpected:" c))
	       (else
		;; also e.g. (u8vector? c):
		;; doesn't contain anything, so:
		c)))))))


(define (possibly-sourcify s master)
  (if (source? master)
      (sourcify s master)
      s))

(define (cj-possibly-sourcify-deep s master)
  (if (source? master)
      (cj-sourcify-deep s master)
      s))

(define (cj-desourcify x)
    (##include "vector-util-1.scm")
  (let ((x (if (##source? x) (##source-code x) x)))
    (cond ((pair? x)
	   (cons (cj-desourcify (car x))
		 (cj-desourcify (cdr x))))
	  ((vector? x)
	   (vector-map-1 cj-desourcify x))
	  ((box? x)
	   (box (cj-desourcify (unbox x))))
	  ;; XX more?
	  (else
	   x))))


(define (read-all-source #!optional (port (current-input-port)))
  ;; NOTE: does NOT return an expr. It returns a *list* of expr's.
  (let recur ()
    (let ((expr (##read-expr-from-port port)))
      (if (eof-object? expr) '()
	  (cons expr (recur))))))



;;
;;; source errors
;;

(define-type source-error
  id: e7f33085-18d1-4220-b542-0e4500f7f001
  ;;invisible:
  source  ;;_location
  message
  args)

(define (source-error source message . args)
  ;; how to make Gambit display it? just wrap for now
  (raise (make-source-error source message args)))

;; (define (location-error location message . args)
;;   (raise (make-source-error source message args)))
;; todo finish (lost-on-tie?)

(define (pos:line pos)
  (+ 1 (bitwise-and pos 65535)))
(define (pos:col pos)
  (+ 1 (quotient pos 65536)))

;; yes, kinda lame name (historic). Show the location that a location object points to.
(define (show-location-location
	 l
	 #!key
	 (errstr "*** ERROR IN (just showing location) ")
	 (msg "")
	 (args '())
	 (display display))
  (let ((cont
	 (lambda (c maybe-p)
	   (let ((cont
		  (lambda (line col)
		    (display (string-append
			      errstr
			      (scm:object->string c)
			      "@"
			      (scm:object->string line)
			      "."
			      (scm:object->string col)
			      " -- "
			      msg
			      (scm:objects->string args prepend: ": ")
			      "\n")))))
	     (if maybe-p
		 (let ((p maybe-p))
		   (cont (pos:line p)
			 (pos:col p)))
		 (cont "?"
		       "?"))))))
    (if l
	(if (location? l)
	    (cont (location-container l)
		  (location-position l))
	    (error "not a location object:" l))
	(cont '(no-location-information)
	      #f))))

(define (show-source-location
	 s
	 #!key
	 (errstr "*** ERROR IN (just showing location) ")
	 (msg "")
	 (args '())
	 (display display))
  (show-location-location (if (##source? s)
			      (##source-locat s)
			      #f)
			  errstr: errstr
			  msg: msg
			  args: args
			  display: display))


;; analog to source-error:
(define (source-warn source message . args)
  (show-source-location source
			errstr: "*** WARNING IN "
			msg: message
			args: args))

;; At runtime use variant for locations instead of source-warn, since
;; locations can be quoted easily, unlike source code:
(define (_location-warn display)
  (lambda (location message . args)
    (show-location-location location
			    errstr: "*** WARNING IN "
			    msg: message
			    args: args
			    display: display)))
(define location-warn (_location-warn display))
(define location-warn-to-string (_location-warn values))

;; test see in simple-match.scm

(define (show-source-error e)
  (show-source-location (source-error-source e)
			errstr: "*** ERROR IN syntax, "
			msg: (source-error-message e)
			args: (source-error-args e)))

(define (source-error->string e)
  (show-source-location (source-error-source e)
			errstr: "*** ERROR IN syntax, "
			msg: (source-error-message e)
			args: (source-error-args e)
			display: values))

(define (show-procedure-location p)
  (if (procedure? p)
      (show-location-location (##procedure-locat p)
			      errstr: "*** DEFINED IN "
			      msg: "as"
			      args: (list p)
			      display: display)
      (error "not a procedure:" p)))


(define (source-quote v)
  (object->u8vector v))

(define (source-dequote v)
  (u8vector->object v))


