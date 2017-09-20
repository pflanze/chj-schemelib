;;; Copyright 2010-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; can't use require and export forms here yet, thus quote them instead:

'(require
 ;; vector-util-1 ;; included directly
 ;; improper-length ;; dito
 ;; cj-env-1 dito
 ;; list-util-1 ;; improper-map now copied directly
 )


'(export source?
	 source-code
	 source-location
	 location?
	 location-container
	 container->path
	 location-position
	 position?
	 position-line
	 position-column
	 position-string
	 maybe-position-string
	 make-position
	 make-location
	 make-source
	 sourcify
	 cj-sourcify-deep
	 possibly-sourcify
	 cj-possibly-sourcify-deep
	 cj-desourcify
	 read-all-source
	 (type source-error)
	 location-string
	 show-location-location
	 show-source-location
	 source-warn
	 location-warn
	 location-warn-to-string
	 location-warn*
	 location-warn-to-string*
	 location-warn-to-string/normalize
	 show-source-error
	 source-error->string
	 show-procedure-location
	 source-quote ;; deprecated, use source-quote* instead?
	 source-dequote ;; dito
	 source-quote*
	 ;; included from improper-length.scm
	 improper-length)


(include "cj-env-1.scm")


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


;; alternative representation that allows column to be omitted:
(define (make-position* line maybe-column)
  (if (<= 0 line)
      (if (or (not maybe-column)
	      (<= 0 maybe-column))
	  (vector 'position* line maybe-column)
	  (error "column out of range:" maybe-column))
      (error "line out of range:" line)))
(define (position*? v)
  (and (vector? v)
       (= (vector-length v) 3)
       (eq? (vector-ref v 0) 'position*)))
(define (@position*-line l)
  (vector-ref l 1))
(define (@position*-maybe-column l)
  (vector-ref l 2))

(define (make-location* container position)
  (if (position*? position)
      (vector 'location* container position)
      (error "not a position* object:" position)))
(define (location*? v)
  (and (vector? v)
       (= (vector-length v) 3)
       (eq? (vector-ref v 0) 'location*)))
(define (@location*-container l)
  (vector-ref l 1))
(define (@location*-position l)
  (vector-ref l 2))



(define (location? o)
  ;; well.
  (and (vector? o)
       (= (vector-length o) 2)))

;; unlike location*?, this also accepts location? objects, and unlike
;; location?, this also accepts location*? objects as long as they
;; have a column (are usable in the location API):
(define (location?* v)
  (or (location? v)
      (and (location*? v)
	   (@position*-maybe-column (@location*-position v))
	   #t)))

(define location-check (check location? "location"))

(define (location-container v)
  (cond ((location? v)
	 (##locat-container v))
	((location*? v)
	 (@location*-container v))
	(else
	 (error "not a location object:" v))) )

(define (container->path container)
  (if (with-exception-catcher (lambda (e) #f) 
			      (lambda () (eval '##container->path-hook)))
      (or (##container->path-hook container)
          container)
    container))

(define (location-position v)
  (cond ((location? v)
	 (##locat-position v))
	((location*? v)
	 (@location*-position v))
	(else
	 (error "not a location object:" v))))

(define (position? o)
  ;; hmm
  (##fixnum? o))

(define position-check (check position? "position"))

(define (position-line v)
  (cond ((position? v)
	 (+ 1 (bitwise-and v 65535)))
	((position*? v)
	 (@position*-line v))
	(else
	 (error "not a position object:" v))))

(define (position-column v)
  (cond ((position? v)
	 ;; XX should use bit shift instead
	 (+ 1 (quotient v 65536)))
	((position*? v)
	 (or (@position*-maybe-column v)
	     (error "position-column: position* does not contain column:" v)))
	(else
	 (error "not a position object:" v))))

;; copy-paste
(define (position-maybe-column v)
  (cond ((position? v)
	 ;; XX should use bit shift instead
	 (+ 1 (quotient v 65536)))
	((position*? v)
	 (@position*-maybe-column v))
	(else
	 (error "not a position object:" v))))


(define (position-string pos)
  (string-append (scm:object->string (position-line pos))
		 "."
		 (scm:object->string (position-column pos))))

(define (maybe-position-string maybe-pos)
  (if maybe-pos
      (position-string maybe-pos)
      "?.?"))


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


(define (location-string l #!key non-highlighting? normalize omit-column?)
  (let ((c (location-container l)))
    (string-append (scm:object->string
		    (if (and normalize (string? c))
			(normalize c)
			c))
		   (if non-highlighting?
		       " @ "
		       "@")
		   (let ((pos (location-position l)))
		     (if omit-column?
			 (object->string (position-line pos))
			 (position-string pos))))))


;; yes, kinda lame name (historic). Show the location that a location object points to.
(define (show-location-location
	 maybe-l
	 #!key
	 (errstr "*** ERROR IN (just showing location) ")
	 (msg "")
	 (args '())
	 (display display)
	 non-highlighting?
	 normalize)
  (display (string-append
	    errstr
	    (if maybe-l
		(location-string maybe-l
				 non-highlighting?: non-highlighting?
				 normalize: normalize)
		"(no-location-information)")
	    " -- "
	    msg
	    (scm:objects->string args prepend: ": ")
	    "\n")))

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
(define (_location-warn display non-highlighting? normalize?)
  ;; non-highlighting?==#t means it will *not* write a message in a
  ;; way that emacs shows a window at that location (good for
  ;; e.g. showing *known* test failures).
  (let ((cont
	 (lambda (normalize location message args)
	   (show-location-location location
				   errstr: (if non-highlighting?
					       "*** Warning in "
					       "*** WARNING IN ")
				   msg: message
				   args: args
				   display: display
				   non-highlighting?: non-highlighting?
				   normalize: normalize))))
    (if normalize?
	(lambda (location normalize message . args)
	  (cont normalize location message args))
	(lambda (location message . args)
	  (cont #f location message args)))))
(define location-warn (_location-warn display #f #f))
(define location-warn-to-string (_location-warn values #f #f))

(define location-warn* (_location-warn display #t #f))
(define location-warn-to-string* (_location-warn values #t #f))

(define location-warn-to-string/normalize (_location-warn values #f #t))

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

;; why the hell did I do the above which aren't a full abstraction?
;; And not this (ah, also compare to quote-source):

(define (source-quote* v)
  `(u8vector->object ',(object->u8vector v)))

