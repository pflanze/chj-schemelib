;;; Copyright 2010-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; can't use require and export forms here yet, thus quote them instead:

'(require
 ;; vector-util-1 ;; included directly
 ;; cj-env-1 ditto
 ;; list-util-1 ;; improper-map now copied directly
 )


'(export source?
         source-code
         source-location
         cj-source:port-name?
         location?
         location-container
         container->path
         location-position
         position?
         position-line
         position-line-add
         position-column
         position-column-add
         position-string
         maybe-position-string
         position
         location
         source
         sourcify
         cj-sourcify-deep
         possibly-source
         possibly-sourcify
         cj-possibly-sourcify-deep
         cj-desourcify
         read-all-source
         (type source-error)
         (type location-error)
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
         show-location-error
         show-source-or-location-error
         source-error->string
         show-procedure-location
         source-quote ;; deprecated, use source-quote* instead?
         source-dequote ;; ditto
         source-quote*)


(include "cj-standarddeclares-1--include.scm")

(include "cj-env-1--include.scm")

(include "vector-util-1--include.scm") ;; for vector-map-1

(define (improper-map fn l #!optional (tail '()))
  (let rec ((l l))
    (cond ((null? l)
           tail)
          ((pair? l)
           (cons (fn (car l))
                 (rec (cdr l))))
          (else
           (fn l)))))



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

(define (mk-source/? type?)
  (lambda (v)
    (improper-map (lambda (v)
                    (let ((c (source-code v)))
                      (if (type? c)
                          c
                          v)))
                  (source-code v))))

(define source/clean-keywords
  (mk-source/? keyword?))

(define (dsssl-meta-object? v)
  (or (eq? v #!optional)
      (eq? v #!rest)
      (eq? v #!key)))

(define source/clean-dsssl-meta-objects
  (mk-source/? dsssl-meta-object?))


(define source-location (source-check ##source-locat))

(define (maybe-source-location v)
  (if (source? v)
      (##source-locat v)
      #f))


;; alternative representation that allows column to be omitted:
(define (position* line maybe-column)
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

(define (location* container position)
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


;; can't move cj-io-util.scm's port-name? here due to dependencies;
;; also, this one is faster since it doesn't walk the string.
(define (cj-source:port-name? v)
  (or (string? v)
      (pair? v)))

(define (location? o)
  ;; well.
  (and (vector? o)
       (= (vector-length o) 2)
       (cj-source:port-name? (##vector-ref o 0))
       (fixnum? (##vector-ref o 1))))

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

(define (position-line-add v n)
  (cond ((position? v)
         (fx+ v n))
        ((position*? v)
         (error "unfinished"))
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

(define (position-column-add v n)
  (cond ((position? v)
         (fx+ v (fx* 65536 n)))
        ((position*? v)
         (error "unfinished"))
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


(define (position line column)
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

(define (location container position)
  ((position-check
    (lambda (position)
      (##make-locat container position)))
   position))

(define (source code locat)
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
  (let ((master-loc (source-location master)))
    (let rec ((s s))
      ((lambda (process)
         (if (source? s)
             (source (process (source-code s))
                          (source-location s))
             (source (process s)
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


(define (possibly-source v maybe-loc)
  (if maybe-loc
      (source v maybe-loc)
      v))

(define (possibly-sourcify s master)
  (if (source? master)
      (sourcify s master)
      s))

(define (cj-possibly-sourcify-deep s master)
  (if (source? master)
      (cj-sourcify-deep s master)
      s))

(define (cj-desourcify x)
  (let ((x (if (##source? x) (##source-code x) x)))
    (cond ((pair? x)
           (cons (cj-desourcify (car x))
                 (cj-desourcify (cdr x))))
          ((##vector? x)
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

(define (raise-source-error source message . args)
  ;; how to make Gambit display it? just wrap for now
  (raise (make-source-error source message args)))

(define-type location-error
  id: 248b2471-aa50-4b9a-b84b-78036d27c3c9
  ;;invisible:
  location
  message
  args)

(define (raise-location-error location message . args)
  (raise (make-location-error location message args)))

;; (define source-or-location-error?
;;   (either source-error? location-error?))
(define (source-or-location-error? v)
  (or (source-error? v)
      (location-error? v)))



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


(define (display/maybe-port val maybe-port)
  (display val (or maybe-port (current-output-port))))

(define (display-console val maybe-port)
  (display/maybe-port val (or maybe-port (current-error-port))))

(define (1st-argument/2 val maybe-port)
  val)


;; yes, kinda lame name (historic). Show the location that a location object points to.
(define (show-location-location
         maybe-l
         #!key
         (errstr "*** ERROR IN (just showing location) ")
         (msg "")
         (args '())
         (display/maybe-port display/maybe-port)
         non-highlighting?
         normalize
         maybe-port)
  (display/maybe-port
   (string-append
    errstr
    (if maybe-l
        (location-string maybe-l
                         non-highlighting?: non-highlighting?
                         normalize: normalize)
        "(no-location-information)")
    " -- "
    msg
    (scm:objects->string args prepend: ": ")
    "\n")
   maybe-port))

(define (show-source-location
         s
         #!key
         (errstr "*** ERROR IN (just showing location) ")
         (msg "")
         (args '())
         (display/maybe-port display/maybe-port)
         maybe-port)
  (show-location-location (if (##source? s)
                              (##source-locat s)
                              #f)
                          errstr: errstr
                          msg: msg
                          args: args
                          display/maybe-port: display/maybe-port
                          maybe-port: maybe-port))


;; analog to source-error:
(define (source-warn source message . args)
  (show-source-location source
                        errstr: "*** WARNING IN "
                        msg: message
                        args: args))

;; At runtime use variant for locations instead of source-warn, since
;; locations can be quoted easily, unlike source code:
(define (_location-warn display/maybe-port non-highlighting? normalize?)
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
                                   display/maybe-port: display/maybe-port
                                   non-highlighting?: non-highlighting?
                                   normalize: normalize))))
    (if normalize?
        (lambda (location normalize message . args)
          (cont normalize location message args))
        (lambda (location message . args)
          (cont #f location message args)))))

(define location-warn
  (_location-warn display-console #f #f))
(define location-warn-to-string
  (_location-warn 1st-argument/2 #f #f))

(define location-warn*
  (_location-warn display-console #t #f))
(define location-warn-to-string*
  (_location-warn 1st-argument/2 #t #f))

(define location-warn-to-string/normalize
  (_location-warn 1st-argument/2 #f #t))

;; test see in simple-match.scm

(define (show-source-error e #!optional maybe-port)
  (show-source-location (source-error-source e)
                        errstr: "*** ERROR IN source, "
                        msg: (source-error-message e)
                        args: (source-error-args e)
                        maybe-port: maybe-port))

;; copy-paste with s/source/location/g
(define (show-location-error e #!optional maybe-port)
  (show-location-location (location-error-location e)
                          errstr: "*** ERROR IN location, "
                          msg: (location-error-message e)
                          args: (location-error-args e)
                          maybe-port: maybe-port))

(define (show-source-or-location-error e #!optional maybe-port)
  (cond ((source-error? e)
         (show-source-error e maybe-port))
        ((location-error? e)
         (show-location-error e maybe-port))
        (else
         (error "show-source-or-location-error: neither source- nor location-error:" e))))

(define (source-error->string e)
  (show-source-location (source-error-source e)
                        errstr: "*** ERROR IN syntax, "
                        msg: (source-error-message e)
                        args: (source-error-args e)
                        display/maybe-port: 1st-argument/2))

(define (show-procedure-location p)
  (if (procedure? p)
      (show-location-location (##procedure-locat p)
                              errstr: "*** DEFINED IN "
                              msg: "as"
                              args: (list p)
                              display/maybe-port: display/maybe-port)
      (error "not a procedure:" p)))


(define (source-quote v)
  (object->u8vector v))

(define (source-dequote v)
  (u8vector->object v))

;; why the hell did I do the above which aren't a full abstraction?
;; And not this (ah, also compare to quote-source):

(define (source-quote* v)
  `(u8vector->object ',(object->u8vector v)))

