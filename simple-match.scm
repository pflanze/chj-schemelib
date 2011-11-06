;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (lib.define-macro-star)
	 (lib.cj-phasing)
	 (lib.test))


(both-times
 
(define-type source-error
  id: e7f33085-18d1-4220-b542-0e4500f7f001
  ;;invisible:
  source  ;;_location
  message
  args)

(define (source-error source message . args)
  ;; how to make Gambit display it? just wrap for now
  (raise (make-source-error source message args)))

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
;;(define (source-warn source )) or rather,
;; for locations since locations can be quoted easily, unlike source code:
(define (_location-warn display)
  (lambda (location message . args)
    (show-location-location location
			    errstr: "*** WARNING IN "
			    msg: message
			    args: args
			    display: display)))
(define location-warn (_location-warn display))
(define location-warn-to-string (_location-warn values))

(TEST
 > (location-warn-to-string '#((console) 3) "hallo" 1)
 "*** WARNING IN (console)@4.1 -- hallo: 1\n"
 )

; (define (warn* message . args)
;   (continuation-capture
;    (lambda (cont)
;      )))
;or, 'simpler':
(define-macro* (warn* message . args)
  (let ((loc (source-location stx)))
    `(location-warn ',loc
		    ,message
		    ,@args)))
 
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


;; rely on improper-length having been loaded by define-macro*.scm

)

(TEST
 > (match* '(1 2 3 4) ((a b . c) c) (s s))
 (3 4)
 > (match* '(1 2 3 4) ((a b . c) (vector a b c)) (s s))
 #(1 2 (3 4))
 > (match* '(1 2 3) ((a b . c) (vector a b c)) (s s))
 #(1 2 (3))
 > (match* '(1 2) ((a b . c) (vector a b c)) (s s))
 #(1 2 ())
 > (match* '(1) ((a b . c) (vector a b c)) (s s))
 (1)
)

(define-macro* (match* input* . clauses*)
  ;; only supports flat list matching for now
  ;; although including improper lists
  ;; not even anything other than just binding variables.
  ;; also doesn't check for errors in clauses syntax very well.
  (let ((V* (gensym 'v*))
	(V (gensym 'v))
	(LEN (gensym 'len)))
    `(let* ((,V* ,input*)
	    (,V (source-code ,V*))
	    (,LEN (improper-length ,V)))
       (if (negative? ,LEN)
	   (source-error ,V* "not a proper list")
	   ,(let rec ((clauses clauses*))
	      (cond ((null? clauses)
		     `(source-error ,V*
				    ,(let ((str (scm:objects->string
						 (map car (map cj-desourcify clauses*))
						 separator: " | ")))
				       (string-append
					"no match, expecting "
					(if (> (length clauses*) 1)
					    (string-append "any of " str)
					    str)))))
		    ((pair? clauses)
		     (let* ((clause* (car clauses))
			    (clause (source-code clause*))
			    (pro* (car clause))
			    (body (cdr clause))	;;(XX require body to be non-null?)
			    (pro (source-code pro*))
			    (len (improper-length pro))
			    )
		       (if (not (negative? len))
			   `(if (= ,LEN ,len)
				;; XX just *assume* that clause contents are just symbols for now
				,(let rec2 ((pro pro))
				   (cond ((null? pro)
					  (cons 'begin body))
					 ((pair? pro)
					  `(let* ((,(car pro) (car ,V))
						  ;; reuse var name (shadowing):
						  (,V (cdr ,V)))
					     ,(rec2 (cdr pro))))))
				,(rec (cdr clauses)))
			   ;; otherwise var arg match:
			   ;; (could optimize: if len is -1, the clause matches everything, no test needed and stop recursing, ignore remaining clauses (XXX croak about it when there *are* further clauses))
			   `(if (>= ,LEN ,(dec (- len)))
				;; XX dito above
				,(let rec2 ((pro pro))
				   (cond ((pair? pro)
					  `(let* ((,(car pro) (car ,V))
						  ;; reuse var name (shadowing):
						  (,V (cdr ,V)))
					     ,(rec2 (cdr pro))))
					 (else
					  `(let ((,pro ,V))
					     ,@body))))
				,(rec (cdr clauses))))))
		    (else
		     ;; can't use source-error here yet (because it has not been defined in this phase)
		     (error "invalid match syntax: expecting list of clauses, got:" clauses))))))))

;; require input to be a proper list (complain otherwise):
;; currently just an alias for match*, but I might change match* some time.
(define-macro* (match-list* input* . clauses)
  `(match* ,input* ,@clauses))

(TEST
> (match* '() (() (list "one item" )))
("one item")
> (match* '(2) ((x) (list "one item" x)))
("one item" 2)
; > (match '() ((x) (list "one item" x)))
; *** ERROR IN (console)@25.1 -- no match for: ()
; 1> 
> (match* '(3 4) ((x) (list "one item" x)) ((x y) (list "two items" x y)))
("two items" 3 4)
> (match* '(3) ((x) (list "one item" x)) ((x y) (list "two items" x y)))
("one item" 3)
> (match* '() ((x) (list "one item" x)) ((x y) (list "two items" x y)) (() '(well)))
(well)
> (match* '(1 2 3 4) ((a b c) c) (s s))
(1 2 3 4)
> (match* '(1 2 3) ((a b c) c) (s s))
3
 )

(compile-time
 (define (assert*-expand desourcify gen-full-desourcify/1 pred val yes-cont no-cont)
   (define V* (gensym 'v*))
   (define V (gensym 'v))
   `(let* ((,V* ,val)
	   (,V (,desourcify ,V*)))
      (if (,pred ,V)
	  (,yes-cont ,V)
	  ,(if (source-code no-cont)
	       no-cont
	       `(source-error ,V*
			      ,(string-append "does not match "
					      (scm:object->string
					       (cj-desourcify pred))
					      " predicate")
			      ,(gen-full-desourcify/1 V* V)))))))

(define-macro* (assert* pred val yes-cont #!optional no-cont)
  (assert*-expand 'cj-desourcify
		  (lambda (V* V)
		    V)
		  pred val yes-cont no-cont))

;; only remove location information 1 level (uh, better names?)
(define-macro* (assert*1 pred val yes-cont #!optional no-cont)
  (assert*-expand 'source-code
		  (lambda (V* V)
		    `(cj-desourcify ,V*))
		  pred val yes-cont no-cont))

;; different from assert* in two ways (1) pass the unwrapped result in
;; 'the same variable as' v instead of expecting a function, (2) evals
;; the input first.
(define-macro* (assert** pred var yes-expr #!optional no-expr)
  (assert* symbol? var
	   (lambda (_)
	    `(assert* ,pred (eval ,var)
		      (lambda (,var)
			,yes-expr)
		      ,@(if (source-code no-expr)
			    (list no-expr)
			    (list))))))

