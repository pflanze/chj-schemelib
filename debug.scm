;;; Copyright 2016-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version. Also, as a special exception to the
;;;    terms of the GPL you can link it with any code produced by Categorical
;;;    Design Solutions Inc. from Quebec, Canada.

(require easy
	 named
	 (cj-warn warn
		  port-add-hook!)
	 (oo-lib-string strings-append)
         test
         (cj-port %with-error-to-string))


(export STRING
	(macro DEBUG)
	(macro DEBUG*)
	(macro T) ;; trace call
        (macro TT) ;; trace tail call

	*debug* ;; well, by alias? Hey, have syntax that sets compile
		;; time variables scoped by the compilation unit?
	2>
	2force

	*single-step?* ;; 
	warn-stop-on-line!
	
	;; UNTESTED and useless?
	(macro STOP-at-line)
	(inline debug:stop-at-line))


;; other lib?

;; (def string.string identity) ;; huh really didn't do that so far?
;; ok, bad, usually stringify, not this ? But, C++ does do this no? <<
;; action. ?

(def (STRING . vals)
     (strings-append (map (lambda (v)
			    (cond ((string? v)
				   v)
				  (else
				   (.string v))))
			  vals)))

;; / other lib?


;; statements below that level remain quiet; #f means don't compile
;; debugging statements into the code at all
(def *debug* 2)

;; whether
(def *debug/continuations* #t)


(def (debug:parse-level form want-marker? cont) ;; form/level/maybe-marker
     (def (with-level level form)
	  (if (and want-marker? (pair? form))
	      (let-pair ((b form*) form)
			(let ((B (source-code b)))
			  (if (string? B)
			      (cont form* level B)
			      (cont form level #f))))
	      (cont form level #f)))
     (if (pair? form)
	 (let-pair ((a form*) form)
		   (let ((A (source-code a)))
		     (if (real? A)
			 (with-level A form*)
			 (with-level 1 form))))
	 (cont form 1 #f)))

(TEST
 ;; DEBUG
 > (debug:parse-level '((a 1) b) #f list)
 (((a 1) b) 1 #f)
 > (debug:parse-level '("uh" (a 1) b) #f list)
 (("uh" (a 1) b) 1 #f)
 > (debug:parse-level '(3 "uh" (a 1) b) #f list)
 (("uh" (a 1) b) 3 #f)
 > (debug:parse-level '(3 (a 1) b) #f list)
 (((a 1) b) 3 #f)

 ;; T
 > (debug:parse-level '((a 1) b) #t list)
 (((a 1) b) 1 #f)
 > (debug:parse-level '("uh" (a 1) b) #t list)
 (((a 1) b) 1 "uh")
 > (debug:parse-level '(3 "uh" (a 1) b) #t list)
 (((a 1) b) 3 "uh")
 > (debug:parse-level '(3 (a 1) b) #t list)
 (((a 1) b) 3 #f))


(def debug:default-level 1)
;; XX why is the default level of 1 in `DEBUG` not used there?

;; DEBUG supports an optional level as the first arg (default:
;; 1). Side-effect only, returns (void) in all cases!
(defmacro (DEBUG . args)
  (if *debug*
      (debug:parse-level
       args #f
       (lambda (args level maybe-marker)
	 (assert (not maybe-marker))
	 `(when (and *debug* (<= *debug* ,level))
            (warn ,@args))))
      `(##void)))

;; Variant of DEBUG that returns the values args evaluate to after
;; dropping a static string at the start of args (regardless of
;; whether debugging is active or not).

(def (debug:perhaps-warn/cont level maybe-cont maybe-msg vals)
     (when (and *debug* (<= *debug* level))
           (let ((msg (if maybe-cont
                          (let ((cstr (object->string maybe-cont)))
                            (if maybe-msg
                                (string-append cstr " " maybe-msg)
                                cstr))
                          (or maybe-msg ""))))
             (apply warn msg vals))))


(def (debug:continuation-capture recv)
     (if *debug/continuations*
	 (continuation-capture recv)
	 (recv #f)))

(defmacro (DEBUG* . args)
  (debug:parse-level
   args #t
   (lambda (args level maybe-marker)
     (let ((vars (map (lambda_ (gensym))
		      (iota (length args)))))
       (let ((exprs (if maybe-marker (cons maybe-marker args) args))
	     (cont

	      (lambda (maybe-msg real-exprs)
		(let ((C (gensym)))
		  `(debug:continuation-capture
		    (lambda (,C) (let ,(map list vars args)
			      (debug:perhaps-warn/cont
			       ,level ,C ,maybe-msg (##list ,@vars))
			      ,(if (one-item? vars) (car vars)
				   `(values ,@vars)))))))))

	 (if (and (pair? exprs)
		  (string? (source-code (car exprs))))
	     (cont (car exprs)
		   (cdr exprs))
	     (cont #f
		   exprs)))))))

(defmacro (DBG . args)
  "If given two arguments, the first is the debug level and the second
the expression. If given one argument, the default debug level is
assumed."
  (mcase args
         (`(`level `expr)
          `(DEBUG* ,level
                   ,(object->string (cj-desourcify expr))
                   ,expr))
         (`(`expr)
          `(DEBUG* ,debug:default-level
                   ,(object->string (cj-desourcify expr))
                   ,expr))
         (else
          (raise-source-error
           stx "need 1 (expr) or 2 (level and expr) arguments"))))

(TEST
 > (def (x-msg msg)
     (let (l (string-split msg ">"))
       (cons (first (string-split (first l) char-digit?))
             (rest l))))
 > (defmacro (debug:TST expr)
     `(letv ((msg res) (%with-error-to-string ,expr))
            (list (x-msg msg)
                  res)))

 > (def *debug* 2)
 > (def x 123) 
 > (debug:TST (DEBUG* x))
 (("") 123)
 > (debug:TST (DEBUG* 1 x))
 (("") 123)
 > (debug:TST (DEBUG* 0 x))
 (("") 123)
 > (debug:TST (DEBUG* 2 x))
 ;; prints: #<continuation #4> 123
 (("#<continuation #" " 123\n") 123)
 > (debug:TST (DEBUG* "x" x))
 (("") 123)
 > (debug:TST (DEBUG* 2 "x" x))
 ;; prints: #<continuation #6> x 123
 (("#<continuation #" " x 123\n") 123)
 > (debug:TST (DBG (+ 10 20)))
 (("") 30)
 > (debug:TST (DBG 2 (+ 10 20)))
 (("#<continuation #" " (+ 10 20) 30\n")
  30)
 > (def *debug* 1)
 > (debug:TST (DBG (+ 10 20)))
 (("#<continuation #" " (+ 10 20) 30\n")
  30))


;; The syntax supports an optional level then an optional marker
;; string as the first arg(s)

(def (debug:T-expand form tailcall?)
     (def Tstr (if tailcall? "TT" "T"))
     (def Tunstr (if tailcall? "  " " "))
     (if *debug*
         (debug:parse-level
          form #t
          (lambda (form level maybe-marker)
            (with-gensym
             res
             (let ((vs (map (comp gensym .string)
                            (cdr (iota (length form))))))
               `(let ,(map (lambda (v arg)
                             `(,v ,arg))
                           vs
                           (cdr form))
                  (when (and *debug* (<= *debug* ,level))
                        (warn ,(if maybe-marker
                                   (string-append Tstr
                                                  " "
                                                  (source-code maybe-marker)
                                                  ":")
                                   (string-append Tstr
                                                  ":"))
                              (list
                               ',(car form)
                               ,@(map (lambda (v)
                                        `(show ,v))
                                      vs))
                              ,@(if tailcall? '() '('...))))
                  ,(let ((call-code `(,(car form) ,@vs)))
                     (if tailcall?
                         call-code
                         `(let ((,res ,call-code))
                            (when (and *debug* (<= *debug* ,level))
                                  (warn ,(if maybe-marker
                                             (string-append
                                              "  "
                                              (source-code maybe-marker)
                                              ":")
                                             " :")
                                        (list ',(car form)
                                              ,@(map (lambda (v)
                                                       `(show ,v))
                                                     vs))
                                        '->
                                        (show ,res)))
                            ,res))))))))
         (debug:parse-level
          form #t
          (lambda (form level maybe-marker)
            form))))

(defmacro (T . form)
  (debug:T-expand form #f))

(defmacro (TT . form)
  (debug:T-expand form #t))

(TEST
 > (defmacro (TE . body)
     `(values->vector
       (with-error-to-string
        (lambda ()
          ,@body))))
 > (both-times (def *debug* 2))
 > (TE (T + 1 2))
 ["" 3]
 ;; these should make it to stderr:
 > (TE (T 3 + 2 3))
 ["T: (+ 2 3) ...\n : (+ 2 3) -> 5\n" 5]
 > (TE (T 3 "ey" + 3 3))
 ["T ey: (+ 3 3) ...\n  ey: (+ 3 3) -> 6\n" 6]
 > (TE (TT 3 + 2 3))
 ["TT: (+ 2 3)\n" 5]
 > (TE (TT 3 "ey" + 3 3))
 ["TT ey: (+ 3 3)\n" 6]
 ;; /stderr
 > (TE (T "ey" + 3 4))
 ["" 7])


(def debug:default-port (current-error-port))

(def (2> #!optional #((maybe path-string?) path))
     (force-output (current-error-port))
     (if path
	 ;; XX O_APPEND ?
	 (current-error-port (open-output-file path))
	 (current-error-port debug:default-port)))

;; 'just a utility', to flush the port of an ongoing thing that used |2>|
(def (2force)
     (force-output (current-error-port)))


;; inline just to avoid ending up here? wait, happens anyway right?
(def-inline (debug:stop-at-line n)
  (let ((m (output-port-line (current-error-port))))
    (when (>= m n)
      (error "reached error-port line " m n))))
;; UNTESTED
(defmacro (STOP-at-line n)
  (assert* natural? n
	   (lambda (n)
	     (if *debug*
		 `(when *debug*
                    (debug:stop-at-line ,n))
		 `(##void)))))

;; better:


(def *single-step?* #t)

(def (warn-stop-on-line! n)
     (port-add-hook!
      (current-error-port)
      (named self
             (lambda (port)
               (let ((m (output-port-line port)))
                 ;; (= m n) is no good as can have multi-line warn statements
                 (when (>= m n)
                       (force-output port)
                       (port-remove-hook! port self)
                       (if *single-step?*
                           (begin
                             (displayln (STRING "reached error-port line " n)
                                        (console-port))
                             (step))
                           (error "reached error-port line" n))))))))
