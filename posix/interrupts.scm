;;; Copyright 2013-2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require ;;cj-queue
	 cj-alist
	 cj-env
	 ;;(list-util map-iota)
	 )

;; (compile #t)

;; (exports

;;  ;; many SIGxxx names. how to export those automatically?
;;  SIGCHLD
;;  SIGPOLL
;;  ;;...
;;  SIGIO ;; = SIGPOLL

;;  signal-number->name ;; takes int. dies on non-existing signals.[could this be made part of type sys?]
 
;;  interrupt-install-handler! ;; (interrupt-install-handler! signal-number handler)
;;  interrupt-remove-handler! ;; (interrupt-remove-handler! signal-number)
;;  )

;; (exports-on-request

;;  make-sigqueue
;;  sigqueue-empty?
;;  sigqueue-endpos
;;  sigqueue-full?
;;  sigqueue-overflow
;;  sigqueue-overflow-reset!
;;  sigqueue-positions
;;  sigqueue-startpos
;;  sigqueue-take!
;;  sigqueue-usage
;;  sigqueue-add!
;;  sigqueue-remove!
;;  sigqueue-ref-signo
;;  ;; constant-from-c: 
;;  SIGQUEUE_ERROR
;;  SIGQUEUE_ERROR2
;;  SIGQUEUE_SUCCESS
;;  ;; ?:
;;  sig-errstr

;;  ;; ~lowlevel handling:
;;  init
;;  call-with-locks
;;  global_queue
;;  signal-take!
;;  interrupt-dispatch
;;  sig-lock!
;;  sig-unlock!
;;  sig-locked?

;;  handlers
;;  handlers-set! ;;(this is for alist-set!)

;;  )

 ;; compiletime: 
 ;; make-gen-code



(define-constant-from-C SIGCHLD)
(define-constant-from-C SIGINT)
(define-constant-from-C SIGHUP)
(define-constant-from-C SIGPOLL)
;;XX and more...


;; cj Fri, 27 Jan 2006 12:25:15 +0100
;; based on code sent by Marc Feeley on 4 Jan 2006

;; this is unix centric, and expects BSD like signal behaviour.
;; I develop on Debian.
;; (Maybe some time, this could go into a |posix| module?)

;; todo: this assumes that the order in which raised interrupts are
;; delivered to scheme code is the same as the one in which the C
;; signal handler is called. Is this a safe assumption?

;; todo: make this thread safe

;; create a retrieve-current setting infrastructure, which then can be
;; given instead of procedure?
;; or, do it with parameters instead ?  how?

;; cj Thu, 28 Dec 2006 01:35:31 +0100
;; NOTE: this is using 'signal', not 'sigaction'. But as long as
;; nobody's interested in realtime signals (and/or special signals
;; like async etc.), that probably doesn't make a difference (since
;; non-realtime signals can be lost).

;; -- compilation infrastructure

;;simplified COPY from gperl-lowlevel.scm
(##define-macro (define-constant-from-C name)
  `(define ,name
     ((c-lambda ()
		int
		,(string-append "___result="
				(symbol->string name)
				";")))))
;;/COPY

(compile-time
 (define (make-gen-code check-code return-code)
   (lambda (name/args c-name argtypes returntype error-message #!optional error-message2)
     (let ((name (car name/args))
	   (args (cdr name/args)))
       `(define ,name/args
	  (let ((res ((c-lambda ,argtypes ,returntype ,c-name) ,@args)))
	    (if ,check-code
		(error (string-append
			,(string-append (symbol->string name) ": ")
			,(if error-message2
			     `(if (= res SIGQUEUE_ERROR2)
				  ,error-message2
				  ,error-message))))
		,return-code)))))))

(##define-macro (define-c/int_or_error . args)
  (apply (make-gen-code '(= res SIGQUEUE_ERROR) 'res) args))

(##define-macro (define-c/status . args)
  (apply (make-gen-code '(not (= res 0)) #!void) args))


;; ----- sigqueue interface ---------------

(c-declare "#include \"interrupts_Cpart.c\"")

;(c-define-type sigqueue (pointer (struct "sigqueue") sigqueue* "sigqueue_release"))
(c-define-type sigqueue (pointer (struct "sigqueue") sigqueue "sigqueue_release"))
; aber nein, default handling of gambit is with the stars.
;(c-define-type sigqueue (pointer (struct "sigqueue") sigqueue* "sigqueue_release"))
; but why. how would I deal with a structure without pointer? todo ask.
;; hey and even stuff like latin1-string do not have stars.


(define-constant-from-C SIGQUEUE_SUCCESS)
(define-constant-from-C SIGQUEUE_ERROR)
(define-constant-from-C SIGQUEUE_ERROR2)

(define make-sigqueue (c-lambda () sigqueue "make_sigqueue"))

(define sigqueue-usage (c-lambda (sigqueue) int "sigqueue_usage"))

(define (sigqueue-full? q)
  (= ((c-lambda (sigqueue) int "sigqueue_isfull") q)
     1))
(define (sigqueue-empty? q)
  (= ((c-lambda (sigqueue) int "sigqueue_isempty") q)
     1))

(define-c/status (sigqueue-add! q signo) "sigqueue_add" (sigqueue int) int "queue is full")

(define-c/int_or_error (sigqueue-ref-signo q) "sigqueue_ref_signo" (sigqueue) int "queue is empty")

(define-c/status (sigqueue-remove! q) "sigqueue_remove" (sigqueue) int "queue is empty")

(define-c/status (sig-lock!) "sig_lock" () int "already locked" "sigprocmask gave error");;todo show OS error
(define-c/status (sig-unlock!) "sig_unlock" () int "not locked" "sigprocmask gave error");;todo show OS error

(define (sig-locked?)
  ((c-lambda () bool "___result= is_locked;")))

;; scheme addons:

(define (sigqueue-take! q)
  (let ((res (sigqueue-ref-signo q)))
    (sigqueue-remove! q)
    res))

(define (sigqueue-startpos q)
  ((c-lambda (sigqueue) int "___result= ___arg1->startpos;") q))
(define (sigqueue-endpos q)
  ((c-lambda (sigqueue) int "___result= ___arg1->endpos;") q))
(define (sigqueue-positions q)
  (values (sigqueue-startpos q)
	  (sigqueue-endpos q)))

(define (sigqueue-overflow q)
  ((c-lambda (sigqueue) bool "___result= ___arg1->overflow;") q))
(define (sigqueue-overflow-reset! q)
  ((c-lambda (sigqueue) void "___arg1->overflow=false;") q))


;; ---- the list of possible signals and their names:  -------------

(insert-result-of
 (begin
   ;; Get all signal names. This assumes that the shell command "kill -l
   ;; <number>" will return the signal name without the SIG prefix.
   (define (signal-number->name num)
     (let* ((port (open-process (list path: "kill"
				      arguments: (list "-l" (number->string num)))))
	    (line (read-line port))
	    (res (close-port port)))
       ;;(warn "got kill res code-or-so =" res) is always #!void.  still ask how todo this.
       (string-append "SIG" line)))
   (let ((alis (map (lambda (num)
			   (cons num (string->symbol (signal-number->name num))))
		    (iota
		     64   ;; the number of valid signal numbers
		     1)	  ;; the lowest signal number
		    )))
     `(begin
	(define %signal-number->name% ',alis)
	,@ (map (lambda (p)
		  `(define ,(cdr p) ,(car p)))
		alis)))))

(define-if-not-defined SIGIO SIGPOLL)

(define (signal-number->name num) ; returns symbol. [should we take err+success continuations?...]
  (number-alist-ref %signal-number->name% num
		    (lambda ()
		      (error "signal-number->name: unknown signal number:" num))))


;; ------- interfacing scheme with handler setup functions: ------

(define sig-errstr (c-lambda ()
			     ;; latin1-string  hm not available anymore? wl fair
			     char-string
			     "sig_errstr"))

(insert-result-of
 (cons 'begin
       (map (lambda (CNAME)
	      `(define (,(string->symbol CNAME)
			  num)
		 (or (= ((c-lambda (int) int ,CNAME) num) 0)
		     (error (string-append ,(string-append CNAME ": system error: ")
					   (sig-errstr))))))
	    '("install_signal" "uninstall_signal"))))


(define-if-not-defined interrupts:handlers
  ;; signal to handler
  (make-table))

(define (interrupt-install-handler! signal-number handler) ;; signal-number may also be a list of numbers
  (if (and (##fixnum? signal-number)
	   (> signal-number 0))
      ;; check for SIGINT and then set current-user-interrupt-handler instead?
      (if (= signal-number SIGINT)
	  (current-user-interrupt-handler handler)
	  (begin
	    (table-set! interrupts:handlers signal-number handler)
	    (install_signal signal-number)))
      (if (list? signal-number)
	  (for-each interrupt-install-handler! signal-number)
	  (error "interrupt-install-handler!: signal-number argument is not an ordinal number:" signal-number))))

(define (interrupt-remove-handler! signal-number) ;; signal-number may also be a list of numbers
  (if (and (##fixnum? signal-number)
	   (> signal-number 0))
      (if (= signal-number SIGINT)
	  (begin
	    (current-user-interrupt-handler ##default-user-interrupt-handler ;; ok?
					    ;; and: do we want it to reinstall the default gambit one? or rather remove it really..?
					    ;; (todo: should we route signal 2 through this interrupts module as well?)
					    ))
	  (begin
	    (uninstall_signal signal-number)
	    (or (table-ref interrupts:handlers signal-number #f)
		(error "interrupt-remove-handler!: no handler installed for signal:"
		       signal-number))
	    (table-set! interrupts:handlers signal-number)))
      (if (list? signal-number)
	  (for-each interrupt-remove-handler! signal-number)
	  (error "interrupt-remove-handler!: signal-number argument is not an ordinal number:" signal-number))))


(define-if-not-defined global_queue #f)

;;çç neue funktionen. noch mehr?

(define (call-with-locks thunk)
  (dynamic-wind
      sig-lock!
      thunk
      sig-unlock!))

(define (signal-take!)
  (call-with-locks (lambda ()
		     (sigqueue-take! global_queue))))


(define (interrupt-dispatch)
  ;; called by scheme runtime as result of the C
  ;; ___EXT(___raise_interrupt) (___INTR_7); call and the
  ;; (##interrupt-vector-set! 7 interrupt-dispatch) mapping.
  (let ((signum (call-with-locks
		 (lambda ()
		   (if (sigqueue-overflow global_queue)
		       (begin
			 (warn "Warning: signal queue has been overflown!")
			 (sigqueue-overflow-reset! global_queue)))
		   (sigqueue-take! global_queue)))))
    (or (table-ref interrupts:handlers signum #f)
	(begin
	  (warn "no scheme signal handler installed anymore for signal:" signum)
	  (exit 123) ;;;?todo
	  ))))


(define (init)
  (let ((res ((c-lambda () int "init"))))
    (cond ((= res SIGQUEUE_SUCCESS)
	   ;; 'extract' the queue:
	   ;;(if (not global_queue) ehr no, forget it. newly initialized in C, we need it here to. The only alternative would be the other direction, give from scheme to C. But we only loose not-yet-delivered signals, so no really big deal anyway.
	   (set! global_queue ((c-lambda () sigqueue "___result_voidstar= global_queue;")))
	   (##interrupt-vector-set! 7 interrupt-dispatch))
	  ((= res SIGQUEUE_ERROR)
	   (error "interrupts init: could not allocate memory for sigqueue"))
	  (else
	   (error "interrupts init: ?? unknown error")))))

(init);yes on each load init it. since C part needs it.right?.

; --------

; test:
(insert-result-of
 (cond (#f
	'(begin
	   (interrupt-install-handler! SIGINT (lambda () (warn "sigint called")))
	   (interrupt-install-handler! SIGUSR1 (lambda () (warn "sigusr1 called")))
	   (interrupt-install-handler! SIGUSR2 (lambda () (warn "sigusr2 called")))


	   (define my-pid (##os-getpid))

	   (display "executing kill -USR1\n")

	   (shell-command (string-append "kill -USR1 " (number->string my-pid)))

	   (display "executing kill -USR2\n")

	   (shell-command (string-append "kill -USR2 " (number->string my-pid)))

	   (display "done\n")))
       (#f
	'(begin
	   (define (test port)
	     (interrupt-install-handler! SIGINT (lambda () (display "i" port)))
	     (interrupt-install-handler! SIGUSR1 (lambda () (display "1" port)))
	     (interrupt-install-handler! SIGUSR2 (lambda () (display "2" port)))
	     (interrupt-install-handler! SIGRTMIN (lambda () (error "got RT signal")))
	     )))
       (else
	'(begin))))
