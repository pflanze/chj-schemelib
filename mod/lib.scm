
(define-macro (compile-time-define-if-not-defined name expr)
  (with-exception-catcher
   (lambda (e)
     (eval `(define ,name ,expr)))
   (lambda ()
     (eval name) ;; if it doesn't exist, will define it
     '(begin))))


(define-macro (local var+exprs . body)
  (let* ((var->kept_ (map (lambda (var+expr)
			    (cons (car var+expr)
				  (gensym)))
			  var+exprs))
	 (var->kept (lambda (var)
		      (cdr (assq var var->kept_)))))
    `(let ,(map (lambda (var+expr)
		  `(,(var->kept (car var+expr)) #f))
		var+exprs)
       (dynamic-wind (lambda ()
		       ,@(map (lambda (var+expr)
				(define var (car var+expr))
				(define expr (cadr var+expr))
				`(begin
				   (set! ,(var->kept var) ,var)
				   (set! ,var ,expr)))
			      var+exprs))
	   (lambda ()
	     ,@body)
	   (lambda ()
	     ,@(map (lambda (var+expr)
		      (define var (car var+expr))
		      (define expr (cadr var+expr))
		      `(begin
			 ;; simply drop value of var (and recalculate from expr)?
			 (set! ,var ,(var->kept var))))
		    var+exprs))))))

(define (symbol-memq sym lis)
  (if (symbol? sym)
      (let lp ((lis lis))
	(if (null? lis)
	    #f
	    (let ((a (car lis)))
	      (if (symbol? a)
		  (if (eq? a sym)
		      lis
		      (lp (cdr lis)))
		  (error "symbol-memq: lis contains non-symbol: " a)))))
      (error "symbol-memq: not a symbol:" sym)))

;; > (symbol-memq 'foo '())
;; #f
;; > (symbol-memq 'foo '(foo))
;; (foo)
;; > (symbol-memq 'bar '(foo 'bar))
;; *** ERROR IN lp, "../lib/mod/mod.scm"@209.5 -- symbol-memq: lis contains non-symbol:  'bar
;; > (symbol-memq ''bar '(foo 'bar))
;; *** ERROR IN symbol-memq, "../lib/mod/mod.scm"@210.7 -- symbol-memq: not a symbol: 'bar
;; > (symbol-memq 'barr '(foo bar))
;; #f

(define-macro (future expr)
  `(thread-start!
    (make-thread
     (lambda ()
       ,expr))))

(define-macro (thunk . body)
  `(lambda ()
     ,@body))


;; Simple thread messaging framework, in-process only.

;; Strictly client-server: clients can only receive responses from
;; servers.

;; make a server:

;; dispatch receives cmd (e.g. a symbol), and should return a
;; procedure that will receive a return procedure and the arguments.

(define (fourmi-server dispatch)
  (thread-start!
   (make-thread
    (lambda ()
      (call/cc
       (lambda (quit)
	 (let loop ()
	   (let* ((msg (thread-receive))
		  (id (vector-ref msg 0))
		  (cmd (vector-ref msg 1))
		  (return-thread (vector-ref msg 2))
		  (args (vector-ref msg 3)))
	     (apply
	      (dispatch cmd)
	      ;; the return procedure:
	      (lambda (v)
		(thread-send return-thread
			     (cons id v)))
	      args))
	   (loop))))))))

;; call server from clients:

(define (fourmi-run th cmd . args)
  (let ((id (box 'id)))
    (thread-send th
		 (vector id
			 cmd
			 (current-thread)
			 args))
    (let ((res (thread-receive)))
      (if (eq? (car res) id)
	  (cdr res)
	  (error "got something else than expected response:" res)))))

