
;; data types:
;;  commport  just the open-process port
;;  remcomm   bundled with local vector port for receiving (commport only send)

(define-type remcomm
  id: a270a48a-a0e8-4adb-9aee-bed0981be2a2
  remote-port
  vector-port)

;; === Interface to user ===

(define (start-compiler)
  (let ((p (open-process
	    (list path: "gsc"
		  arguments: (list
			      "-:tE,d-,t8,f8,tu"
			      ;; "-f" ;; no .gambcini loading
			      ;; XX ^ actually load code that way for now
			      )
		  stdin-redirection: #t
		  stdout-redirection: #t
		  ;;XX: bad bc of loosing ordering. but, would need sep ports
		  stderr-redirection: #f))))
    (write `(remote:start-compiler) p)
    (force-output p)

    ;; Drop Gambit welcome message
    (let lp ()
      (let ((line (read-line p #\newline #f)))
	(if (string=? line "compiler-running")
	    (void)
	    (lp))))

    (let ((remcomm (make-remcomm p (open-vector (list buffering: #f)))))
      ;; start proxy thread
      (thread-start!
       (make-thread
	(commport-dispatcher remcomm)))

      (dorem:check-ok remcomm)

      remcomm)))

(define (stop-compiler remcomm)
  (let ((p (remcomm-remote-port remcomm)))
    (close-port p)
    (process-status p)))


;; === Internal, on client side ===

;; Using a proxy thread to transparently channel output- and error
;; character ports to the terminal.

;; This is run aynchronically (but sends normal messages back to
;; 'synchronous' thread)
(define (commport-dispatcher remcomm)
  (let ((rp (remcomm-remote-port remcomm))
	(vp (remcomm-vector-port remcomm)))
    (lambda ()
      (call/cc
       (lambda (exit-lp)
	 (let lp ()
	   (let ((msg (remcomm:recv rp
				    ;; on eof:
				    (lambda ()
				      (println "process exited with status: "
					       (process-status rp))
				      ;;^ unnecessary?
				      (write `(exception
					       process-exited)
					     vp)
				      (exit-lp)))))
	     (case (and (pair? msg)
			(car msg))
	       ((port)
		;; heh and here it's *not* a separate thread
		(println (cadr msg) ": " (caddr msg)))
	       (else
		;; send  ?  why write?
		(write msg vp))))
	   (lp)))))))

(define (make-dorem-command format cont)
  (lambda (p . args)
    (let ((rp (remcomm-remote-port p))
	  (vp (remcomm-vector-port p)))
      ;; v--XX ach wll wrong name for remcomm:send then. sgh
      (remcomm:send rp (apply format args))
      (let redo ()
	;; (why read and not a recv)
	(let ((msg (read vp)))
	  (case (and (pair? msg)
		     (car msg))
	    ((value)
	     (cont (cadr msg)))
	    ((exception)
	     (raise (cadr msg)))
	    (else
	     (error "invalid reply:" msg))))))))


;; === Interface to user, cont. ===

(define dorem:check-ok
  (make-dorem-command
   (lambda ()
     `(ok?))
   (lambda (reply)
     (or (eq? reply 'ok)
	 (error "unexpected reply:" reply)))))

(define dorem:check-ok-error
  (make-dorem-command
   (lambda ()
     `(ok-error))
   values))

(define dorem:compile-expr
  (make-dorem-command
   (lambda (path expr)
     `(compile-expr ,path ,expr))
   values))


;; === Internal ===

(define (natural0->u8vector n len)
  (let ((v (make-u8vector len)))
    (let lp ((i 0)
	     (r n))
      (if (zero? r)
	  v
	  (if (< i len)
	      (let* ((r* (arithmetic-shift r -8))
		     (b (bitwise-and r 255)))
		(u8vector-set! v i b)
		(lp (inc i)
		    r*))
	      (error "number does not fit in len:" n len))))))

;; > (natural0->u8vector 255 4)
;; #u8(255 0 0 0)
;; > (natural0->u8vector 256 4)
;; #u8(0 1 0 0)
;; > (natural0->u8vector 258 4)
;; #u8(2 1 0 0)
;; > (natural0->u8vector 123210391823 5)
;; #u8(15 97 233 175 28)
;; > (+ 15 (* 256 (+ 97 (* 256 (+ 233 (* 256 (+ 175 (* 256 28)))) ))))
;; 123210391823

(define (u8vector->natural0 v len)
  (let lp ((n 0)
	(i (dec len)))
    (if (negative? i)
	n
	(lp (+ (u8vector-ref v i)
	       (arithmetic-shift n 8))
	    (dec i)))))

;; > (u8vector->natural0  '#u8(0 1 0 0) 4)
;; 256
;; > (u8vector->natural0  '#u8(1 0 0 0) 4)
;; 1
;; > (u8vector->natural0  '#u8(15 97 233 175 28) 5)
;; 123210391823

(define remcomm:len-len 8) ;; bytes

;; cmd is a sexpr, or actually anything
(define (remcomm:send p cmd)
  (let* ((v (object->u8vector cmd))
	 (len (u8vector-length v)))
    (write-subu8vector (natural0->u8vector len remcomm:len-len)
		       0 remcomm:len-len p)
    (write-subu8vector v 0 len p)
    (force-output p)))

;; resized as needed. XXX: ever downsize?
(define remcomm:buf-len 8) ;; follows shrinking tho
(define remcomm:buf (make-u8vector remcomm:buf-len))

;; always reuse
(define remcomm:len-len-buf (make-u8vector remcomm:len-len))

(define (read-u8vector-tmp! p len on-eof)
  ;; returned buf is only valid up to next call
  (let ((buf
	 (if (= len remcomm:len-len)
	     remcomm:len-len-buf
	     (if (< remcomm:buf-len len)
		 (let ((newbuf (make-u8vector len)))
		   (set! remcomm:buf newbuf)
		   (set! remcomm:buf-len len)
		   newbuf)
		 (begin
		   ;; u8vector->object requires the len set correctly
		   (##u8vector-shrink! remcomm:buf len)
		   ;; also have to adapt acceptable len, because the
		   ;; buf will be actually reduced in size once gc
		   ;; runs, right?
		   (set! remcomm:buf-len len)
		   remcomm:buf)))))
    (let ((rdlen (read-subu8vector buf 0 len p len)))
      (cond ((= rdlen len)
	     buf)
	    ((zero? rdlen)
	     (on-eof))
	    (else
	     (error "only read:" rdlen))))))

(define (remcomm:recv p on-eof)
  (let* ((lenv (read-u8vector-tmp! p remcomm:len-len on-eof))
	 (len (u8vector->natural0 lenv remcomm:len-len))
	 (_
	  ;; HACK
	  (if (>= len 4428738507345651052)
		(let lp ()
		  (println (read-line p))
		  (lp))))
	 (v (read-u8vector-tmp! p len on-eof))
	 (res (u8vector->object v)))
    res))

(define (remcomm:virtual-port kind commport)
  (let* ((p (open-string (list buffering: 'line)))
	 ;; well does the buffering setting have an effect?
	 (th (make-thread
	      (lambda ()
		(let lp ()
		  ;; never eof? in-process.
		  (let ((line (read-line p)))
		    ;; or read-line ? ?
		    ;;v- XX does this need a mutex (internally)?
		    (remcomm:send commport `(port ,kind ,line)))
		  (lp))))))
    (thread-start! th)
    p))

(define (remote:start-compiler)
  (display "\ncompiler-running\n")
  (force-output)
  (let ((in (current-input-port))
	(out (current-output-port)))
    (parameterize
     ((current-output-port (remcomm:virtual-port 'output-port out))
      (current-error-port (remcomm:virtual-port 'error-port out)))
     (let lp ()
       (let ((msg (remcomm:recv in
				;; on eof:
				(lambda ()
				  (exit 0))))) ;; catch exceptions?
	 (remcomm:send out
		       (remote:dispatch msg)))
       (lp)))))

(define (remote:dispatch msg)
  (with-exception-catcher
   (lambda (e)
     `(exception ,e))
   (lambda ()
     `(value ,(case (car msg)
		((ok?) 'ok)
		((ok-error)
		 (error 'ok))
		((load)
		 (let ((path (cadr msg)))
		   ;;XX or a dependency or what loading?
		   (load path)))
		((compile-expr)
		 (let ((path (cadr msg))
		       (expr (caddr msg)))
		   (compile-expr path expr)))
		(else
		 (raise `(unknown-message ,(car msg)))))))))

