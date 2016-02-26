(require (cj-source-util-2 assert)
	 (stream stream-map)
	 cut
	 )

;; well HAD something in mod

;; also HAD something base64 alike encoding in mod

;; also HAD something io stream in  lib


;;XX move to lib
(define (port->lines-stream p)
  (let lp ()
    (let ((line (read-line p)))
      (delay
	(if (eof-object? p)
	    (begin
	      (close-port p)
	      ;; checking process-status if it's a process port?
	      ;; ...also HAD process-port? in lib, right?
	      '())
	    (cons line
		  (lp)))))))

(define (make-realrandom-string-stream maybe-stringlen)
  (if maybe-stringlen
      (assert (< maybe-stringlen 76))) ;; that's what base64 delivers. sick yeah but...
  (let ((s (port->lines-stream
	    ;; XX redirect stderr just to silence the warnings upon
	    ;; termination; evil.
	    (open-process
	     (list path: "bash"
		   arguments: (list "-c" "exec base64 /dev/urandom 2>/dev/null")
		   stdout-redirection: #t)))))
    (if maybe-stringlen
	(stream-map (cut substring <> 0 maybe-stringlen)
		    s)
	s)))

