;;; Copyright 2013-2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (cj-source-util-2 assert)
	 (stream stream-map)
	 cut
	 (char-util char-alphanumeric?))


(export make-realrandom-string-stream
	make-realrandom-alphanumeric-string-stream)

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

(define (make-realrandom-string-stream #!optional maybe-stringlen maybe-fn)
  (if maybe-stringlen
      (assert (< maybe-stringlen 76))) ;; that's what base64 delivers. sick yeah but...
  (let ((s (port->lines-stream
	    ;; XX redirect stderr just to silence the warnings upon
	    ;; termination; evil.
	    (open-process
	     (list path: "bash"
		   arguments: (list "-c" "exec base64 /dev/urandom 2>/dev/null")
		   stdout-redirection: #t)))))
    (if (or maybe-stringlen maybe-fn)
	(stream-map (let ((l1 (cut substring <> 0 maybe-stringlen)))
		      (cond ((and maybe-stringlen maybe-fn)
			     (lambda (v)
			       (maybe-fn (l1 v))))
			    (maybe-stringlen
			     l1)
			    (else
			     maybe-fn)))
		    s)
	s)))

;; XX careful: does not guarantee to deliver stringlen characters per string!
(define (make-realrandom-alphanumeric-string-stream #!optional maybe-stringlen)
  (make-realrandom-string-stream #f
				 (lambda (s)
				   ;; well..
				   (list->string
				    (let ((l (filter char-alphanumeric?
						     (string->list s))))
				      (if maybe-stringlen
					  (take l maybe-stringlen)
					  l))))))

