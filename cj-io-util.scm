(define (writeln obj)
  (write obj)
  (newline))


(define (backtick cmd . args)
  (let* ((p (open-process (list path: cmd
				arguments: args
				stdout-redirection: #t)))
	 (output (read-line p #f)))
    (close-port p)
    (assert (zero? (process-status p)))
    (chomp output)))

(define (hostname)
  (backtick "hostname"))

;; where should that be moved to?
(define file-info->mtime
  (compose time->seconds
	   file-info-last-modification-time))

