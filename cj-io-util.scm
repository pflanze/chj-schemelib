(define (writeln obj)
  (write obj)
  (newline))


(define (port.content p)
  (read-line p #f))

(define (pathspec.xcontent pathspec)
  (let* ((p (open-input-file pathspec))
	 (output (port.content p)))
    (close-port p)
    output))


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

