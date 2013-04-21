(define (writeln obj)
  (write obj)
  (newline))

(define (hostname)
  (let* ((p (open-process (list path: "hostname"
			       stdout-redirection: #t)))
	 (output (read-line p)))
    (close-port p)
    (assert (zero? (process-status p)))
    output))

;; where should that be moved to?
(define file-info->mtime
  (compose time->seconds
	   file-info-last-modification-time))

