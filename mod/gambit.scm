
(define (compile-expr path expr)
  ;; reuses global compile-options
  (local ((c#expand-source (lambda (_expr)
			     expr)))
	 (compile-file path compile-options)))


(define (file-mtime path)
  (time->seconds (file-info-last-modification-time path)))


