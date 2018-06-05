

(define-type http-status ;; reply-status ? automtic
  id: bd6c7604-f299-489e-bed7-223be2b1ab42

  code
  maybe-name
  )

(define (http-status-name s) ;; NOTE that this can still return #f if the code does not exist! And it can return a string even if code is not in the table, if the object already carries the name.
  (or (http-status-maybe-name s)
      (http-status-code->name (http-status-code s))))


(insert-result-of
 (let ((alist
	;; from Perl's HTTP::Status module
	'((100 . "Continue")
	  (101 . "Switching Protocols")
	  (102 . "Processing")
	  (200 . "OK")
	  (201 . "Created")
	  (202 . "Accepted")
	  (203 . "Non-Authoritative Information")
	  (204 . "No Content")
	  (205 . "Reset Content")
	  (206 . "Partial Content")
	  (207 . "Multi-Status")
	  (300 . "Multiple Choices")
	  (301 . "Moved Permanently")
	  (302 . "Found")
	  (303 . "See Other")
	  (304 . "Not Modified")
	  (305 . "Use Proxy")
	  (307 . "Temporary Redirect")
	  (400 . "Bad Request")
	  (401 . "Unauthorized")
	  (402 . "Payment Required")
	  (403 . "Forbidden")
	  (404 . "Not Found")
	  (405 . "Method Not Allowed")
	  (406 . "Not Acceptable")
	  (407 . "Proxy Authentication Required")
	  (408 . "Request Timeout")
	  (409 . "Conflict")
	  (410 . "Gone")
	  (411 . "Length Required")
	  (412 . "Precondition Failed")
	  (413 . "Request Entity Too Large")
	  (414 . "Request-URI Too Large")
	  (415 . "Unsupported Media Type")
	  (416 . "Request Range Not Satisfiable")
	  (417 . "Expectation Failed")
	  (422 . "Unprocessable Entity")
	  (423 . "Locked")
	  (424 . "Failed Dependency")
	  (500 . "Internal Server Error")
	  (501 . "Not Implemented")
	  (502 . "Bad Gateway")
	  (503 . "Service Unavailable")
	  (504 . "Gateway Timeout")
	  (505 . "HTTP Version Not Supported")
	  (507 . "Insufficient Storage"))))

   ;;(newline) ;; GTRRRR immer dies
   ;;(pp-through
   `(begin
					;       ,@(map (lambda (p)
					; 	       `(define ,(string->symbol
					; 			  (string-append
					; 			   "http-status-"
					; 			   (string-to-identifyer
					; 			    (cdr p))))
					; 		  (make-http-status ,(car p)
					; 				    ,(cdr p))))
					; 	     alist)
					;^- currently unsolved problem: cannot auto-generate export list
					; so I write this instead:
      (define http-status:%symbol->obj%
	(list->table
	 ,(list 'quasiquote
		(map
		 (lambda (p)
		   (cons (string->symbol
			  (string-to-identifyer
			   (cdr p)))
			 (list 'unquote
			       `(make-http-status ,(car p)
						  ,(cdr p)))))
		 alist))
	 test: eq?))
      (define http-status:%code->name%
	(list->table
	 ',alist
	 test: eq?)))))


(define (http-status symbol)
  (table-ref http-status:%symbol->obj% symbol))

(define (http-status-code->name code)
  (table-ref http-status:%code->name% code #f))

(define (maybe-http-status:code hs-or-code)
  (cond ((http-status? hs-or-code)
	 (http-status-code hs-or-code))
	((##fixnum? hs-or-code)
	 hs-or-code)
	(else (error "not a http-status or -code:" hs-or-code))))

(define (make-check-http-status-or-code from to)
  (lambda (hs-or-code)
    (let ((code (maybe-http-status:code hs-or-code)))
      (and (>= code from)
	   (< code to)))))

(define http-status:is-info? (make-check-http-status-or-code 100 200))
(define http-status:is-success? (make-check-http-status-or-code 200 300))
(define http-status:is-redirect? (make-check-http-status-or-code 300 400))
(define http-status:is-error? (make-check-http-status-or-code 400 600))
(define http-status:is-client-error? (make-check-http-status-or-code 400 600))
(define http-status:is-server-error? (make-check-http-status-or-code 500 600))

