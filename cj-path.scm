;; hm again

(define (path-append a b)
  (string-append a "/" b))


;; XX should this be in an IO library, not path.

(define (if-file-info path chase? then/1 else/0)
  (let ((prevhandler (current-exception-handler)))
    (continuation-capture
     (lambda (return)
       (then/1 (with-exception-handler
		(lambda (e)
		  (if (no-such-file-or-directory-exception? e)
		      (continuation-graft return else/0)
		      (prevhandler e)))
		(thunk
		 (file-info path chase?))))))))

(define (maybe-file-info path #!optional (chase? #t))
  (if-file-info path chase? identity false/0))

(define (file-directory? path)
  (eq? (file-info-type (file-info path)) 'directory))
;; well, better, no error when not existing:
(define (-d? path #!optional (chase? #t))
  (if-file-info path
		chase?
		(lambda (info)
		  (eq? (file-info-type info) 'directory))
		false/0))
(define (-f? path #!optional (chase? #t))
  (if-file-info path
		chase?
		(lambda (info)
		  (eq? (file-info-type info) 'regular))
		false/0))
