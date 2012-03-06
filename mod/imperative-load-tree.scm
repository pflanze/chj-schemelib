;; XX without locking or processes yet:
(define xbox-get! unbox)
;;(define xbox-set! set-box!)
(define (xbox-update! b fn)
  (set-box! b (fn (unbox b))))
(define make-xbox box)

;; (is |loading| necessary? vs just thread deadlock?)

(define load-tree/loading/save-code!
  (lambda (save-code! reference)
    (let ((refs (make-xbox '())))
      (letrec
	  ((load-tree/loading
	    (lambda (loading)
	      (lambda (mod)
		(delay
		  (let ((sym (val0 mod)))
		    (cond ((memq sym loading)
			   (source-error (val1 mod) "circular dependency"
					 loading))
			  ((assq sym (xbox-get! refs))
			   => reference)
			  (else
			   (let* ((depends+code
				   (begin
				     (println "calculating depends+code for:" sym)
				     (mod:file->depends+code (mod->mod-path sym))))
				  (res
				   (cons sym
					 (map (load-tree/loading
					       (cons sym loading))
					      ;; perhaps keep code?XX
					      (val0 depends+code)))))
			     (xbox-update! refs (lambda (l)
						  (cons (cons sym res) l)))
			     (save-code! sym (val1 depends+code))
			     res)))))))))
	load-tree/loading))))

(define (load-tree mod)
  (((load-tree/loading/save-code!
     (lambda (sym code)
       (void))
     (lambda (k+v)
       `(REF ,(cdr k+v))))
    '())
   mod))
