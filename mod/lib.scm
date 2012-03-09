
(define-macro (compile-time-define-if-not-defined name expr)
  (with-exception-catcher
   (lambda (e)
     (eval `(define ,name ,expr)))
   (lambda ()
     (eval name) ;; if it doesn't exist, will define it
     '(begin))))


(define-macro (local var+exprs . body)
  (let* ((var->kept_ (map (lambda (var+expr)
			    (cons (car var+expr)
				  (gensym)))
			  var+exprs))
	 (var->kept (lambda (var)
		      (cdr (assq var var->kept_)))))
    `(let ,(map (lambda (var+expr)
		  `(,(var->kept (car var+expr)) #f))
		var+exprs)
       (dynamic-wind (lambda ()
		       ,@(map (lambda (var+expr)
				(define var (car var+expr))
				(define expr (cadr var+expr))
				`(begin
				   (set! ,(var->kept var) ,var)
				   (set! ,var ,expr)))
			      var+exprs))
	   (lambda ()
	     ,@body)
	   (lambda ()
	     ,@(map (lambda (var+expr)
		      (define var (car var+expr))
		      (define expr (cadr var+expr))
		      `(begin
			 ;; simply drop value of var (and recalculate from expr)?
			 (set! ,var ,(var->kept var))))
		    var+exprs))))))
