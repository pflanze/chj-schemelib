(include "monad.scm")


(define (loaded+loading loaded loading)
  (vals loaded loading))
(define loadstate-loaded val0)
(define loadstate-loading val1)

(define (m:load-tree mod loadstate)
  (delay
    (let ((sym (val0 mod)))
      (cond ((memq sym (loadstate-loading loadstate))
	     (source-error (val1 mod) "circular dependency"
			   (loadstate-loading loadstate)))
	    ((memq sym (loadstate-loaded loadstate))
	     ;; ref, how.
	     (v+m `(loaded ,sym) loadstate))
	    (else
	     (let* ((depends+code
		     (begin
		       (println "calculating depends+code for:" sym)
		       (mod:file->depends+code (mod->mod-path sym))))
		    (v+loadstate*
		     (force
		      (m:map m:load-tree
			     ;; perhaps keep code?XX
			     (val0 depends+code)
			     ;; loading ourselves:
			     (loaded+loading
			      (loadstate-loaded loadstate)
			      (cons sym (loadstate-loading loadstate))))))
		    (v (val-v v+loadstate*))
		    (loadstate* (val-m v+loadstate*)))
	       (or (eq? (car (loadstate-loading loadstate*)) sym)
		   (error "bug"))
	       (v+m (cons sym v)
		    ;; loaded ourselves:
		    (loaded+loading
		     (cons sym (loadstate-loaded loadstate*))
		     (cdr (loadstate-loading loadstate*))))))))))

