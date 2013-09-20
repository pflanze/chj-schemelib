
;; (define (get-xml-file path)
;;   )
;; actually can't use current xml-to-sexpr (directly). Can't do path to stdout.
;; anyway.

(define (get-sxml-file path)
  ;; xone? or
  (let ((res (call-with-input-file path read-all)))
    (if (and (pair? res)
	     (null? (cdr res)))
	(car res)
	res)))


;; (define (put-xml-file path sxml)
;;   ;; hmmm. have (sxml>>xml-file item path) already. Which doesn't
;;   ;; specify utf-8 huh?
;;   (call-with-output-file
;;       (list path: path
;; 	    char-encoding: 'UTF-8)
;;     (cut sxml>>xml <>)))

;; well not used anyway right now hu.

