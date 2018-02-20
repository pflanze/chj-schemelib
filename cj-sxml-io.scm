(require list-util
	 (cj-io-util xcall-with-input-process))

(export get-xml-file
	get-sxml-file)


(define (get-xml-file path)
  (let ((res (xcall-with-input-process
	      (list path: "xml-to-sexpr" 
		    arguments: (list "--expand-entities" "--stdout" "--" path))
	      read-all)))
    (if (one-item? res)
	(car res)
	res)))

(define (get-sxml-file path)
  ;; xone? or
  (let ((res (call-with-input-file path read-all)))
    (if (one-item? res)
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

