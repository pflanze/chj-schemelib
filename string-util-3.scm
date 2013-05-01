;; also requires |define.|
;; Hm actually perhaps it doesn't.
;; Well or maybe it does. |for| as macro, then .for for the underlying?

;; (define-macro* (for var seq . body)
;;   `(.for seq
;; 	 (lambda (,var return)
;; 	   ,@body)))


(define (string-contains-char? str pred)
  (let ((len (string-length str)))
    (let lp ((i 0))
      (and (< i len)
	   (or (pred (string-ref str i))
	       (lp (inc i)))))))

(TEST
 > (string-contains-char? "Hello" char-newline?)
 #f
 > (string-contains-char? "Hello\n" char-newline?)
 #t
 )
