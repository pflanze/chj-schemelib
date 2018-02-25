(require)

(export random-hexstring
	random-filename)

;; XX not fork safe

(define (random-hexstring len)
  (let* ((s (number->string (random-integer (expt 16 len)) 16))
	 (l (string-length s)))
    (if (= l len)
	s
	(string-append (make-string (- len l) #\0) s))))

(define (random-filename)
  ;; 128 bits
  (random-hexstring 32))

