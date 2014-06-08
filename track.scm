;; debugging with conts

(def *tracks* (make-table test: eq?
			  weak-keys: #t
			  weak-values: #f ;; keep cont alive as long as obj. sgh?
			  ))

;; (def tracks-keepalive-num 100)
;; (def *tracks-keepalive* (make-vector tracks-keepalive-num))

;; (def *tracks-keepalive-i* 0)

(def (T v)
     (continuation-capture
      (lambda (c)
	(table-set! *tracks* v c)
	;;(vector-set! *tracks-keepalive* *tracks-keepalive-i*  ) not so simple. need key, too, remove explicitely from table. ah and vector shouldn't keep alive. rather wills
	v)))

(def (visit v)
     (cond ((table-ref *tracks* v #f)
	    => (lambda (c)
		 ;; XX my wrappers where?
		 (##repl-within c "" "")))
	   (else
	    (error "no continuation stored for object:" v))))

(def (noT v)
     v)

