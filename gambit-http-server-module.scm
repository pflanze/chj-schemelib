(requires gambit-interpreter-env
	  gambit-html
	  gambit-uri
	  feeley-token-table
	  cj-env
	  ldoc
	  (cj-test TEST)
	  )

(exports

make-http-server
http-server-start!
with-output-as-reply>> ;; reply
reply-html>> ;; reply-html
current-request

;; send-error  is now connection:error>>

;;method-not-implemented-error
;;unimplemented-method  internal
;;bad-request-error

;; from define-type request
make-request
request?
request-attributes
request-attributes-set!
request-connection
request-connection-set!
request-method
request-method-set!
request-server
request-server-set!
request-uri
request-uri-set!
request-version
request-version-set!

;; from define-type server
make-server
server?
server-method-table
server-method-table-set!
server-port-number
server-port-number-set!
server-threaded?
server-threaded?-set!
server-timeout
server-timeout-set!

)

;; ^- really want  a class  based approach   "but"  maybe  auto supported namespace notation would be enough.

;;Internal:
;; serve-connection
;; version-table
;; read-headers
;; find-char-pos
;; split-attribute-line
