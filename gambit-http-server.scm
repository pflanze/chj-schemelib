; Originally from:
; File: "http.scm", Time-stamp: <2005-05-11 16:40:58 feeley>
; Copyright (C) 2005 by Marc Feeley, All Rights Reserved.

; which has been split into separate files by Christian at pflanze mine nu

(documentation

 ;; define-type's will be auto-documented.
 "you call make-http-server with port-number, timeout, threaded?, and the GET/POST/etc. optional functions."
 "you call http-server-start!(server) on it. no arguments besides the server object. This does open-tcp-server, and calls accept-connections on (server,port)"
 ;;see below for rest of docs.

 "Funcs to be used:"
 "reply"
 "reply-html"
 "current-request  --- why dynamic parm?" "(EH: WHY thread-specific even ?) AHH: ist das weil sonst in dynamic env ein port rum hängt?"
 "div. error functions  if necessary  ?"
 ; wie ist konzept?    muss request holen  dann damit  was machen    dann reply rufen  das printet  es gibt kein reply value  der wird ignored.          wie will ich  [special]  codes  geben ?.
 )


(declare
  (standard-bindings)
  (extended-bindings)
  (block)
;  (not safe) ;; cj: why not safe? (and why does it still output e.g. *** ERROR IN gambit-httpd#hex-digit, "gambit-httpd.scm"@120.29 -- (Argument 2) Exact INTEGER expected  (string-ref "Hallo" "fun")  ?
)
(include "gambit-default-namespace.scm")


(define-type server
  id: c69165bd-c13f-11d9-830f-00039301ba52

  port-number
  timeout
  threaded?
  (method-table unprintable:) ;; unpr by cj, because it is so big.
)

(define-type request
  id: 8e66862f-c143-11d9-9f4e-00039301ba52

  (server unprintable:) ;; (unpr not by cj)
  connection
  method
  uri
  version
  attributes
)


(define unimplemented-method-error>>
  (lambda ()
    (let* ((request (current-request))
           (connection (request-connection request)))
      (connection:unimplemented-method-error>> connection))))

(define unimplemented-method unimplemented-method-error>>)


(define make-http-server
  (lambda (#!key
	   (port-number 80)
	   (timeout     300)
	   (threaded?   #f) ;; cj: remove the ? here? we don't want to set a predicate .   it is the predicate per se    komisch.
	   (OPTIONS     unimplemented-method)
	   (GET         unimplemented-method)
	   (HEAD        unimplemented-method)
	   (POST        unimplemented-method)
	   (PUT         unimplemented-method)
	   (DELETE      unimplemented-method)
	   (TRACE       unimplemented-method)
	   (CONNECT     unimplemented-method))
    (make-server
     port-number
     timeout
     threaded?
     (make-token-table
      ("OPTIONS" OPTIONS)
      ("GET"     GET)
      ("HEAD"    HEAD)
      ("POST"    POST)
      ("PUT"     PUT)
      ("DELETE"  DELETE)
      ("TRACE"   TRACE)
      ("CONNECT" CONNECT)))))

(define http-server-start!
  (lambda (s) ;; ps JA actually IST das bloss ein  server  typ. nicht http-server.
    (let ((server-port
           (open-tcp-server
            (list port-number: (server-port-number s)
                  backlog: 128
                  reuse-address: #t
                  char-encoding: 'latin1
                  eol-encoding: 'cr-lf))))
      (accept-connections s server-port))))


(documentation
 "accept-connections (called internally by http-server-start!)
 loops forever over:"
 (item "accept a connection")
 (item "if threaded? is true, creates a new thread (with dummy input and output ports ((what's up with error port?))), and starts a new threadserve-connection")
 (item "calls (http-server:serve-connection<> hs connection)")
 )

(define accept-connections
  (lambda (http-server server-port)
    (let loop ()
      (let* ((connection
	      (read server-port))
	     (serve-connection
	      (lambda ()
		(with-exception-catcher
		 (lambda (e)
		   (warn "an exception occurred:" e);;; todo mein besser ding what ever.
		   (connection:internal-server-error>> connection))
		 (&
		  (http-server:serve-connection<> http-server connection) ;;; Yet another grund, um namespace anzugeben:  explizit sagen, "das ist eine methode eigentlich" ?.  Dabei ist es eigentlich eine interne hier, hei. *nicht* für export deklariert zu verstehen. hei hum.
		  ;; TODO  was wenn das returned. closen connection und so. und eben error handler.
		  ))
		;; close connection:
		(with-exception-catcher
		 (lambda (e)
		   (warn "close-output-port:" e))
		 (&
		  (close-output-port connection)))
		(with-exception-catcher
		 (lambda (e)
		   (warn "close-input-port:" e))
		 (&
		  (close-input-port connection))))))
	(if (server-threaded? http-server)
	    (let ((dummy-port (open-string))) ;; b14 is offering (open-dummy) instead and using it in it's web-server example.
	      (parameterize ((current-input-port dummy-port)
			     (current-output-port dummy-port))
			    (thread-start!
			     (make-thread
			      ;;^- the 'trick for cleaning up dynamic env'  (w/o explicit delimited conts support) ---- ehr, no, not for that, but for threading really.
			      serve-connection))))  
	    (serve-connection)))
      (loop))))


;; ------------------------------------------------------------------
;;lowlevel replies ?

(define connection:error>>
  (lambda (connection code html)
    (if code
	;; write a http/1.x header(s huh aha)
	(display
	 `()))
    (write-html html connection)
    (close-port connection)))

(define connection:unimplemented-method-error>>
  (lambda (connection)
    (connection:error>>
     connection
     (<html> (<head> (<title> "501 Method Not Implemented"))
             (<body>
              (<h1> "Method Not Implemented"))))))

(define connection:bad-request-error>>
  (lambda (connection #!optional msg)
;    (error "bad-request-error, do you want to examine?" msg) ;;todo for debugging only  note exceptions heere are NOT CATCHED YET in multithreaded case!
;;backtrace in browser or even better error log would be nice as well.
    (connection:error>>
     connection
     (<html> (<head> (<title> "400 Bad Request"))
             (<body>
              (<h1> "Bad Request")
              (<p> "Your browser sent a request that this server could "
                   "not understand."
                   (<br>))
	      ;;cj 'hack':
	      (if msg
		  (<p> "(Reason: " msg ")")
		  ""))))))

;; cj:   todo maybe fix details
;; Status Code Definitions:  http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
;; btwTODO:  shouldn't it return a proper reply header ?? with the code,  yes. if http1.x.
(define connection:internal-server-error>> 
  (lambda (connection)
    (connection:error>>
     connection
     (<html> (<head> (<title> "500 Internal Server Error"))
	     (<body>
	      (<h1> "Internal Server Error")
	      (<p> "An error occurred. If this persists, please "
		   "contact the server administrator " ;; todo: at xyz..
		   "and inform him about this problem and what you might "
		   "have done which could have led to this error. "
		   "He will also find some information in the server log."))))))

;; ------------------------------------------------------------------
;;normal replies?

(define *default-reply-encoding* 'latin1) ;; this is the default, copied to current-reply-encoding on thread creation. ok?
;; ODER IST DAS UGLY?todo
(define current-reply-encoding (make-parameter *default-reply-encoding*))

; (define somemessage "Hallöchen")
; (warn "somemessage='" somemessage "' of length " (string-length somemessage))


(define connection:headers>> ;; sollte es eben DOCH NICHT in plural sein  ?  header-lines but header ?
  (lambda (connection http-version status statusname header-alist)
    (display (list http-version " " status " " statusname "\n")
	     connection)
    (for-each (lambda (p)
		(display (list (car p)
			       ": "
			       (cdr p)
			       "\n")
			 connection))
	      header-alist)
    (newline connection)))

(TEST
 > (call-with-output-string "" (lambda (c) (connection:headers>> c 'HTTP/1.1 200 "OK" '(("Chris" . "good")))))
 "HTTP/1.1 200 OK\nChris: good\n\n"
);; HMMM todo: **escaping**?


(define with-output-as-reply>>  ;;;; nimmt current env.
  (lambda (thunk) ;;; und schreibt darauf  was der thunk schreibt.  dazwischen liegt ein u8vector leider.DERZEIT.
    (let* ((request
            (current-request))
           (connection
            (request-connection request))
           (version
            (request-version request)))
      (if (or (eq? version 'HTTP/1.0)
              (eq? version 'HTTP/1.1))
          (let ((message
                 (with-output-to-u8vector
                  `(char-encoding: ,(current-reply-encoding)
				   eol-encoding: cr-lf)
                  thunk)))
            (display
             `(,version " 200 OK\n"
			"Content-Length: " ,(u8vector-length message) "\n"
			"Content-Type: text/html; charset=ISO-8859-1\n"	;;; çççççTODO
			"Connection: close\n"
			"\n")
             connection)
            (write-subu8vector message 0 (u8vector-length message) connection))
          (with-output-to-port connection thunk)))))

(define reply-html>>
  (lambda (html)
    (with-output-as-reply>>
     (lambda () (write-html html)))))

(define current-request
  (lambda ()
    (thread-specific (current-thread)))) ; request is stored in thread

;------------------------------------------------------------------------------

(define *input-timeout* 120) ;; 300 is really too much I think. ok? (nicht mal für debugging relevant,right?)
(define *output-timeout* 120)

(define http-server:serve-connection<>
  (lambda (http-server connection) ;; returns if serving on this connection is finished; may throw exceptions!

    ;; Configure the connection with the client so that
    ;; ... the read operation will fail
    ;; (and the thread will terminate).
    ;; todo frage:  muss man connection nicht closen ? (sonst dos problem)  na, muss man nicht einen excn handler aufsetzen der eben immer close macht?.  ((ps unter dos sollte timeout verkleinert werden..))
    (input-port-timeout-set! connection *input-timeout*)
    (output-port-timeout-set! connection *output-timeout*)

    (let loop () ;; there could be multiple requests.

      (let ((req (read-line connection)))
	(if (not (string? req))
	    (connection:bad-request-error>> connection)
	    (let* ((end
		    (let loop ((i 0))
		      (cond ((= i (string-length req))
			     #f)
			    ((char=? (string-ref req i) #\space)
			     i)
			    (else
			     (loop (+ i 1))))))
		   (method-index
		    (and end
			 (token-table-lookup-substring
			  (server-method-table http-server)
			  req
			  0
			  end))))
	      (if method-index

		  (parse-uri
		   req
		   (+ end 1)
		   (string-length req)
		   #t
		   (lambda (uri i)

		     (define handle-version
		       (lambda (version)
			 (case version
			   ((HTTP/1.0 HTTP/1.1)
			    (let ((attributes (connection:headers<< connection)))
			      (if attributes
				  (handle-request version attributes)
				  ;; (note by cj: false does not mean no attributes, but an error has happened. (would it be better to throw an exception instead?))  --- aha (todo?): exception die dann ja eben doch   auf die leitung muss  bloss die frage  wann auf welche weise   und ich denk das hier ist schon appropriate!
				  (connection:bad-request-error>> connection "error reading or parsing the headers"))))
			   ((#f)
					; this is an HTTP/0.9 request
			    (handle-request 'HTTP/0.9 '()))
			   (else
					; (cj: this case can't happen, right?)
			    (connection:bad-request-error>> connection)))))

		     (define handle-request
		       (lambda (version attributes)
			 (let ((method-table (server-method-table http-server)))
			   (let ((request
				  (make-request
				   http-server
				   connection
				   (vector-ref method-table method-index)
				   uri
				   version
				   attributes)))
			     (thread-specific-set! (current-thread) request))
			   ((vector-ref method-table (+ method-index 1))))))

		     (cond ((not uri)
			    (connection:bad-request-error>> connection))
			   ((not (< i (string-length req)))
			    (handle-version #f))
			   ((not (char=? (string-ref req i) #\space))
			    (connection:bad-request-error>> connection))
			   (else
			    (cond ((token-table-substring-ref
				    version-table
				    req
				    (+ i 1)
				    (string-length req))
				   => handle-version)
				  (else
				   (connection:bad-request-error>> connection)))))))

		  (connection:unimplemented-method-error>> connection))))))))

(define version-table
  (make-token-table
   ("HTTP/1.0" 'HTTP/1.0)
   ("HTTP/1.1" 'HTTP/1.1)
   ;;cj: (well, maybe not really necessary)  todo if so then also lowercase method names
   ("http/1.0" 'HTTP/1.0)
   ("http/1.1" 'HTTP/1.1)
   ))

(define connection:headers<<
  (lambda (connection)			; -> string-alist
    (let loop ((attributes '()))
      (let ((line (read-line connection)))
        (cond ((or (not line) ;; cj: when does this happen?   did marc miswrite this?
		   (eof-object? line))
               #f)
              ((= (string-length line) 0)
               attributes)
              (else
               (let ((attribute (split-attribute-line line)))
                 (if attribute
                     (loop (cons attribute attributes))
                     #f))))))))

(define find-char-pos
  (lambda (str char)
    (let loop ((i 0))
      (if (< i (string-length str))
          (if (char=? char (string-ref str i))
              i
              (loop (+ i 1)))
          #f))))

(define split-attribute-line
  (lambda (line)
    (let ((pos (find-char-pos line #\:)))
      ;;       (and pos
      ;; 	   (< (+ pos 1) (string-length line))
      ;; 	   (char=? #\space (string-ref line (+ pos 1)))	;;cj: who requires that? apache accepts it without space.
      ;; 	   (cons (substring line 0 pos)
      ;; 		 (substring line (+ pos 2) (string-length line))))
      (and pos
           (cons (substring line 0 pos)
                 (let ((line-len (string-length line)))
		   (substring line
			      (let ((pos+1 (+ pos 1)))
				(if (and (< pos+1 line-len)
					 (char=? #\space (string-ref line pos+1)))
				    (+ pos 2)
				    pos+1))
			      line-len)))))))


(TEST
 > (call-with-input-string "a: aa\nb: bbb\n\n" connection:headers<<)
 (("b" . "bbb") ("a" . "aa"))
 > (call-with-input-string "a: aa\nb: bbb\n" connection:headers<<)
 #f
 > (call-with-input-string "a: aa\nb:bbb\n\n" connection:headers<<)
 (("b" . "bbb") ("a" . "aa"))
 > (call-with-input-string "a: aa\nb:bbb \n\n" connection:headers<<)
 (("b" . "bbb ") ("a" . "aa"))
 > (call-with-input-string "a: aa\n\n\n" connection:headers<<)
 (("a" . "aa"))
 > (call-with-input-string "\n\n\n" connection:headers<<)
 ()
 )
