;; Originally from: File: "http.scm", Time-stamp: <2005-05-11 16:40:58 feeley>
;;                  Copyright (C) 2005 by Marc Feeley, All Rights Reserved.
;; Copyright 2005, 2018 Christian Jaeger <ch@christianjaeger.ch>

(require easy
	 (srfi-1 fold-right)
	 (cj-string string-list->string)
	 if-let
	 test)

(export (class uri)
	parse-uri
	parse-uri-query

	string->uri
	string->uri-query
	encode-for-uri

	uri.string)

(include "cj-standarddeclares.scm")


;; URI parsing.

(define hex-digit
  (lambda (str i)
    (let ((n (char->integer (string-ref str i))))
      (cond ((and (>= n 48) (<= n 57))
             (- n 48))
            ((and (>= n 65) (<= n 70))
             (- n 55))
            ((and (>= n 97) (<= n 102))
             (- n 87))
            (else
             #f)))))

(define hex-octet
  (lambda (str i)
    (let ((n1 (hex-digit str i)))
      (and n1
           (let ((n2 (hex-digit str (+ i 1))))
             (and n2
                  (+ (* n1 16) n2)))))))

(define plausible-hex-escape?
  (lambda (str end j)
    (and (< (+ j 2) end)
         (not (control-or-space-char? (string-ref str (+ j 1))))
         (not (control-or-space-char? (string-ref str (+ j 2)))))))

(define control-or-space-char?
  (lambda (c)
    (or (not (char<? #\space c))
        (not (char<? c #\x7f)))))

(define excluded-char?
  (lambda (c)
    (or (not (char<? #\space c))
        (not (char<? c #\x7f))
        (char=? c #\<)
        (char=? c #\>)
        (char=? c #\#)
        (char=? c #\%)
        (char=? c #\")
        (char=? c #\{)
        (char=? c #\})
        (char=? c #\|)
        (char=? c #\\)
        (char=? c #\^)
        (char=? c #\[)
        (char=? c #\])
        (char=? c #\`))))

(define extract-escaped
  (lambda (str start n)
    (let ((result (make-string n)))
      (let loop ((i start) (j 0))
        (if (< j n)
            (let ((c (string-ref str i)))
              (if (char=? c #\%)
                  (let ((n (hex-octet str (+ i 1))))
                    (and n
                         (begin
                           (string-set! result j (integer->char n))
                           (loop (+ i 3)
                                 (+ j 1)))))
                  (begin
                    (string-set! result j (if (char=? c #\+) #\space c))
                    (loop (+ i 1)
                          (+ j 1)))))
            result)))))

(defclass (uri [string? scheme]
		      [string? authority] ;; also gets to contain authentication and port info if present!
		      [string? path]
		      [(maybe (alist-of string? string?)) query]
		      [(maybe string?) fragment]))


(define parse-uri
  (lambda (str start end decode? cont) -> uri?

     (define (*uri _uri)
       (insert-result-of `(uri ,@(map (lambda (i)
					`(vector-ref _uri ,i))
				      (iota 5)))))
     (let ((uri (vector #f #f "" #f #f)))

       (define (uri-scheme-set! uri val) (vector-set! uri 0 val))
       (define (uri-authority-set! uri val) (vector-set! uri 1 val))
       (define (uri-path-set! uri val) (vector-set! uri 2 val))
       (define (uri-query-set! uri val) (vector-set! uri 3 val))
       (define (uri-fragment-set! uri val) (vector-set! uri 4 val))
      

       (define extract-string
	 (lambda (i j n)
	   (if decode?
	       (extract-escaped str i n)
	       (substring str i j))))

       (define extract-query
	 (lambda (i j n)
	   (if decode?
	       (parse-uri-query
		str
		i
		j
		decode?
		(lambda (bindings end)
		  bindings))
	       (substring str i j))))

       (define state0		   ; possibly inside the "scheme" part
	 (lambda (i j n)
	   (if (< j end)
	       (let ((c (string-ref str j)))
		 (cond ((char=? c #\:)
			(if (= n 0)
			    (state2 j (+ j 1) 1) ; the ":" is in the "path" part
			    (let ((scheme (extract-string i j n)))
			      (and scheme
				   (begin
				     (uri-scheme-set! uri scheme)
				     (if (and (< (+ j 2) end)
					      (char=? (string-ref str (+ j 1))
						      #\/)
					      (char=? (string-ref str (+ j 2))
						      #\/))
					 (state1 (+ j 3) (+ j 3) 0)
					 (state2 (+ j 1) (+ j 1) 0)))))))
		       ((char=? c #\/)
			(if (and (= n 0)
				 (< (+ j 1) end)
				 (char=? (string-ref str (+ j 1)) #\/))
			    (state1 (+ j 2) (+ j 2) 0)
			    (state2 i (+ j 1) (+ n 1))))
		       ((char=? c #\?)
			(let ((path (extract-string i j n)))
			  (and path
			       (begin
				 (uri-path-set! uri path)
				 (state3 (+ j 1) (+ j 1) 0)))))
		       ((char=? c #\#)
			(let ((path (extract-string i j n)))
			  (and path
			       (begin
				 (uri-path-set! uri path)
				 (state4 (+ j 1) (+ j 1) 0)))))
		       ((char=? c #\%)
			(and (plausible-hex-escape? str end j)
			     (state0 i (+ j 3) (+ n 1))))
		       ((control-or-space-char? c)
			(let ((path (extract-string i j n)))
			  (and path
			       (begin
				 (uri-path-set! uri path)
				 j))))
		       (else
			(state0 i (+ j 1) (+ n 1)))))
	       (let ((path (extract-string i j n)))
		 (and path
		      (begin
			(uri-path-set! uri path)
			j))))))

       (define state1			; inside the "authority" part
	 (lambda (i j n)
	   (if (< j end)
	       (let ((c (string-ref str j)))
		 (cond ((char=? c #\/)
			(let ((authority (extract-string i j n)))
			  (and authority
			       (begin
				 (uri-authority-set! uri authority)
				 (state2 j (+ j 1) 1)))))
		       ((char=? c #\?)
			(let ((authority (extract-string i j n)))
			  (and authority
			       (begin
				 (uri-authority-set! uri authority)
				 (state3 (+ j 1) (+ j 1) 0)))))
		       ((char=? c #\#)
			(let ((authority (extract-string i j n)))
			  (and authority
			       (begin
				 (uri-authority-set! uri authority)
				 (state4 (+ j 1) (+ j 1) 0)))))
		       ((char=? c #\%)
			(and (plausible-hex-escape? str end j)
			     (state1 i (+ j 3) (+ n 1))))
		       ((control-or-space-char? c)
			(let ((authority (extract-string i j n)))
			  (and authority
			       (begin
				 (uri-authority-set! uri authority)
				 j))))
		       (else
			(state1 i (+ j 1) (+ n 1)))))
	       (let ((authority (extract-string i j n)))
		 (and authority
		      (begin
			(uri-authority-set! uri authority)
			j))))))

       (define state2			; inside the "path" part
	 (lambda (i j n)
	   (if (< j end)
	       (let ((c (string-ref str j)))
		 (cond ((char=? c #\?)
			(let ((path (extract-string i j n)))
			  (and path
			       (begin
				 (uri-path-set! uri path)
				 (state3 (+ j 1) (+ j 1) 0)))))
		       ((char=? c #\#)
			(let ((path (extract-string i j n)))
			  (and path
			       (begin
				 (uri-path-set! uri path)
				 (state4 (+ j 1) (+ j 1) 0)))))
		       ((char=? c #\%)
			(and (plausible-hex-escape? str end j)
			     (state2 i (+ j 3) (+ n 1))))
		       ((control-or-space-char? c)
			(let ((path (extract-string i j n)))
			  (and path
			       (begin
				 (uri-path-set! uri path)
				 j))))
		       (else
			(state2 i (+ j 1) (+ n 1)))))
	       (let ((path (extract-string i j n)))
		 (and path
		      (begin
			(uri-path-set! uri path)
			j))))))

       (define state3			; inside the "query" part
	 (lambda (i j n)
	   (if (< j end)
	       (let ((c (string-ref str j)))
		 (cond ((char=? c #\#)
			(let ((query (extract-query i j n)))
			  (and query
			       (begin
				 (uri-query-set! uri query)
				 (state4 (+ j 1) (+ j 1) 0)))))
		       ((char=? c #\%)
			(and (plausible-hex-escape? str end j)
			     (state3 i (+ j 3) (+ n 1))))
		       ((control-or-space-char? c)
			(let ((query (extract-query i j n)))
			  (and query
			       (begin
				 (uri-query-set! uri query)
				 j))))
		       (else
			(state3 i (+ j 1) (+ n 1)))))
	       (let ((query (extract-query i j n)))
		 (and query
		      (begin
			(uri-query-set! uri query)
			j))))))

       (define state4			; inside the "fragment" part
	 (lambda (i j n)
	   (if (< j end)
	       (let ((c (string-ref str j)))
		 (cond ((char=? c #\%)
			(and (plausible-hex-escape? str end j)
			     (state4 i (+ j 3) (+ n 1))))
		       ((control-or-space-char? c)
			(let ((fragment (extract-string i j n)))
			  (and fragment
			       (begin
				 (uri-fragment-set! uri fragment)
				 j))))
		       (else
			(state4 i (+ j 1) (+ n 1)))))
	       (let ((fragment (extract-string i j n)))
		 (and fragment
		      (begin
			(uri-fragment-set! uri fragment)
			j))))))

       (let ((i (state0 start start 0)))
	 (cont (and i (*uri uri))
	       (or i start))))))

(define parse-uri-query
  (lambda (str start end decode? cont)
    (let ((rev-bindings '()))

      (define extract-string
        (lambda (i j n)
          (if decode?
              (extract-escaped str i n)
              (substring str i j))))

      (define state0
        (lambda (i j n)
          (if (< j end)
            (let ((c (string-ref str j)))
              (cond ((char=? c #\%)
                     (and (plausible-hex-escape? str end j)
                          (state0 i
                                  (+ j 3)
                                  (+ n 1))))
                    ((char=? c #\=)
                     (let ((name (extract-string i j n)))
                       (and name
                            (let ((j (+ j 1)))
                              (state1 j
                                      j
                                      0
                                      name)))))
                    ((char=? c #\&)
                     #f)
                    ((excluded-char? c)
                     (if (= n 0)
                         j
                         #f))
                    (else
                     (state0 i
                             (+ j 1)
                             (+ n 1)))))
            (if (= n 0)
                j
                #f))))

      (define state1
        (lambda (i j n name)
          (if (< j end)
            (let ((c (string-ref str j)))
              (cond ((char=? c #\%)
                     (and (plausible-hex-escape? str end j)
                          (state1 i
                                  (+ j 3)
                                  (+ n 1)
                                  name)))
                    ((char=? c #\&)
                     (let ((val (extract-string i j n)))
                       (and val
                            (let ((j (+ j 1)))
                              (set! rev-bindings
                                    (cons (cons name val) rev-bindings))
                              (and (< j end)
                                   (state0 j
                                           j
                                           0))))))
                    ((char=? c #\=)
                     #f)
                    ((excluded-char? c)
                     (let ((val (extract-string i j n)))
                       (and val
                            (begin
                              (set! rev-bindings
                                    (cons (cons name val) rev-bindings))
                              j))))
                    (else
                     (state1 i
                             (+ j 1)
                             (+ n 1)
                             name))))
            (let ((val (extract-string i j n)))
              (and val
                   (begin
                     (set! rev-bindings
                           (cons (cons name val) rev-bindings))
                     j))))))

      (let ((i (state0 start start 0)))
        (cont (and i (reverse rev-bindings))
              (or i start))))))

(define string->uri
  (lambda (str decode?)
    (parse-uri str
               0
               (string-length str)
               decode?
               (lambda (uri end)
                 (and (= end (string-length str))
                      uri)))))

(TEST
> (cdr (##vector->list (string->uri "http://chris@www.ethz.ch:31/" #t)))
("http" "chris@www.ethz.ch:31" "/" #f #f)
)


(define string->uri-query
  (lambda (str decode?)
    (parse-uri-query str
                     0
                     (string-length str)
                     decode?
                     (lambda (query end)
                       (and (= end (string-length str))
                            query)))))

(define encode-for-uri
  (lambda (str)
    (let ((end (string-length str)))

      (define copy
        (lambda (result i j n)
          (if (< i j)
              (let ((new-j (- j 1))
                    (new-n (- n 1)))
                (string-set! result new-n (string-ref str new-j))
                (copy result i new-j new-n))
              result)))

      (define hex
        (lambda (x)
          (string-ref "0123456789ABCDEF" (bitwise-and x 15))))

      (define encode
        (lambda (i j n)
          (if (< j end)
              (let ((c (string-ref str j)))
                (cond ((char=? c #\space)
                       (let ((result (encode (+ j 1) (+ j 1) (+ n 1))))
                         (string-set! result n #\+)
                         (copy result i j n)))
                      ((or (char=? c #\+)
                           (excluded-char? c))
                       (let ((result (encode (+ j 1) (+ j 1) (+ n 3))))
                         (let* ((x (char->integer c))
                                (hi (hex (arithmetic-shift x -4)))
                                (lo (hex x)))
                           (string-set! result n #\%)
                           (string-set! result (+ n 1) hi)
                           (string-set! result (+ n 2) lo))
                         (copy result i j n)))
                      (else
                       (encode i (+ j 1) (+ n 1)))))
              (let ((result (make-string n)))
                (copy result i j n)))))

      (encode 0 0 0))))

;; cj Fri, 31 Mar 2006 17:18:11 +0200

(define (scheme:after scheme)
  (cond ((or (string-ci=? scheme "http")
	     (string-ci=? scheme "ftp"))
	 "://")
	((string-ci=? scheme "mailto")
	 ":")
	(else
	 (error "unknown scheme: " scheme))))
;; hmm aber  dispatch  anhand scheme muss ich eh machen   oder   wegen query string  oder?  knowledge is everything   might be at least  or so.  nvd-uri lesen or so.
;strut  space   ave   .


(define (alis->query-string-list alis tail)
  (let ((l (fold-right (lambda (p tail)
			 (cons "&"
			       (cons (encode-for-uri (car p))
				     (cons "="
					   (cons (encode-for-uri (cdr p))
						 tail)))))
		       tail
		       alis)))
    (if (pair? l)
	;;(begin (set-car! l "?") l)
	(cons "?" (cdr l)) ;; only almost unmeasurably slower
	l)))


(define (uri.string-list uri)
  (letrec ((scheme (uri.scheme uri))
	   (*all (& (*scheme)))
	   (*scheme
	    (& (let ((tail (*scheme-after)))
		 (cons scheme tail))))
	   (*scheme-after
	    (& (let ((tail (*authority)))
		 (cons (scheme:after scheme)
		       tail))))
	   (*authority
	    (& (let ((tail (*path)))
		 (if-let ((ua (uri.authority uri)))
			 (cons ua tail)
			 tail))))
	   (*path
	    (& (let ((tail (*query)))
		 (cons (uri.path uri)
		       tail))))
	   (*query
	    (& (let ((tail (*fragment)))
		 (if-let ((uq (uri.query uri)))
			 (cond ((pair? uq)
				(alis->query-string-list uq tail))
			       ((string=? uq "")
				tail)
			       (else
				(cons "?" (cons uq tail))))
			 tail))))
	   (*fragment
	    (& (let ((tail '()))
		 (if-let ((uf (uri.fragment uri)))
			 (cons uf tail)
			 tail)))))
    (*all)))

(define (uri.string uri)
  (string-list->string (uri.string-list uri)))


(TEST
> (uri.string (string->uri "http://www.ethz.ch/grg?ab=k%e3ppeli&d%f6ner=1" #t))
"http://www.ethz.ch/grg?ab=k%E3ppeli&d%F6ner=1"
)


; todo:
; > (string->uri "http://www.ethz.ch/ethlife//?fun=1&for=4&you" #t)
; #f  <- bad.
; > (string->uri "http://www.ethz.ch/ethlife//?fun=1&for=4&you=12" #t)
; #<uri #13
;       scheme: "http"
;       authority: "www.ethz.ch"
;       path: "/ethlife//"
;       query: (("fun" . "1") ("for" . "4") ("you" . "12"))
;       fragment: #f>
; > (string->uri "http://www.ethz.ch?fo=12;bla=14" #t)
; #f <- very bad

; ps  ja  evtl ist reverse aliste von query params richtiger:  spätere param angaben override former.

; (define u (string->uri "http://www.ethz.ch/grg?ab=k%E4ppeli&d=1" #t))
; (define (test-gambit-uri n fn uri)
;   (*do-times (- n 1)
; 	     (fn uri))
;   (fn uri))

; (define (test-make-string n m)
;   (*do-times (- n 1)
; 	     (##make-string m))
;   (##make-string m))
