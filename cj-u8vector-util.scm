;; (requires
;;  gambit-interpreter-env
;;  cj-math ;; quotient-ceiling
;;  cj-let-named-star ;; let-named*
;;  (cj-test TEST)
;;  )

;; (exports
;;  ;;digit->hexchar
;;  u8vector->hex-string
;;  u8vector->hex-string-lc
;;  u8vector->alphanumeric-string
;;  u8vector->integer
;;  u8vector->integer-string
;;  u8vector->string
;;  string->u8vector
;;  string->u8vector0

;;  write-u8vector
;;  read-u8vector
;;  read-u8vector-from-file
;; )


(declare
 (standard-bindings)
 (extended-bindings)
 (block)
 ;;(fixnum)(not safe)
 )


;; ---- hex encoding (16 bits per char) -----

;; use upper case by default, as (number->string n 16) is using.

(def (digit->hexchar@ i hexbasechar)
     (if (##fixnum.> i 9)
	 (##fixnum.->char
	  (##fixnum.+
	   (##fixnum.<-char hexbasechar)
	   (##fixnum.- i 10)))
	 (##fixnum.->char
	  (##fixnum.+
	   (##fixnum.<-char #\0)
	   i))))


(def (digit->hexchar #(fixnum? i)
		     #!optional
		     (hexbasechar #\A))
     (digit->hexchar@ i hexbasechar))


(def (u8vector->hex-string #(u8vector? u8vec)
			   #!optional
			   (hexbasechar #\A))
     (let* ((length/2 (u8vector-length u8vec))
	    (str (##make-string (* length/2 2))))
       (let loop ((pos 0))
	 (if (< pos length/2)
	     (let* ((v (u8vector-ref u8vec pos))
		    (hi (quotient v 16))
		    (lo (modulo v 16)))
	       (string-set! str (+ (* pos 2) 0)
			    (digit->hexchar@ hi hexbasechar))
	       (string-set! str (+ (* pos 2) 1)
			    (digit->hexchar@ lo hexbasechar))
	       (loop (+ pos 1)))
	     str))))

(def (u8vector->hex-string-lc u8vec)
     (u8vector->hex-string u8vec #\a))


;; ---- alphanumeric encoding (32 bits per char) -----

(def cj-u8vector-util:_chars
     '#(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k
	    #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u
	    #\v #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5))

;; A base 32 encoding (not following any standard)
(def (u8vector->alphanumeric-string #(u8vector? u8vec))
     (let* ((src-len (u8vector-length u8vec))
	    (src-bits (* 8 src-len))
	    (dst-len (quotient-ceiling (* 8 src-len) 5))
	    (str (##make-string dst-len)))
       (let loop ((src-bitpos 0)
		  (dst-pos 0))
	 (if (< src-bitpos src-bits)
	     (let ((src-bytepos (quotient src-bitpos 8))
		   (src-subbit (modulo src-bitpos 8)))
	       (let* ((srcvalue
		       (+ (u8vector-ref u8vec src-bytepos)
			  (* 256 (let ((src-bytepos-next (+ src-bytepos 1)))
				   (if (< src-bytepos-next src-len)
				       (u8vector-ref u8vec src-bytepos-next)
				       0)))))
		      (srcvalue
		       (bitwise-and (arithmetic-shift srcvalue (- src-subbit))
				    31)))
		 (string-set! str dst-pos
			      (vector-ref cj-u8vector-util:_chars srcvalue))
		 (loop (+ src-bitpos 5)
		       (+ dst-pos 1))))
	     str))))

(TEST
 > (u8vector->alphanumeric-string (u8vector))
 ""
 > (u8vector->alphanumeric-string (u8vector 0))
 "aa"
 > (u8vector->alphanumeric-string (u8vector 0 0))
 "aaaa"
 > (u8vector->alphanumeric-string (u8vector 0 0 0))
 "aaaaa"
 > (u8vector->alphanumeric-string (u8vector 0 0 1))
 "aaaca"
 > (u8vector->alphanumeric-string (u8vector 0 1 1))
 "aiaca"
 > (u8vector->alphanumeric-string (u8vector 1 1 1))
 "biaca")


(def (u8vector->integer #(u8vector? v) #!optional big-endian?)
     (let* ((len (u8vector-length v))
	    (len-1 (- len 1)))
       (let lp ((i 0)
		(t 0))
	 (if (>= i len)
	     t
	     (lp (+ i 1)
		 (+ t (arithmetic-shift
		       (u8vector-ref v i)
		       (arithmetic-shift
			(if big-endian?
			    (- len-1 i)
			    i)
			3))))))))

(TEST
> (u8vector->integer (u8vector 1 2) #t)
258
> (u8vector->integer (u8vector 1 0) #t)
256
> (u8vector->integer (u8vector 1 0) #f)
1
> (u8vector->integer (u8vector 0 1) #f)
256
> (u8vector->integer (u8vector ))
0
> (u8vector->integer (u8vector 3))
3
> (u8vector->integer (u8vector 3) #t)
3
> (u8vector->integer (u8vector 1 2 3 4 5 6 7 8) #t)
72623859790382856
> (u8vector->integer (u8vector 1 2 3 4 5 6 7 8) #f)
578437695752307201
)

(def (u8vector->integer-string #(u8vector? v) #!optional big-endian?)
     ;; "cheap" variant in sense of programmer time
     (number->string (u8vector->integer v big-endian?)))


;; also see u8vector.string in oo-vector-lib
(def (u8vector->string #(u8vector? v))
     (let* ((len (u8vector-length v))
	    (str (##make-string len)))
       (let loop ((i 0))
	 (if (< i len)
	     (begin
	       (string-set! str i (integer->char (u8vector-ref v i)))
	       (loop (+ i 1)))
	     str))))

;probably faster, but check first if really correct
; (define u8vector->string
;   (lambda(v)
;     (if (not (##u8vector? v))
; 	(error "wrong type"))
;     (let* ((len (##u8vector-length v))
; 	   (out (##make-string len)))
;       (let loop ((i 0))
; 	(if (##fixnum.< i len)
; 	    (begin
; 	      (##string-set! out i (##u8vector-ref v i))
; 	      (loop (##fixnum.+ i 1)))
; 	    out)))))

;; there's also a slower definition in oo-vector-lib
(def (string->u8vector #(string? v))
     (let* ((len (##string-length v))
	    (out (##make-u8vector len)))
       (let loop ((i 0))
	 (if (##fixnum.< i len)
	     (let ((ch (##string-ref v i)))
	       (or (##char<=? ch (##fixnum.->char 255))
		   (error
		    "string->u8vector: can't convert non-ascii char at pos"
		    i ch))
	       (##u8vector-set! out i ch)
	       (loop (##fixnum.+ i 1)))
	     out))))

;; bad copy paste. but i don't care so much right now.:
(def (string->u8vector0 #(string? v))
     (let* ((len (##string-length v))
	    (out (##make-u8vector (+ len 1))))
       (let loop ((i 0))
	 (if (##fixnum.< i len)
	     (let ((ch (##string-ref v i)))
	       (or (##char<=? ch (##fixnum.->char 255))
		   (error
		    "string->u8vector: can't convert non-ascii char at pos"
		    i ch))
	       (##u8vector-set! out i ch)
	       (loop (##fixnum.+ i 1)))
	     (begin
	       (##u8vector-set! out len 0)
	       out)))))


(def (write-u8vector #(u8vector? v)
		     #!optional
		     (port (current-output-port)))
     (let ((len (u8vector-length v)))
       (let-named*
	loop ((written 0)
	      (warned #f))
	(let ((done (write-subu8vector v written len port)))
	  (let ((written (+ written done)))
	    (if (< written len)
		(begin
		  (warn (string-append "write-u8vector warning: not written"
				       " whole buffer at once, retrying.."))
		  (loop warned: #t))
		(if warned
		    (warn "write-u8vector note: now buffer fully written."))))))))


(def readbuf-size 4096)

(def (read-u8vector #!optional
		    (port (current-output-port))
		    (maxlen #f))
     (define (while-not endval in out)
       (let loop ()
	 (let ((i (in)))
	   (if (eqv? i endval)
	       i
	       (begin
		 ;;(warn "while-not onceagain since I got:" i);;;
		 (out i)
		 (loop))))))

     (define buf (##make-u8vector readbuf-size))

     (if maxlen (error "maxlen not yet supported"))
  
     (with-output-to-u8vector
      (u8vector)
      (lambda ()
	(while-not 0
		   (lambda()
		     (read-subu8vector buf 0 readbuf-size))
		   (lambda(n)
		     (ALL-write-subu8vector buf 0 n))))))


(def (ALL-write-subu8vector uv from to)
     (let ((written (write-subu8vector uv from to)))
       (or (= written (- to from))
	   (error "ALL-write-subu8vector: couldn't write it all out at once"
		  ;; why would that be if the output is an u8vector port?
		  from to written))))


;; This is about 3 times faster than (with-input-from-file path
;; read-u8vector) for an 7.7kb example file.
(def (read-u8vector-from-file path
			      #!optional
			      (maxlen #f))
     (let ((len (file-size path)))
       (let ((v (##make-u8vector len)))
	 (call-with-input-file path
	   (lambda(port)
	     (let ((read (##read-subu8vector v 0 len port)))
	       (if (##fixnum.= read len)
		   v
		   (error (string-append
			   "read-u8vector-from-file: some error "
			   "(maybe timeout?) made it not read the whole file:")
			  path read))))))))

