;(include "cj-standarddeclares.scm")
(declare
 (standard-bindings)
 (extended-bindings)
 (block)
 ;;(fixnum)(not safe)
 )

;; ---- hex encoding (16 bits per char) -----

;; use upper case by default, as (number->string n 16) is using.

(define (digit->hexchar@ i hexbasechar)
  (if (##fixnum.> i 9)
      (##fixnum.->char (##fixnum.+ (##fixnum.<-char hexbasechar) (##fixnum.- i 10)))
      (##fixnum.->char (##fixnum.+ (##fixnum.<-char #\0) i))))
;; why is there no ##integer->char and ##char->integer ? ah, ##fixnum.<-/->char

(define (digit->hexchar i
			#!optional
			(hexbasechar #\A))
  (or (##fixnum? i) (error "wrong argument type, not a fixnum"))
  (digit->hexchar@ i hexbasechar))


(define (u8vector->hex-string u8vec
			      #!optional
			      (hexbasechar #\A))
  (or (u8vector? u8vec) (error "wrong argument type, not an u8vector"))
  (let* ((length/2 (u8vector-length u8vec))
	 (str (##make-string (* length/2 2))))
    (let loop ((pos 0))
      (if (< pos length/2)
	  (let* ((v (u8vector-ref u8vec pos))
		 (hi (quotient v 16))
		 (lo (modulo v 16)))
	    (string-set! str (+ (* pos 2) 0) (digit->hexchar@ hi hexbasechar))
	    (string-set! str (+ (* pos 2) 1) (digit->hexchar@ lo hexbasechar))
	    (loop (+ pos 1)))
	  str))))

(define (u8vector->hex-string-lc u8vec)
  (u8vector->hex-string u8vec #\a))

;; ---- alphanumeric encoding (32 bits per char) -----

(define _chars '#(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k
		      #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u
		      #\v #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5))
;;(^- #u8(..) doesn't work for that.  Maybe one should use some macro magic?)


(define (u8vector->alphanumeric-string u8vec) ;; _binary2text
  (or (u8vector? u8vec) (error "wrong argument type, not an u8vector"))
  ;; -> string
  (let* ((src-len (u8vector-length u8vec))
	 (src-bits (* 8 src-len))
	 (dst-len (quotient/ceiling (* 8 src-len) 5))
	 (str (##make-string dst-len)))
    (let loop ((src-bitpos 0)
	       (dst-pos 0))
      (if (< src-bitpos src-bits)
	  (let ((src-bytepos (quotient src-bitpos 8))
		(src-subbit (modulo src-bitpos 8)))
	    (let* ((srcvalue (+ (u8vector-ref u8vec src-bytepos)
				(* 256 (let ((src-bytepos-next (+ src-bytepos 1)))
					 (if (< src-bytepos-next src-len)
					     (u8vector-ref u8vec src-bytepos-next)
					     0)))))
		   (srcvalue (bitwise-and (arithmetic-shift srcvalue (- src-subbit))
					  31)))
	      (string-set! str dst-pos (vector-ref _chars srcvalue))
	      (loop (+ src-bitpos 5)
		    (+ dst-pos 1))))
	  str))))


(define (u8vector->number u8vec #!optional big-endian?)
  (let* ((len (u8vector-length u8vec))
	 (len-1 (- len 1)))
    (let lp ((i 0)
	     (t 0))
      (if (>= i len)
	  t
	  (lp (+ i 1)
	      (+ t (arithmetic-shift
		    (u8vector-ref u8vec i)
		    (arithmetic-shift
		     (if big-endian?
			 (- len-1 i)
			 i)
		     3))))))))

(TEST
> (u8vector->number (u8vector 1 2) #t)
258
> (u8vector->number (u8vector 1 0) #t)
256
> (u8vector->number (u8vector 1 0) #f)
1
> (u8vector->number (u8vector 0 1) #f)
256
> (u8vector->number (u8vector ))
0
> (u8vector->number (u8vector 3))
3
> (u8vector->number (u8vector 3) #t)
3
> (u8vector->number (u8vector 1 2 3 4 5 6 7 8) #t)
72623859790382856
> (u8vector->number (u8vector 1 2 3 4 5 6 7 8) #f)
578437695752307201
)

(define (u8vector->number-string u8vec #!optional big-endian?)
  ;; "cheap" variant in sense of programmer time
  (number->string (u8vector->number u8vec big-endian?)))


(define (u8vector->string u8vec)
  (or (u8vector? u8vec) (error "wrong argument type, not an u8vector" u8vec))
  (let* ((len (u8vector-length u8vec))
	 (str (##make-string len)))
    (let loop ((i 0))
      (if (< i len)
	  (begin
	    (string-set! str i (integer->char (u8vector-ref u8vec i)))
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

(define string->u8vector
  (lambda(v)
    (or (##string? v) (error "wrong argument type, not a string" v))
    (let* ((len (##string-length v))
	   (out (##make-u8vector len)))
      (let loop ((i 0))
	(if (##fixnum.< i len)
	    (let ((ch (##string-ref v i)))
	      (or (##char<=? ch (##fixnum.->char 255))
		  (error "string->u8vector: can't convert non-ascii char at pos" i ch))
	      (##u8vector-set! out i ch)
	      (loop (##fixnum.+ i 1)))
	    out)))))

;; bad copy paste. but i don't care so much right now.:
(define string->u8vector0
  (lambda(v)
    (or (##string? v) (error "wrong argument type, not a string" v))
    (let* ((len (##string-length v))
	   (out (##make-u8vector (+ len 1))))
      (let loop ((i 0))
	(if (##fixnum.< i len)
	    (let ((ch (##string-ref v i)))
	      (or (##char<=? ch (##fixnum.->char 255))
		  (error "string->u8vector: can't convert non-ascii char at pos" i ch))
	      (##u8vector-set! out i ch)
	      (loop (##fixnum.+ i 1)))
	    (begin
	      (##u8vector-set! out len 0)
	      out))))))

; ------------------
; cj Sun, 17 Jul 2005 00:23:30 +0200

(define (write-u8vector uv
			#!optional
			(port (current-output-port)))
  (let ((len (u8vector-length uv)))
    (let-named*
     loop ((written 0)
	   (warned #f))
     (let ((done (write-subu8vector uv written len)))
       (let ((written (+ written done)))
	 (if (< written len)
	     (begin
	       (warn "write-u8vector warning: not written whole buffer at once, retrying..")
	       (loop warned: #t) ; I've shadowed written, so don't have to give it explicitely ;)
	       )
	     (if warned
		 (warn "write-u8vector note: now buffer fully written."))))))))

;; read everything until #!eof or maxlen is reached.
; (define (read-u8vector uv
; 		       #!optional
; 		       (port (current-output-port))
; 		       #!key
; 		       (maxlen #f))
; ;  (if (keyword? port)
; ;      (read-u8vector uv (current-output-port) port
;   ;ach GRRRRRRRRR.
;   ;so forget it.
;anal?

(define readbuf-size 4096)

(define (read-u8vector #!optional
		       (port (current-output-port))
		       (maxlen #f))
  ;; btw is that slow? keyword params?
  ;;todo.
  
  ;; he habe ich nicht schon mal irgendwo.
  ;; grund des vvec?

  ;;(let loop ((

  ;;(let ((out (u8vector)))


					;   (define (while-not-eof in out)
					;     (
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

  (and maxlen (error "maxlen not yet supported"))
  
  (with-output-to-u8vector (u8vector)
			   (lambda();GRRRRRRRRRRRRRRRRR
			     (while-not 0
					(lambda()
					  (read-subu8vector buf 0 readbuf-size))
					(lambda(n)
					  (ALL-write-subu8vector buf 0 n))))))

(define (ALL-write-subu8vector uv from to)
					;   (let loop ((pos from))
					;     ;; EBEN und dann hatte ich das warning flag.
  (let ((written (write-subu8vector uv from to)))
    (or (= written (- to from))
	(error "ALL-write-subu8vector: couldn't write it all out at once. why if the output is an u8vector port??" from to written))))


(define (read-u8vector-from-file path
				 #!optional
				 (maxlen #f))
  ;; is about 3 times faster than (with-input-from-file path read-u8vector) for an 7.7kb example file.
  (let ((len (file-size path)))
    (let ((v (##make-u8vector len)))
      (call-with-input-file path
	(lambda(port)
	  (let ((read (##read-subu8vector v 0 len port)))
	    (if (##fixnum.= read len)
		v
		(error "read-u8vector-from-file: some error (maybe timeout?) made it not read the whole file:" path read))))))))
