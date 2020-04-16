;;; Copyright 2016-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; A representation of char* buffers using Scheme objects. NOTE: does
;; not use a separate type name! (Todo: write a separate cstring.scm
;; as such a wrapper?)

;; Also see u8-parse.scm, cj-u8vector-util.scm (Todo: clean up?)


(require easy
	 (cj-source-util-2 assert)
	 utf8 ;; or include? sigh.
	 unclean
	 (string-util-3 char-list.string-reverse)
	 cj-warn
         show
         (predicates-1 false/2)
         test
	 test-logic
         test-random)


(export u8vector0?
	(method u8vector0.strlen
		u8vector.strlen
		string.utf8-bytes
		string.utf8-u8vector
		string.utf8-u8vector0
		u8vector.utf8-parse
                u8vector.maybe-utf8-parse
		u8vector.utf8-codepoints
		u8vector.show
		u8vector0.utf8-parse
		u8vector0.maybe-utf8-parse
		u8vector0.utf8-codepoints
                u8vector0.show))


(c-declare "
       #include <string.h>
       #include <assert.h>
")


(def (u8vector0? v)
     (and (u8vector? v)
	  (let ((len (u8vector-length v)))
	    (and (>= len 1)
		 (zero? (u8vector-ref v (dec len)))))))

(TEST
 > (u8vector0? (u8vector))
 #f
 > (u8vector0? (u8vector 0))
 #t
 > (u8vector0? (u8vector 1 0))
 #t
 > (u8vector0? (u8vector 0 1))
 #f)


;; Call this strlen and not length since u8vector0.length and
;; u8vector.length would be dangerously ambiguous (a 0 value at the
;; end would make length be reported shorter than expected for work
;; with u8vectors that *do* allow 0 values). Also, this doesn't do
;; UTF-8 decoding which might be expected; thus really reuse the libc
;; name.

(def. (u8vector0.strlen v)
  (assert (u8vector0? v)) ;; XX should really be made part of method? !
  (##c-code "
size_t res= strlen(___CAST(char*,___BODY(___ARG1)));
assert(res <= ___MAX_FIX);
___RESULT= ___FIX(res);
" v))

(TEST
 > (%try-error (u8vector0.strlen (u8vector)))
 #(error "assertment failure: (u8vector0? v)" (u8vector0? '#u8()))
 > (%try-error (u8vector0.strlen (u8vector 100)))
 #(error "assertment failure: (u8vector0? v)" (u8vector0? '#u8(100)))
 > (u8vector0.strlen (u8vector 100 0))
 1
 > (u8vector0.strlen (u8vector 100 99 98 0))
 3
 > (u8vector0.strlen (u8vector 100 99 98 0 3 4 5 0))
 3
 > (.strlen '#u8(195 164 195 182 195 188 0))
 6 ;; whereas those are just 3 characters
 )



;; format as UTF-8

;;(include "utf8.scm") now loaded as normal dependency

(def. (string.utf8-bytes s #!optional (len (string-length s)))
  (let lp ((i 0)
	   (l 0))
    (if (< i len)
	(lp (inc i)
	    (+ l (utf8-bytes (char->integer (string-ref s i)))))
	l)))


;; Also see string->u8vector0 in cj-u8vector-util.scm which can't do
;; UTF-8; XX eliminate it.

(def (make-string->utf8-u8vector* 0? allow-nulls?)
     (lambda (s)
       (let* ((len (string-length s))
	      (bytes (string.utf8-bytes s len))
	      (out (##make-u8vector (if 0? (inc bytes) bytes))))
	 (let lp ((i 0)
		  (i* 0))
	   (if (< i len)
	       (lp (inc i)
		   (u8vector.utf8-put!
                    out i*
                    (let ((n
                           (char->integer (string-ref s i))))
                      (if (or allow-nulls? (not (zero? n)))
                          n
                          (error "null character not allowed")))
                    ))
	       (begin
		 (when 0?
                       (u8vector-set! out bytes 0))
		 out))))))

(def. string.utf8-u8vector (make-string->utf8-u8vector* #f #t))
(def. string.utf8-u8vector0 (make-string->utf8-u8vector* #t #f))


(TEST
 > (string.utf8-u8vector "")
 #u8()
 > (show #)
 (.utf8-u8vector "")
 > (string.utf8-u8vector0 "")
 #u8(0)
 > (show #)
 (.utf8-u8vector0 "")
 > (string.utf8-u8vector "Hello")
 #u8(72 101 108 108 111)
 > (show #)
 (.utf8-u8vector "Hello")
 > (string.utf8-u8vector0 "Hello")
 #u8(72 101 108 108 111 0)
 > (show #)
 (.utf8-u8vector0 "Hello")
 > (string.utf8-u8vector "Hellö")
 #u8(72 101 108 108 195 182)
 > (show #)
 (.utf8-u8vector "Hellö")
 > (string.utf8-u8vector0 "Hellö")
 #u8(72 101 108 108 195 182 0)
 > (show #)
 (.utf8-u8vector0 "Hellö")
 ;; not u8vector0 but embedded \0:
 > (show '#u8(72 101 108 108 195 182 0 65))
 (.utf8-u8vector "Hell\366\0A")
 ;; invalid utf8:
 > (show '#u8(72 101 108 108 195 182 0 65 128))
 (u8vector 72 101 108 108 195 182 0 65 128)

 > (string.utf8-u8vector0 "Hellöl")
 #u8(72 101 108 108 195 182 108 0)
 > (string.utf8-u8vector0 "äöü")
 ;; #u8(#xC3 #xA4  #xC3 #xB6  #xC3 #xBC  0) =
 #u8(195 164 195 182 195 188 0)
 > (%try-error (string.utf8-u8vector0 "Hel\0lo"))
 [error "null character not allowed"]

 ;; Careful, 
 > (show '#u8(72 101 108 108 195 182 0 65 128 0))
 ;; NOT (.utf8-u8vector0 "Hell\366") since
 ;; > (eval #)
 ;; #u8(72 101 108 108 195 182 0)
 (u8vector 72 101 108 108 195 182 0 65 128 0))


(TEST
 > (.utf8-parse '#u8(195 164 195 182 195 188 0))
 "äöü"
 > (.utf8-parse '#u8(195 164 195 182 195 188 0 0))
 "äöü"
 > (%try-error (.utf8-parse '#u8(195 164 195 182 195 0 188 0)))
 [error "utf-8 decoding error" 4 #u8(195 164 195 182 195 0 188 0)]
 > (.utf8-parse '#u8(195 164 195 182 0 195 188 0))
 "äö"
 > (%try-error (.utf8-parse '#u8(195 164 195 0 182 195 188 0)))
 [error "utf-8 decoding error" 2 #u8(195 164 195 0 182 195 188 0)])


(def (<>.utf8-parse T? T.strlen get return error/2)
     (typed-lambda
      (#(T? v))
      (let ((len (T.strlen v)))
	(let lp ((i 0)
		 ;; using a list instead of pre-calculating size, XX room
		 ;; for optimization (also use @u8vector.utf8-get then).
		 (l '())
		 (n 0))
	  (if (< i len)
	      (letv ((maybe-c i*) (get v i))
		    (if maybe-c
			(lp i*
			    (cons maybe-c l)
			    (inc n))
                        ;; could check (= i* i) and if it did advance,
                        ;; be fine with skipping over it, but when
                        ;; would that be useful?
                        (error/2 i v)))
	      (return l))))))

(def (u8vector0:<>.show maybe-utf8-parse constr super-show)
     "Since there's (currently, XX idea?) no way to fall back on method
dispatch, need the |super-show| argument."
     (lambda (v show)
       (cond ((maybe-utf8-parse v)
              ;; XX also check that the number of escapes will be
              ;; reasonably small? !
              => (lambda (str)
                   (let ((res `(,constr ,str)))
                     ;; check that the conversion back is really the same; 
                     (if (equal? (eval res) v)
                         res
                         (super-show v show)))))
             (else
              ;; fall back to boring definition
              (super-show v show)))))

(def (utf8-decoding-error/2 i v)
     (error "utf-8 decoding error" i v))

(def. u8vector.utf8-parse
  (<>.utf8-parse u8vector?
                 u8vector-length
                 u8vector.utf8-get
                 char-list.string-reverse
                 utf8-decoding-error/2))

(def. u8vector.maybe-utf8-parse
  (<>.utf8-parse u8vector?
                 u8vector-length
                 u8vector.utf8-get
                 char-list.string-reverse
                 false/2))

(def. u8vector.utf8-codepoints
  (<>.utf8-parse u8vector?
                 u8vector-length
                 u8vector.utf8-get-codepoint
                 reverse
                 utf8-decoding-error/2))

(def. u8vector.show
  (u8vector0:<>.show u8vector.maybe-utf8-parse
                     `.utf8-u8vector
                     ;; ugly, can't access that definition, have to
                     ;; define it again:
                     (lambda (v show)
                       `(u8vector ,@(u8vector->list v)))))


;; don't call this u8vector0.string -- u8vector.string does *not* do
;; utf8 decoding, also, why hard code this implicitely so hard. It's
;; wrong.

(def. u8vector0.utf8-parse
  (<>.utf8-parse u8vector0?
                 u8vector0.strlen
                 u8vector.utf8-get
                 char-list.string-reverse
                 utf8-decoding-error/2))

(def. u8vector0.maybe-utf8-parse
  (<>.utf8-parse u8vector0?
                 u8vector0.strlen
                 u8vector.utf8-get
                 char-list.string-reverse
                 false/2))

(def. u8vector0.utf8-codepoints
  (<>.utf8-parse u8vector0?
                 u8vector0.strlen
                 u8vector.utf8-get-codepoint
                 reverse
                 utf8-decoding-error/2))

(def. u8vector0.show
  (u8vector0:<>.show u8vector0.maybe-utf8-parse
                     `.utf8-u8vector0
                     u8vector.show))

(TEST
 > (.utf8-parse '#u8(195 164 195 182 195 188 0))
 "äöü"
 > (.utf8-parse '#u8(195 164 195 182 195 188))
 "äöü"

 > (u8vector0.utf8-parse '#u8(195 164 195 182 195 188 0))
 "äöü"
 > (u8vector.utf8-parse '#u8(195 164 195 182 195 188 0))
 "äöü\0"

 > (%try-error (u8vector0.utf8-parse '#u8(195 164 195 182 195 188)))
 #(error "v does not match T?:" #u8(195 164 195 182 195 188))
 ;; ^ XX sigh. stupid system, disjoint of compile-time syntax
 ;; vs. instantiation (at runtime). (C++ templates to the rescue? Or,
 ;; well, you know, just keep referring to it at runtime instead of
 ;; stringifying syntax. About like assert does, really! Todo.)
 > (u8vector.utf8-parse '#u8(195 164 195 182 195 188))
 "äöü"

 > (u8vector0.utf8-parse '#u8(195 164 195 182 195 188 0 0))
 "äöü"
 > (u8vector.utf8-parse '#u8(195 164 195 182 195 188 0 0))
 "äöü\0\0"
 > (%try-error (u8vector0.utf8-parse '#u8(10 128 0)))
 [error "utf-8 decoding error" 1 #u8(10 128 0)]
 > (%try-error (u8vector0.utf8-parse '#u8(195 164 195 182 195 0 188 0)))
 [error "utf-8 decoding error" 4 #u8(195 164 195 182 195 0 188 0)]
 > (%try-error (u8vector.utf8-parse '#u8(195 164 195 182 195 0 188 0)))
 [error "utf-8 decoding error" 4 #u8(195 164 195 182 195 0 188 0)]
 > (u8vector0.utf8-parse '#u8(195 164 195 182 0 195 188 0))
 "äö"
 > (u8vector.utf8-parse '#u8(195 164 195 182 0 195 188 0))
 "äö\0ü\0"
 > (%try-error (u8vector0.utf8-parse '#u8(195 164 195 0 182 195 188 0)))
 [error "utf-8 decoding error" 2 #u8(195 164 195 0 182 195 188 0)])


;; Stronger inversibility test
(TEST
 > (def n 500)
 > (def ts (make-list! n (& (let ((v (random-u8vector 5)))
                              (cons v (show v))))))
 > (for-all ts (lambda-pair ((a b)) (equal? a (eval b))))
 ;; should be guaranteed by code already but hey..
 ()
 > (<= (* 0.92 n)
       (length (filter (comp (lambda (v) (eq? (cadr v) 'u8vector))) ts))
       (* 0.98 n))
 #t)


