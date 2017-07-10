;;; Copyright 2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Safe fixnum operations:

;; In the sense that even when used in (not safe) mode, they do check
;; for over-/underflows. I.e. make (not safe) only about *type*
;; safety, not values, or so.

;; They are meant to be used with cj-inline.

(require define-macro-star
	 test
	 cj-test
	 (cj-inline inline-through-decompile))


(export (macros safe-fx#+
		safe-fx#-
		safe-fx#*
		safe-fx#=
		safe-fx#positive?
		safe-fx#negative?
		safe-fx#zero?
		safe-fx#<
		safe-fx#>
		safe-fx#<=
		safe-fx#>=
		)
	(inline safe-fx#inc
		safe-fx#dec)
	(macros use-safe-fx
		inline-safe-fx))


;; Any faster way (this does superfluous type checks if used in a not
;; safe context)? How about ASM?
(define-macro* (safe-fx#+ . args)
  `(let ()
     (declare (safe))
     (fx+ ,@args)))

(define-macro* (safe-fx#- . args)
  `(let ()
     (declare (safe))
     (fx- ,@args)))

(define-macro* (safe-fx#* . args)
  `(let ()
     (declare (safe))
     (fx* ,@args)))

(define-macro* (safe-fx#= . args)
  `(let ()
     (declare (safe))
     (fx= ,@args)))

(define-macro* (safe-fx#positive? . args)
  `(let ()
     (declare (safe))
     (fxpositive? ,@args)))

(define-macro* (safe-fx#negative? . args)
  `(let ()
     (declare (safe))
     (fxnegative? ,@args)))

(define-macro* (safe-fx#zero? . args)
  `(let ()
     (declare (safe))
     (fxzero? ,@args)))


(define-macro* (safe-fx#< . args)
  `(let ()
     (declare (safe))
     (fx< ,@args)))

(define-macro* (safe-fx#> . args)
  `(let ()
     (declare (safe))
     (fx> ,@args)))

(define-macro* (safe-fx#<= . args)
  `(let ()
     (declare (safe))
     (fx<= ,@args)))

(define-macro* (safe-fx#>= . args)
  `(let ()
     (declare (safe))
     (fx>= ,@args)))




;; (define (safe-fx#inc x)
;;   (let ((v (fx+ x 1)))
;;     (if (fx< x v)
;; 	v
;; 	(error "fixnum overflow"))))

;; (define (safe-fx#dec x)
;;   (let ((v (fx- x 1)))
;;     (if (fx> x v)
;; 	v
;; 	(error "fixnum underflow"))))

;; But actually better be safe for types, too? I mean, ehr, just have
;; ops that are always working on fx but always safe? Because
;; otherwise, when using an inc in another place and it produces a
;; bignum, we're going to have a problem again. Moo.

(define-inline (safe-fx#inc x)
  (declare (safe))
  (fx+ x 1))

(define-inline (safe-fx#dec x)
  (declare (safe))
  (fx- x 1))



(define-macro* (use-safe-fx . ops)
  `(##namespace ("safe-fx#" ,@ops)))

(define-macro* (inline-safe-fx . ops)
  (let ((ops (if (null? ops)
		 '(inc dec)
		 ops)))
    `(begin
       ,@(map (lambda (op*)
		(let ((op (string->symbol
			   (string-append "safe-fx#"
					  (symbol->string (source-code op*))))))
		  `(define ,op (inline ,op))))
	      ops))))


;; Testing
(define (safe-fx:test a b)
  (declare (not safe))
  (safe-fx#+ (safe-fx#inc a) (safe-fx#inc b)))


(TEST
 > (safe-fx:test 10 20)
 32
 > (fixnum? max-fixnum)
 #t
 > (fixnum? (inc max-fixnum))
 #f
 > (%try (safe-fx:test 10 max-fixnum))
 (exception text: "FIXNUM overflow\n(fx+ 2305843009213693951 1)\n")
 > (%try (safe-fx:test (/ max-fixnum 2) (/ max-fixnum 2)))
 (exception text: "(Argument 1) FIXNUM expected\n(fx+ 2305843009213693951/2 1)\n")
 > (%try (safe-fx:test (arithmetic-shift max-fixnum 1) (arithmetic-shift max-fixnum 1)))
 (exception text: "(Argument 1) FIXNUM expected\n(fx+ 4611686018427387902 1)\n"))

