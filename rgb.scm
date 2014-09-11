;;; Copyright 2013-2014 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy test)

;; lib

(def (0..1? v)
     (and (real? v)
	  (<= 0 v)
	  (<= v 1)))

(def 01-margin 0.001)
(def min01 (- 01-margin))
(def max01 (+ 1 01-margin))

(def (0..1? v)
     (and (real? v)
	  (<= min01 v)
	  (<= v max01)))



(def +/2 (lambda (a b) (+ a b)))

;; call it mean or average ?
(def mean (compose (C / _ 2) +/2))

(TEST
 > (mean 1 2)
 3/2
 > (mean 1 3)
 2
 > (mean 1 2.)
 1.5)

;; lib, too:

(def hexdigits "0123456789ABCDEF")

(def (number->uc-hex-string/padding #(natural0? x) #(natural0? paddigits))
     (let lp ((digits '())
	      (x x))
       (if (zero? x)
	   (string-pad-left (list->string digits) #\0 paddigits)
	   (let ((d (bitwise-and x 15))
		 (r (arithmetic-shift x -4)))
	     (lp (cons (string-ref hexdigits d) digits)
		 r)))))

(TEST
 > (number->uc-hex-string/padding 0 4)
 "0000"
 > (number->uc-hex-string/padding 1 4)
 "0001"
 > (number->uc-hex-string/padding 15 4)
 "000F"
 > (number->uc-hex-string/padding 18 4)
 "0012"
 > (number->uc-hex-string/padding 65534 4)
 "FFFE"
 > (number->uc-hex-string/padding 65536 4)
 "10000")

;; /lib

;; rgb8 is always in sRGB 'transfer' format (non-linear), ok?
(defstruct rgb8
  #(uint8? r8)
  #(uint8? g8)
  #(uint8? b8))

(def uint8.01
     (C / _ 255))

(def 01.uint8
     (lambda_
      (let ((r (inexact->exact (floor (* _ 256)))))
	(if (= r 256)
	    255
	    r))))

(TEST
 > (F (Lforall '(-1 0 1 2 253 254 255 255. 256)
	       (lambda_ (= (01.uint8 (uint8.01 _)) _))))
 ;; failures are outside of number range, "though"
 (-1 256))

(def. rgb8.r01 (compose uint8.01 rgb8.r8))
(def. rgb8.g01 (compose uint8.01 rgb8.g8))
(def. rgb8.b01 (compose uint8.01 rgb8.b8))

;; RGB in 0..1 floating point range, sRGB 'transfer' format
(defstruct rgb01t
  #(0..1? r01t)
  #(0..1? g01t)
  #(0..1? b01t))

;; RGB in 0..1 floating point range, linear (proportional to physical
;; light energy, right?) format
(defstruct rgb01l
  #(0..1? r01l)
  #(0..1? g01l)
  #(0..1? b01l))

(def. rgb01t.r01l (compose srgb:transfer.lum rgb01t.r01t))
(def. rgb01t.g01l (compose srgb:transfer.lum rgb01t.g01t))
(def. rgb01t.b01l (compose srgb:transfer.lum rgb01t.b01t))

(def. rgb01l.r01t (compose srgb:lum.transfer rgb01l.r01l))
(def. rgb01l.g01t (compose srgb:lum.transfer rgb01l.g01l))
(def. rgb01l.b01t (compose srgb:lum.transfer rgb01l.b01l))

(def. rgb01t.r8 (compose 01.uint8 rgb01t.r01t))
(def. rgb01t.g8 (compose 01.uint8 rgb01t.g01t))
(def. rgb01t.b8 (compose 01.uint8 rgb01t.b01t))

(def. rgb01l.r8 (compose 01.uint8 rgb01l.r01t))
(def. rgb01l.g8 (compose 01.uint8 rgb01l.g01t))
(def. rgb01l.b8 (compose 01.uint8 rgb01l.b01t))


(TEST
 > (.r01 (rgb8 0 255 128))
 0
 > (.b01 (rgb8 0 255 128))
 128/255
 )

(def. (rgb8.rgb01t x)
  (let-rgb8 ((r g b) x)
	    (rgb01t (uint8.01 r)
		    (uint8.01 g)
		    (uint8.01 b))))

(def. (rgb01t.rgb01l x)
  ;; XX evil, too much duplication. this is optimization here
  ;; ah and at least  have   map functions  right? evil.
  (let-rgb01t ((r g b) x)
	      (let ((conv srgb:transfer.lum))
	       (rgb01l (conv r)
		       (conv g)
		       (conv b)))))

(def. rgb8.rgb01l
  (compose rgb01t.rgb01l rgb8.rgb01t))

(def. rgb01t.rgb01t identity)
(def. rgb01l.rgb01l identity)
;; XXX rgb01l.rgb01t

(def. (rgb01t.rgb8 x)
  (let-rgb01t ((r g b) x)
	      (rgb8 (01.uint8 r)
		    (01.uint8 g)
		    (01.uint8 b))))

;; XXX rgb01l.rgb8

(def. rgb8.rgb8 identity)


(TEST
 ;; for all accessors, converted object should give the same value as
 ;; original
 > (def accessors (list .r01 .g01 .b01))
 > (def x (rgb8 13 7 255))
 > (def x* (.rgb01 x))
 > (F (Lforall accessors (lambda_ ((on _ =) x x*))))
 ()
 )


;; now to the actual meat:

(defmacro (def-rgb01 name e)
  (let ((prefixed (lambda (prefix)
		    (source.symbol-append prefix name))))    
    (pp-through
     `(begin
	(def ,(prefixed "rgb01:") ,e)
	(def. ,(prefixed "rgb01.") ,(prefixed "rgb01:"))
	;; it can also handle other types:
	(def. ,(prefixed "rgb8.") ,(prefixed "rgb01:"))))))


(def (rgb01:op/2 op)
     (lambda (a b)
       (apply rgb01 (map (lambda_ (op (_ a) (_ b)))
			 (list .r01 .g01 .b01)))))

(def-rgb01 + (rgb01:op/2 +))
(def-rgb01 - (rgb01:op/2 -))
(def-rgb01 mean (rgb01:op/2 mean))

(def (rgb01:.op op)
     (typed-lambda (a #(number? b))
	      (insert-result-of
	       `(rgb01l ,@(map (lambda_
				`(op (,_ a) b))
			       '(.r01l .g01l .b01l))))))

(def-rgb01 .* (rgb01:.op *))
(def-rgb01 ./ (rgb01:.op /))

(TEST
 > (..* (rgb8 100 50 0) 2)
 #(rgb01 40/51 20/51 0)
 > (%try-error (..* (rgb8 100 200 0) 2))
 #(error "does not match 0..1?:" 80/51))

(def (iter-stream f start)
     (let rec ((x start))
       (delay (cons x
		    (rec (f x))))))

(TEST
 > (F (stream-take (iter-stream (C ..* _ 0.9) (rgb01 1 1 0.5)) 3))
 (#(rgb01 1 1 .5) #(rgb01 .9 .9 .45) #(rgb01 .81 .81 .405)))


;; print as hex

(def (rgb.string x)
     (def (conv #(uint8? x))
	  (number->uc-hex-string/padding x 2))
     (insert-result-of
      `(string-append "#"
		      ,@(map (lambda_
			      `(conv (,_ x)))
			     '(.r8 .g8 .b8)))))

(def. rgb8.string rgb.string)
(def. rgb01.string rgb.string)

(TEST
 > (.string (rgb8 0 128 255))
 "#0080FF"
 > (.string (.rgb01 (rgb8 0 128 255)))
 "#0080FF"
 > (.string (rgb01 1 0.5 0))
 "#FF8000"
 )

