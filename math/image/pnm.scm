;;; Copyright 2013-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 (u8-parse mk-read-all read-u8-integer read-u8-u8line read-u8-word))

(include "../../cj-standarddeclares.scm")

;;XX move to cj-alist or so?

(define (string-alist->fn whatmsg alis)
  (lambda (v)
    (or (string-alist-maybe-ref alis v)
	(error (string-append "unknown " whatmsg ":") v))))

;;/move


(define-struct. pnmformat
  #(string? magic)
  #(symbol? type)
  #(symbol? encoding)
  #(string? suffix)
  #(natural? channels)
  #(boolean? explicit-maxval?)
  #((maybe natural?) bpc))

(define. (pnmformat.raw? v)
  (xcase (.encoding v)
	 ((raw) #t)
	 ((ascii) #f)))

(define. (pnmformat.expected-maxval v #!optional (error error))
  (cond ((.bpc v)=> (compose dec (cut expt 2 <>)))
	(else (error "can't give expected maxval, bpc unknown"))))
(define. (pnmformat.maybe-expected-maxval v)
  (pnmformat.expected-maxval v false/1))

(define pnm:magic.format
  (string-alist->fn
   "pnm magic"
   (map (lambda_
	 (cons (car _) (apply pnmformat _)))
	'( ;; magic, type, encoding, suffix,
	  ;;     channels, explicit-maxval?, bpc(bitsperchannel),
	  ("P1" bit ascii "pbm" 1 #f 1)
	  ("P2" gray ascii "pgm" 1 #t #f) ;; 8 or 16 bpc, right?
	  ("P3" pix ascii "ppm" 3 #t #f)  ;; dito
	  ("P4" bit raw "pbm" 1 #f 1)
	  ("P5" gray raw "pgm" 1 #t #f) ;; right?
	  ("P6" pix raw "ppm" 3 #t #f) ;; dito
	  ;; and own inventions:
	  ;; A= alpha channel
	  ;; H= high quality (16 bit/channel)
	  ("PRGBA" pixt raw "prgba" 4 #t #f)
	  ("PCMYK" cmyk raw "pcmyk" 4 #t #f)
	  ;; floats:
	  ("PF32G" gray raw "pf32g" 1 #f 32)
	  ("PF32RGB" pix raw "pf32rgb" 3 #f 32)
	  ("PF64G" gray raw "pf64g" 1 #f 64)
	  ("PF64RGB" pix raw "pf64rgb" 3 #f 64)))))
;; more generally then just 3 dimensional matrices; (or n-dim); add complex...
;; ('add data layouts')



(define-struct. pnminfo
  name ;; optional path or whatever?
  #(pnmformat? format)
  #((maybe u8vector?) maybe-comment)
  #(natural? width)
  #(natural? height)
  #((maybe natural?) maybe-maxval))

(define. (pnminfo.maxval v)
  (or (.maybe-maxval v)
      (.expected-maxval (.format v))))


(define (Mr:read-pnm-body p info)
  (let* ((read-lum (if (.raw? (.format info))
		       ##read-u8
		       read-u8-integer)))
    ((lambda (width height maxval)
       (let* ((range (inc maxval))
	      (m (@make-Mr height width))
	      (descale (/ 1. (exact->inexact range))))
	 (for..<
	  (row 0 height)
	  (for..< (col 0 width)
		  (let ((lum (read-lum p)))
		    ;; (stupid, could (should?) use Mi, again? or: )
		    ;; what number should this create, [0..1)?
		    (Mr.set!@ m row col (* (exact->inexact lum) descale)))))
	 (let ((next (read-u8 p)))
	   (assert (eof-object? next)))
	 m))
     (.width info)
     (.height info)
     (.maxval info))))



(define (read-pnminfo p)
  (let* ((format (pnm:magic.format (read-u8-word p)))
	 (cont (lambda (maybe-comment width height)
		 (let ((cont (lambda (maybe-maxval)
			       (pnminfo (##port-name p) ;;ok?
					format
					maybe-comment
					width
					height
					maybe-maxval))))
		   (if (.explicit-maxval? format)
		       (let ((maxval (read-u8-integer p)))
			 (cond ((.maybe-expected-maxval format)
				=> (lambda (expected)
				     (assert (= maxval expected)))))
			 (cont maxval))
		       (cont #f)))))
	 (l (read-u8-u8line p)))
    (if (= (u8vector-ref l 0) (char->integer (string-ref "#" 0)))
	;;(^ will error on empty lines)
	(let* ((width (read-u8-integer p))
	       (height (read-u8-integer p)))
	  (cont l width height))
	(let ((vs (call-with-input-u8vector
		   l
		   (mk-read-all read-u8-integer))))
	  (assert (= (length vs) 2))
	  ;; could be more lenient, really, though. But that will do
	  ;; it for both gimp and my own format?
	  (cont #f (car vs) (cadr vs))))))


;; (define (Mr:read-pnm p)
;;   (Mr:read-pnm-body p (read-pnminfo p)))

;; (define (Mr:read-pnm-file path)
;;   (call-with-input-file path Mr:read-pnm))

(define (_:read-pnm-file _:read-pnm-body)
  (lambda (path)
    (call-with-input-file path
      (lambda (p)
	(_:read-pnm-body p (read-pnminfo p))))))

(define Mr:read-pnm-file (_:read-pnm-file Mr:read-pnm-body))

