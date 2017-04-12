;;; Copyright 2013-2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 math/image/pnm ;; pnminfo ..
	 ;;XX ? math/image/pnmatrix
	 math/image/pnmatrix-macros ;; pnmatrix:headsize pnmatrixtypes ..
	 )

(declare (block)
	 (standard-bindings)
	 (extended-bindings))


(define. (pnminfo.bpc info)
  (cond ((.bpc (.format info))=>
	 (lambda (required-bpc)
	   (cond ((.maxval info)=>
		  (lambda (maxval)
		    (assert (= (dec (expt 2 required-bpc)) maxval))
		    required-bpc))
		 (else
		  required-bpc))))
	(else
	 (cond ((.maxval info)=>
		(lambda (maxval)
		  (xcase maxval
			 ((255) 8)
			 ((65535) 16))))
	       (else
		(error "neither format's required-bpc nor infos's maxval present"))))))

(define. (pnminfo.filesize info)
  (let* ((format (.format info))
	 (numbits/row (* (.bpc info) (.width info) (.channels format)))
	 ;; bpm seems to require alignment of rows on byte boundaries?
	 (numbytes/row (letv ((r rest) (quotient+modulo numbits/row 8))
			     (if (zero? rest)
				 r
				 (inc r))))
	 (numbytes (* numbytes/row (.height info)))
	 (_ (assert (integer? numbytes)))
	 (size (+ numbytes pnmatrix:headsize)))
    size))

(define. (pnminfo.pnmatrixtype info)
  (let* ((bpc (.bpc info))
	 (format (.format info))
	 (formatmagic (.magic format))
	 (matches (filter (mcase-lambda
			   (`(`pnmatrixtype `magic `maybe-bpc)
			    (and (string=? formatmagic magic)
				 (or (not maybe-bpc)
				     (= bpc maybe-bpc)))))
			  pnmatrixtypes))
	 (match (xone matches)))
    (car match)))


(define-macro (define-pnmatrixbasetype bpc)
  (let ((t (symbol-append (if (< bpc 32)
			      'u
			      'f)
			  (.string bpc))))
    `(begin
       (define (,(symbol-append t '?) v)
	 (and (##fixnum? v)
	      (fx>= v 0)
	      (fx< v ,(expt 2 bpc))))
       (define-struct. ,(symbol-append t 'matrix)
	 #(symbol? pnmatrixtype) ;; (.pnmatrixtype info), but faster, used by highlevel predicates
	 #(pnminfo? info)
	 #(size0? size0)
	 #(size0? size1)
	 #(size0? channels)
	 #(fixnum? addr)))))

;; unsigned integer
(define-pnmatrixbasetype 8)
(define-pnmatrixbasetype 16)
;; float
(define-pnmatrixbasetype 32)
(define-pnmatrixbasetype 64)




;; remove?
(define. u8matrix.ref
  (typed-lambda
   (#(u8matrix? m) #(size0? i0) #(size0? i1) #(size0? channel))
   (let* ((channels (@u8matrix.channels m))
	  (s1 (@u8matrix.size1 m))
	  (rowlen (fx* s1 channels)))
     (assert (fx< i0 (@u8matrix.size0 m)))
     (assert (fx< i1 s1))
     (assert (fx< channel channels))
     (let ((i (fx+ pnmatrix:headsize (fx* i0 rowlen) (fx* i1 channels) channel)))
       (##c-code "
___U8* p= ___CAST(void*,___ARG1);
size_t i= ___INT(___ARG2);
___RESULT= ___FIX(p[i]);
" (@u8matrix.addr m) i)))))


;; XX hmm, does that make sense? Only works for PNMs with 3 (or,
;; worse, more!, hmm) channels.
(define. (u8matrix.ref3 m i0 i1 #!optional (values values))
  (values (u8matrix.ref m i0 i1 0)
	  (u8matrix.ref m i0 i1 1)
	  (u8matrix.ref m i0 i1 2)))


(define. u8matrix.cset!
  (typed-lambda
   (#(u8matrix? m) #(size0? i0) #(size0? i1) #(size0? channel) #(size0? v))
   (let* ((channels (@u8matrix.channels m))
	  (s1 (@u8matrix.size1 m))
	  (rowlen (fx* s1 channels)))
     (assert (fx< i0 (@u8matrix.size0 m)))
     (assert (fx< i1 s1))
     (assert (fx< channel channels))
     ;;(assert (fx<= v (.maxval (@u8matrix.info m)))) ;; ugh.
     (assert (fx< v 256))
     (let ((i (fx+ pnmatrix:headsize (fx* i0 rowlen) (fx* i1 channels) channel)))
       (##c-code "
___U8* p= ___CAST(void*,___ARG1);
size_t i= ___INT(___ARG2);
___U8 v= ___INT(___ARG3);
p[i]= v;
" (@u8matrix.addr m) i v)
       (void)))))

