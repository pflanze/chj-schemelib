;;; Copyright 2018-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star)

(export (macro @vector-ref)
	(macro @vector-set!)
	(macro @vector-length)
        cj-gambit-sys:vector-like?)


(include "cj-standarddeclares.scm")


(define-macro* (@vector-ref v i)
  ;; need unsafe mode for ##vector-ref to be compiled efficiently!
  (let ((V (gensym 'V))
	(I (gensym 'I)))
    `(##let ((,V ,v)
	     (,I ,i))
	    (##declare (not safe))
	    (##vector-ref ,V ,I))))

(define-macro* (@vector-set! v i val)
  ;; need unsafe mode here, too?
  (let ((V (gensym 'V))
	(I (gensym 'I))
	(VAL (gensym 'VAL)))
    `(##let ((,V ,v)
	     (,I ,i)
	     (,VAL ,val))
	    (##declare (not safe))
	    (##vector-set! ,V ,I ,VAL))))

(define-macro* (@vector-length v)
  ;; need unsafe mode here, too?
  (let ((V (gensym 'V)))
    `(##let ((,V ,v))
	    (##declare (not safe))
	    (##vector-length ,V))))



;; Things that can sensibly be permutated (i.e. where each position
;; has the same type--which is not true for values, structures, .. at
;; least not dynamically)

;; (Also see |mem-bytes-like?|)

(define (cj-gambit-sys:vector-like? obj)
  (##c-code "
___RESULT=___FAL;
if (___MEM_ALLOCATED (___ARG1) && !___PAIRP(___ARG1)) { /* really do have to check against pair */
    switch (___HD_SUBTYPE(___HEADER(___ARG1))) {
	case ___sVECTOR:
	//case ___sSTRUCTURE:
	//case ___sBOXVALUES:
	//case ___sMEROON:
	case ___sSTRING:
	case ___sS8VECTOR:
	case ___sU8VECTOR:
	case ___sS16VECTOR:
	case ___sU16VECTOR:
	case ___sS32VECTOR:
	case ___sU32VECTOR:
	case ___sF32VECTOR:
	case ___sS64VECTOR:
	case ___sU64VECTOR:
	case ___sF64VECTOR:
	//case ___sFLONUM:
		___RESULT= ___TRU;
    }
}
" obj))

;; Tests see cj-gambit-sys.scm


