;;; Copyright 2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 test)

;; (c/load "tinymt" cc-options: "-std=c99")

;; Dependency:
;; mkdir src; cd src; git clone https://github.com/MersenneTwister-Lab/TinyMT.git
;; Currently using v1.1.1 (53206ad2f9d09b7696e5b2fcc4ace6650731910c)


;; XX lib
;; huh so many things. autogen, please

(def (make-f32box)
     (##make-f32vector 1))

(def (f32box v)
     (##f32vector v))

(def (f32box? v)
     (and (f32vector? v)
	  (= (f32vector-length v) 1)))

(def (@f32unbox v)
     (##f32vector-ref v 0))

(def (f32unbox #(f32box? v))
     (@f32unbox v))

;;/ XX


(c-declare "
#include \"TinyMT/tinymt/tinymt32.c\"
")

(def size-of-tinymt32_t (##c-code "___RESULT= ___FIX(sizeof(tinymt32_t));"))


(def (tinymt32? v)
     (and (u8vector? v)
	  (= (u8vector-length v) size-of-tinymt32_t)))

(def (make-tinymt32 #(uint32? seed))
     (let ((r (make-u8vector size-of-tinymt32_t))
	   (seed* (s32vector seed)))
       (##c-code "
    tinymt32_t *tinymt = ___CAST(tinymt32_t*, ___BODY(___ARG1));
    uint32_t *seed_array = ___CAST(uint32_t*, ___BODY(___ARG2)); // better way?

// XXX are these values from check.c actually safe to use?
    tinymt->mat1 = 0x8f7011ee;
    tinymt->mat2 = 0xfc78ff1f;
    tinymt->tmat = 0x3793fdff;

    tinymt32_init_by_array(tinymt, seed_array, 1);
" r seed*)
       r))

(def (@tinymt32-generate-float! ctx b)
     (##c-code "
    tinymt32_t *tinymt = ___CAST(tinymt32_t*, ___BODY(___ARG1));
    float *b = ___CAST(float*, ___BODY(___ARG2));

    *b= tinymt32_generate_float(tinymt);
" ctx b))


(def (tinymt32-generate-float! #(tinymt32? ctx) #(f32box? b))
     (@tinymt32-generate-float! ctx b))


(def (tinymt32-generate-float #(tinymt32? ctx))
     (let ((b (make-f32box)))
       (@tinymt32-generate-float! ctx b)
       (@f32unbox b)))


(TEST
 > (def t (make-tinymt32 1))
 > (tinymt32-generate-float t)
 .013245880603790283 ;; 0.0132459
 > (tinymt32-generate-float t)
 .20838993787765503 ;; 0.2083899
 > (tinymt32-generate-float t)
 .14579975605010986 ;; 0.1457998
 )

