;;; Copyright 2016-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 test
	 (cj-functional values-of)
	 (cj-source-util-2 assert)
	 (oo-util values.vector))

;; https://en.wikipedia.org/wiki/UTF-32
;; UTF-32 (or UCS-4) stands for Unicode Transformation Format 32
;; bits. It is a protocol to encode Unicode code points that uses
;; exactly 32 bits per Unicode code point.

;; https://en.wikipedia.org/wiki/Null_character
;; It is present in many character sets, including .. the Universal
;; Character Set (or Unicode)


;; These are defined in Gambit's c_intf.c, but in no header file, so:
(c-declare "
#ifdef __cplusplus
extern \"C\" {

___UCS_4 _Z12___UTF_8_getPPc
   ___P((___UTF_8STRING *ptr),
        (ptr)
___UTF_8STRING *ptr;);

int _Z14___UTF_8_bytesj
   ___P((___UCS_4 c),
        (c)
___UCS_4 c;);

void _Z12___UTF_8_putPPcj
   ___P((___UTF_8STRING *ptr,
         ___UCS_4 c),
        (ptr,
         c)
___UTF_8STRING *ptr;
___UCS_4 c;);

}

static
 ___UCS_4 ___UTF_8_get
   ___P((___UTF_8STRING *ptr),
        (ptr)
___UTF_8STRING *ptr;) {
    return _Z12___UTF_8_getPPc(ptr);
}

static
int ___UTF_8_bytes
   ___P((___UCS_4 c),
        (c)
___UCS_4 c;) {
    return _Z14___UTF_8_bytesj(c);
}

static
void ___UTF_8_put
   ___P((___UTF_8STRING *ptr,
         ___UCS_4 c),
        (ptr,
         c)
___UTF_8STRING *ptr;
___UCS_4 c;) {
    return _Z12___UTF_8_putPPcj(ptr,c);
}
#else

 ___UCS_4 ___UTF_8_get
   ___P((___UTF_8STRING *ptr),
        (ptr)
___UTF_8STRING *ptr;);

int ___UTF_8_bytes
   ___P((___UCS_4 c),
        (c)
___UCS_4 c;);

void ___UTF_8_put
   ___P((___UTF_8STRING *ptr,
         ___UCS_4 c),
        (ptr,
         c)
___UTF_8STRING *ptr;
___UCS_4 c;);

#endif
")


(def (ucs4-codepoint? v)
     (and (exact-natural0? v)
	  ;; be careful about the null value!  valid in UCS-4 but not
	  ;; necessarily in strings.
	  (<= v #xFFFFFFFF)))

(TEST
 > (ucs4-codepoint? #\A)
 #f
 > (ucs4-codepoint? 65)
 #t
 > (ucs4-codepoint? -1)
 #f
 > (ucs4-codepoint? 0)
 #t
 > (ucs4-codepoint? 0.)
 #f
 > (ucs4-codepoint? (dec (expt 2 32)))
 #t
 > (ucs4-codepoint? (expt 2 32))
 #f)


(def (@utf8-bytes c)
     (##c-code "
___RESULT=___FIX(___UTF_8_bytes(___INT(___ARG1)));"
	       c))

(def (utf8-bytes #(ucs4-codepoint? c))
     (@utf8-bytes c))

(TEST
 > (utf8-bytes 0)
 1
 > (utf8-bytes 100)
 1
 > (utf8-bytes (.integer #\ä))
 2
 > (utf8-bytes 3233)
 3
 > (utf8-bytes #xFFFF)
 3
 > (utf8-bytes #xFFFFFF)
 5
 ;; looks like the validity check is not compiled in:
 > (utf8-bytes #xFFFFFFFF)
 6)


(def (@u8vector-utf8-put! u8vec i c) ;; returns new i
     (##c-code "
size_t i= ___INT(___ARG2);
___UTF_8STRING base= ___CAST(___UTF_8STRING , ___BODY(___ARG1));
___UTF_8STRING p= &(base[i]);
___UTF_8_put(&p, ___INT(___ARG3));
___RESULT= ___FIX(p-base);"
	       u8vec i c))

(def. (u8vector.utf8-put! #(u8vector? v) ;; XX stupid oo
			  #(natural0? i)
			  #(ucs4-codepoint? c))
  -> natural0? ;; new i

  (@u8vector-utf8-put!
   v
   (-> (C <= _ (- (u8vector-length v) (utf8-bytes c)))
       i)
   c))

(TEST
 > (def v (make-u8vector 10 77))
 > (u8vector.utf8-put! v 1 (.integer #\ä))
 3
 > v
 #u8(77 195 164 77 77 77 77 77 77 77)
 > (u8vector.utf8-put! v 3 (.integer #\ö))
 5
 > v
 #u8(77 195 164 195 182 77 77 77 77 77)
 > (u8vector.utf8-put! v 5 (.integer #\A))
 6
 > v
 #u8(77 195 164 195 182 65 77 77 77 77))


(def (@u8vector-utf8-get! u8vec i+res) ;; i+res = (u32vector i* codepoint)
     (##c-code "
___UCS_4 *i_res= ___CAST(___UCS_4*, ___BODY(___ARG2));
___UTF_8STRING base= ___CAST(___UTF_8STRING, ___BODY(___ARG1));
___UTF_8STRING p= &(base[i_res[0]]);

___UCS_4 c= ___UTF_8_get(&p);

i_res[0]= p-base;
i_res[1]= c;

" u8vec i+res))

(def sizeof-ucs4
     (-> (C = _ 4) ;; i.e. same as U32
	 (##c-code "___RESULT= ___FIX(sizeof(___UCS_4));")))

(def. (u8vector.utf8-get #(u8vector? v)
			 #(natural0? i))
  -> (values-of (maybe char?)
		;; XX size_t? index?
		natural0?)
  
  (let ((i+res (u32vector i 0)))
    (@u8vector-utf8-get! v i+res)
    (let ((i* (u32vector-ref i+res 0))
	  (c (u32vector-ref i+res 1)))
      (if (= i* i)
	  (values #f i)
	  (begin
	    (assert (<= i* (u8vector-length v)))
	    ;; ^ XX should this take into consideration zero end byte
	    ;; in the case of u8vector0? But need real typing
	    ;; then. Alternatively check in functions like
	    ;; u8vector0.utf8-parse .
	    (values (integer->char (-> ucs4-codepoint? c))
		    i*))))))

(TEST
 > (def v '#u8(195 164 195 182 195 188 0))
 > (values.vector (u8vector.utf8-get v 0))
 #(#\ä 2)
 > (values.vector (u8vector.utf8-get v 2))
 #(#\ö 4)
 > (values.vector (u8vector.utf8-get v 4))
 #(#\ü 6)

 > (def v '#u8(72 101 108 108 195 182 108 0)) ;; "Hellöl"
 > (values.vector (u8vector.utf8-get v 4))
 #(#\ö 6)
 > (values.vector (u8vector.utf8-get v 6))
 #(#\l 7)
 > (values.vector (u8vector.utf8-get v 7))
 #(#\nul 8)
 > (%try-error (u8vector.utf8-get v 8))
 #(error
   "assertment failure: (<= i* (u8vector-length v))"
   (<= 9 (u8vector-length '#u8(72 101 108 108 195 182 108 0))))
 )

