;;; Copyright 2016-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 (u8vector0 string.utf8-u8vector)
	 skein-code)

(export make-skein512-digest
	skein512-digest?
	skein-hash-bytes
	skein:raw-digest
	skein:digest)




(insert-result-of
 `(c-declare ,skein-code))


(##c-code "skein_init();")

(define-constant-from-C skein:BYTES)
 ;; don't run each time though please^^ ehr   prefix please  no conflict please
(assert (= skein:BYTES (/ 512 8)))

(def (skein512-digest? v)
     (and (u8vector? v)
	  (= (u8vector-length v) skein:BYTES)))

(def (skein-hash-bytes #(u8vector? in)
		       #((maybe natural0?) len)
		       #(skein512-digest? out))
     (let* ((size (u8vector-length in))
	    (len (if len
		     (assert (<= len size))
		     size)))
       (assert (fixnum? len)) ;; stupid but hell so it is now.
       (##c-code "
const byte* msg= ___CAST(const byte*, ___BODY(___ARG1));
int len= ___INT(___ARG2);
char* digest= ___CAST(char*, ___BODY(___ARG3));
struct Skein512digest out;

skein_hash_bytes(msg, len, &out);

memcpy(digest, out.bytes, BYTES);

___RESULT=___VOID;
"
		 in
		 len
		 out)))

(def (make-skein512-digest)
     (make-u8vector skein:BYTES))

(def (skein:raw-digest v)
     (def (hash u)
	  (let ((d (make-skein512-digest)))
	    (skein-hash-bytes u #f d)
	    d))

     (cond ((string? v)
	    (hash (string.utf8-u8vector v)))
	   ((u8vector? v)
	    (hash v))
	   (else
	    ;; well should use OO you know
	    (error "skein:digest: can only hash strings or u8vectors"))))

(def skein:digest (comp-function u8vector->hex-string-lc skein:raw-digest))

