;;; Copyright 2013-2016 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require test
	 easy
	 unixtime-types
	 (predicates in-signed-range?))



(c-declare "
       #include <time.h>
       #include <stdlib.h>
")

(def sizeof-time_t (##c-code "___RESULT= ___FIX(sizeof(time_t));"))

(assert (<= sizeof-time_t 8))
;; ah wow we are on 64 bits already?!

(def bitsof-time_t (* sizeof-time_t 8))


(def (time_t? v)
  (and (number? v)
       (exact? v)
       (in-signed-range? bitsof-time_t v)))


;; XX is this alias a *bad* idea?
(def unixtime? time_t?)

(def max-ctime-bytes 26) ;; according to man page


;; http://pic.dhe.ibm.com/infocenter/aix/v6r1/topic/com.ibm.aix.basetechref/doc/basetrf1/ctime.htm
;; The ctime subroutine adjusts for the time zone and daylight saving
;; time, if it is in effect.

(def (ctime #(time_t? t))
     (let ((in (s64vector t))
	   ;; give it a little extra safety margin:
	   (out (##make-u8vector (+ max-ctime-bytes 30))))
       (##c-code "{
    long long *in = ___CAST(long long*, ___BODY(___ARG1));
    time_t t= *in;
    char *out = ___CAST(char*, ___BODY(___ARG2));
    ctime_r(&t, out);
}" in out)
       ;; XXX not supporting unicode here. Any locales that need it?
       (let* ((len (dec ;; ignore the trailing newline
		    (let lp ((i 0))
		      (if (zero? (u8vector-ref out i))
			  i
			  (lp (inc i))))))
	      (res (##make-string len)))
	 (for..< (i 0 len)
		 (string-set! res i
			      (integer->char (u8vector-ref out i))))
	 res)))

;; http://pic.dhe.ibm.com/infocenter/aix/v6r1/topic/com.ibm.aix.basetechref/doc/basetrf1/ctime.htm
;; The gmtime subroutine converts the long integer pointed to by the
;; Clock parameter into a tm structure containing the Coordinated
;; Universal Time (UTC), which is the time standard the operating
;; system uses.

;; I'm returning a vector with an additional field containing the
;; 'extern long timezone' value, "seconds West of UTC" (man tzset),
;; actually 0 for gmtime.

(def. (unixtime.gmtime #(time_t? t))
  (let ((in (s64vector t))
	;; XX still output localtime objects? Since they contain
	;; the time zone, all should be ok?
	(out (make-vector 11 'localtime)))
    (##c-code "{
    long long *in = ___CAST(long long*, ___BODY(___ARG1));
    time_t t= *in;
    ___SCMOBJ *out = ___BODY(___ARG2);
    struct tm res;
    gmtime_r(&t,&res);
#define LTSET(i,e) out[i+1]= ___FIX(e)
    LTSET(0,res.tm_sec);
    LTSET(1,res.tm_min);
    LTSET(2,res.tm_hour);
    LTSET(3,res.tm_mday);
    LTSET(4,res.tm_mon);
    LTSET(5,res.tm_year);
    LTSET(6,res.tm_wday);
    LTSET(7,res.tm_yday);
    LTSET(8,res.tm_isdst); /* always 0 for gmtime, because it doesn't deal
                             with tz data, unlike localtime. ? */
    LTSET(9,0); /* ok? */
#undef LTSET
}" in out)
    out))

;; http://pic.dhe.ibm.com/infocenter/aix/v6r1/topic/com.ibm.aix.basetechref/doc/basetrf1/ctime.htm
;; The localtime subroutine converts the long integer pointed to by
;; the Clock parameter, which contains the time in seconds since
;; 00:00:00 UTC, 1 January 1970, into a tm structure. The localtime
;; subroutine adjusts for the time zone and for daylight-saving time,
;; if it is in effect. Use the time-zone information as though
;; localtime called tzset.-- what was that last sentence? SIGH?

;; I'm returning a vector with an additional field containing the
;; 'extern long timezone' value, "seconds West of UTC" (man tzset).

(def. (unixtime.localtime #(time_t? t))
  (let ((in (s64vector t))
	(out (make-vector 11 'localtime)))
    (##c-code "
{
    long long *in = ___CAST(long long*, ___BODY(___ARG1));
    time_t t= *in;
    ___SCMOBJ *out = ___BODY(___ARG2);
    struct tm res;
    localtime_r(&t,&res);
#define LTSET(i,e) out[i+1]= ___FIX(e)
    LTSET(0, res.tm_sec);
    LTSET(1, res.tm_min);
    LTSET(2, res.tm_hour);
    LTSET(3, res.tm_mday);
    LTSET(4, res.tm_mon);
    LTSET(5, res.tm_year);
    LTSET(6, res.tm_wday);
    LTSET(7, res.tm_yday);
    LTSET(8, res.tm_isdst);
    LTSET(9, timezone);
#undef LTSET
}
" in out)
    out))


(def (string->u8vector/0 str)
  (let* ((len (string-length str))
	 (res (##make-u8vector (inc len))))
    (for..< (i 0 len)
	    (u8vector-set! res i
			   (char->integer (string-ref str i))))
    (u8vector-set! res len 0)
    res))

(def (setenv! key val) ;; does |setenv| do the same really?
  (##c-code "___RESULT=
       ___FIX(setenv( ___CAST(char*,___BODY(___ARG1)),
                      ___CAST(char*,___BODY(___ARG2)), 1));"
	    (string->u8vector/0 key)
	    (string->u8vector/0 val)))

(def (tzset)
  (##c-code "tzset();")
  (void))

(def (set-TZ! str)
  (setenv! "TZ" str)
  ;; *and*, essential!:
  (tzset))

;; so, after all, in the end, it *is* the 'extern long timezone' value
;; that's relevant for those calls? ? Just that *on their first call*,
;; they call tzset themselves. What a * mess. *And* badly documented.
;; XX: so, could (*perhaps*) just set the timezone value there,
;; temporarily, and pass around as explicit argument in Scheme. Iff I
;; find out how to prevent those calls from calling tzset themselves
;; reliably.


