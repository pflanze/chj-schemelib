;;; Copyright 2013-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 unixtime-Cpart-compiletime
	 unixtime-types
	 (predicates %in-signed-range?)
	 u8vector0
	 test)

(export time_t? integer-unixtime? unixtime?
	real-unixtime?
	ctime
	(method unixtime.gmtime)
	(method unixtime.localtime)
	setenv! ;; XX move elsewhere?
	tzset
	set-TZ!
	mktime (method localtime.unixtime))

(include "cj-standarddeclares.scm")


(c-declare "
#ifndef _GNU_SOURCE
       #define _GNU_SOURCE
#endif
#ifndef _XOPEN_SOURCE
       #define _XOPEN_SOURCE
#endif
       #include <time.h>
       #include <stdlib.h>
")


(def (time_t? v)
  (and (number? v)
       (exact? v)
       (integer? v)
       (%in-signed-range? bitsof-time_t v)))


(def integer-unixtime? time_t?)
;; unixtime? might also be handled as a float or rational, but not by
;; default
(def unixtime? time_t?)

(def (real-unixtime? v)
  (and (number? v)
       (%in-signed-range? bitsof-time_t v)))


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
	(out (localtime 1 2 3 4 5 6 7 8 9 10)))
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
    LTSET(9,0); /* timezone, ok? */
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
	(out (localtime 1 2 3 4 5 6 7 8 9 10)))
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



;; mktime:

;; According to man mktime:

;; * ignores the values supplied by the caller in the tm_wday and
;;   tm_yday fields

;; * The value specified in the tm_isdst field informs mktime()
;;   whether or not day‐ light saving time (DST) is in effect for the
;;   time supplied in the tm structure: a positive value means DST is
;;   in effect; zero means that DST is not in effect; and a negative
;;   value means that mktime() should (use time‐ zone information and
;;   system databases to) attempt to determine whether DST is in
;;   effect at the specified time. -- You can pass #f to get the same
;;   effect as -1.

;; Call |set-TZ!| beforehand! The timezone field is being *ignored*!

(def (mktime #(localtime? l)) -> time_t?
     (let ((res (s64vector 0)))
       (##c-code "
{
    ___SCMOBJ l= ___ARG1;
    long long *res= ___CAST(long long*, ___BODY(___ARG2));

    ___SCMOBJ tmp;
    struct tm in;
#define LTSET(i, target) tmp= ___VECTORREF(l, ___FIX(i+1)); target= (___FIXNUMP(tmp) ? ___INT(tmp) : -1);
    LTSET(0, in.tm_sec);
    LTSET(1, in.tm_min);
    LTSET(2, in.tm_hour);
    LTSET(3, in.tm_mday);
    LTSET(4, in.tm_mon);
    LTSET(5, in.tm_year);
    LTSET(6, in.tm_wday);
    LTSET(7, in.tm_yday);
    LTSET(8, in.tm_isdst);
    // LTSET(9, timezone); ignored. Instead call |set-TZ!| beforehand!
#undef LTSET
    *res= mktime (&in);
}
"
		 l
		 res)
       (s64vector-ref res 0)))


(def. localtime.unixtime mktime)

;; strptime see unixtime-Cpart-strptime, but it doesn't really work.


(def (setenv! key val) ;; does |setenv| do the same really?
  (##c-code "___RESULT=
       ___FIX(setenv( ___CAST(char*,___BODY(___ARG1)),
                      ___CAST(char*,___BODY(___ARG2)), 1));"
	    (string.utf8-u8vector0 key)
	    (string.utf8-u8vector0 val)))

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


