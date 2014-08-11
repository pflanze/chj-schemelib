;;; Copyright 2013-2014 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require test
	 easy)

(c-declare "
       #include <time.h>
       #include <stdlib.h>
")

(def sizeof-time_t (##c-code "___RESULT= ___FIX(sizeof(time_t));"))

(assert (<= sizeof-time_t 8))
;; ah wow we are on 64 bits already?!

(def bitsof-time_t (* sizeof-time_t 8))

(def (in-signed-range? wordsize-bits v)
  (let ((half (expt 2 (dec wordsize-bits))))
    (and (<= (- half) v)
	 (< v half))))

(TEST
 > (def (test basenum v)
     (list (in-signed-range? 8 v)
	   (number->string (+ basenum v) 2)))
 > (test (expt 2 16) -1)
 (#t "1111111111111111")
 ;;   1234567812345678
 > (test (expt 2 16) -126)
 (#t "1111111110000010")
 > (test (expt 2 16) -127)
 (#t "1111111110000001")
 > (test (expt 2 16) -128)
 (#t "1111111110000000")
 > (test (expt 2 16) -129)
 (#f "1111111101111111")
 > (test (expt 2 15) 0)
 (#t "1000000000000000")
 > (test (expt 2 15) 127)
 (#t "1000000001111111")
 > (test (expt 2 15) 128)
 (#f "1000000010000000")
 ;;   1234567812345678
 )

(def (time_t? v)
  (and (number? v)
       (exact? v)
       (in-signed-range? bitsof-time_t v)))

(def max-ctime-bytes 26) ;; according to man page


;; http://pic.dhe.ibm.com/infocenter/aix/v6r1/topic/com.ibm.aix.basetechref/doc/basetrf1/ctime.htm
;; The ctime subroutine adjusts for the time zone and daylight saving
;; time, if it is in effect.

(def (ctime #(time_t? t))
     (let ((in (##make-s64vector 1))
	   ;; give it a little extra safety margin:
	   (out (##make-u8vector (+ max-ctime-bytes 30))))
       (s64vector-set! in 0 t)
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

(def (gmtime #(time_t? t))
     (let ((in (##make-s64vector 1))
	   (out (##make-s32vector 10)))
       (s64vector-set! in 0 t)
       (##c-code "{
    long long *in = ___CAST(long long*, ___BODY(___ARG1));
    time_t t= *in;
    int *out = ___CAST(int*, ___BODY(___ARG2));
    struct tm res;
    gmtime_r(&t,&res);
    out[0]= res.tm_sec;
    out[1]= res.tm_min;
    out[2]= res.tm_hour;
    out[3]= res.tm_mday;
    out[4]= res.tm_mon;
    out[5]= res.tm_year;
    out[6]= res.tm_wday;
    out[7]= res.tm_yday;
    out[8]= res.tm_isdst; /* always 0 for gmtime, because it doesn't deal
                             with tz data, unlike localtime. ? */
    out[9]= 0; /* ok? */
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

(def (localtime #(time_t? t))
     (let ((in (##make-s64vector 1))
	   (out (##make-s32vector 10)))
       (s64vector-set! in 0 t)
       (##c-code "{
    long long *in = ___CAST(long long*, ___BODY(___ARG1));
    time_t t= *in;
    int *out = ___CAST(int*, ___BODY(___ARG2));
    struct tm res;
    localtime_r(&t,&res);
    out[0]= res.tm_sec;
    out[1]= res.tm_min;
    out[2]= res.tm_hour;
    out[3]= res.tm_mday;
    out[4]= res.tm_mon;
    out[5]= res.tm_year;
    out[6]= res.tm_wday;
    out[7]= res.tm_yday;
    out[8]= res.tm_isdst;
    out[9]= timezone;
}" in out)
       out))


;; date -R format

(def rfc-2822:days
  '#("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(def rfc-2822:months
  '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

;; seconds
(def (tm.tzoffset tm) 
  (- (* (s32vector-ref tm 8) 3600)
     (s32vector-ref tm 9)))

(def (tm->rfc-2822 tm)
  (let* ((tzoffset  (tm.tzoffset tm))
	 ;; ^ is this really correct?
	 (tzoffsetm (abs tzoffset))
	 (tzoffset-hours (quotient tzoffsetm 3600))
	 (tzoffset-minutes (quotient (modulo tzoffsetm 3600) 60)))
    
    (string-append (vector-ref rfc-2822:days (s32vector-ref tm 6))
		   ", "
		   (number->string (s32vector-ref tm 3))
		   " "
		   (vector-ref rfc-2822:months (s32vector-ref tm 4))
		   " "
		   (number->string (+ 1900 (s32vector-ref tm 5)))
		   " "
		   (number->padded-string 2 (s32vector-ref tm 2))
		   ":"
		   (number->padded-string 2 (s32vector-ref tm 1))
		   ":"
		   (number->padded-string 2 (s32vector-ref tm 0))
		   " "
		   (if (negative? tzoffset) "-" "+")
		   (number->padded-string 2 tzoffset-hours)
		   (number->padded-string 2 tzoffset-minutes))))


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

(TEST
 ;; use tzselect to find out about TZ values
 > (set-TZ! "Europe/London")
 > (ctime 1366681842)
 "Tue Apr 23 02:50:42 2013"
 > (gmtime 1366681842)
 #s32(42 50 1 23 3 113 2 112 0 0)
 > (tm->rfc-2822 (gmtime 1366681842))
 "Tue, 23 Apr 2013 01:50:42 +0000"
 > (localtime 1366681842)
 #s32(42 50 2 23 3 113 2 112 1 0)
 > (tm->rfc-2822 (localtime 1366681842))
 "Tue, 23 Apr 2013 02:50:42 +0100"

 > (set-TZ! "Europe/Zurich")
 > (ctime 1366681842)
 "Tue Apr 23 03:50:42 2013"
 > (tm->rfc-2822 (localtime 1366681842))
 "Tue, 23 Apr 2013 03:50:42 +0200"

 > (localtime 1356209442)
 #s32(42 50 21 22 11 112 6 356 0 -3600)
 > (ctime 1356209442)
 "Sat Dec 22 21:50:42 2012"
 > (tm->rfc-2822 (localtime 1356209442))
 "Sat, 22 Dec 2012 21:50:42 +0100"
 
 > (set-TZ! "America/La_Paz")
 > (ctime 1366681842)
 "Mon Apr 22 21:50:42 2013"
 > (tm->rfc-2822 (localtime 1366681842))
 "Mon, 22 Apr 2013 21:50:42 -0400"
 )

(def unixtime->rfc-2822 (compose tm->rfc-2822 localtime))

