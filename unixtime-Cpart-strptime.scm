;;; Copyright 2016 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; strptime from glibc seems to be broken both with regards to
;; handling %z and the is_dst field. (The web seems to agree with
;; this.) Huh.

(require test
	 easy
	 unixtime-types
	 (unixtime-Cpart set-TZ!)
	 u8vector0)

(export strptime)


(c-declare "
       #define _GNU_SOURCE
       #define _XOPEN_SOURCE
       #include <time.h>
       #include <stdlib.h>
")


(def (strptime #(u8vector0? in) #(u8vector0? format))
     -> (values-of (maybe localtime?)
		   ;;XX size_t?
		   natural0?)

     (let* ((out (make-vector 11 'localtime))
	    (res (##c-code "
{
    const char *in = ___CAST(char*, ___BODY(___ARG1));
    const char *format = ___CAST(char*, ___BODY(___ARG2));
    ___SCMOBJ *out = ___BODY(___ARG3);
    struct tm res;
    const char *in2= strptime(in,format,&res);
    res.tm_isdst=-1; /* XXX why is this not being set? */
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
    ___RESULT= in2 ? ___FIX(in2-in) : ___FAL;
}
" in format out)))
       (if res
           (values out res)
           (values #f 0))))


(def rfc-2822-format
     (.utf8-u8vector0 "%a %b %d %H:%M:%S %z %Y"))
;; man strptime:
;; %a or %A
;;        The weekday name according to the  current  locale,
;;        in abbreviated form or the full name.
;; %b or %B or %h
;;        The  month name according to the current locale, in
;;        abbreviated form or the full name.
;; %d or %e
;;        The day of month (1-31).
;; %H     The hour (0-23).
;; %M     The minute (0-59).
;; %S     The second (0-60; 60 may occur  for  leap  seconds;
;;        earlier also 61 was allowed).
;; %z     An RFC-822/ISO 8601 standard timezone specification.
;; %Z     The timezone name.
;; %Y     The year, including century (for example, 1991).

(TEST
 > (set-TZ! "Europe/Zurich")
 > (values->vector (strptime (.utf8-u8vector0 "Thu Jan 21 07:35:56 2016")
			     (.utf8-u8vector0 "%a %b %d %H:%M:%S %Y")))
 #(#(localtime 56 35 7 21 0 116 4 20 -1 -3600) 24)
 ;; XXX -1 is wrong but how do we get it?

 > (values->vector (strptime (.utf8-u8vector0 "Hello") (.utf8-u8vector0 "%s")))
 #(#f 0)
 > (values->vector (strptime (.utf8-u8vector0 "Thu Jan 21 07:35:56 GMT 2016")
			     rfc-2822-format))
 #(#f 0) ;; this is WRONG of course but it seems that glibc just
	 ;; doesn't support it
 )

