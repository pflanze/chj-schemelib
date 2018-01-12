;;; Copyright 2013-2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 jclass
	 cj-functional ;; contained in easy?
	 (string-util-2 number->padded-string)
	 (vector-util vector.value.pos)
	 english
	 test)



;; date -R format

(def rfc-2822:wdays
  '#("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

(def rfc-2822:months
     '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(def rfc-2822:string.wday (vector.value.pos rfc-2822:wdays))
(def rfc-2822:string.month (vector.value.pos rfc-2822:months))


(TEST
 > (rfc-2822:string.month "Jan")
 0
 > (rfc-2822:string.month "Feb")
 1
 > (rfc-2822:string.month "Dec")
 11
 > (%try-error (rfc-2822:string.month "December"))
 #(error "unknown key:" "December")
 > (rfc-2822:string.wday "Mon")
 1
 > (rfc-2822:string.wday "Sun")
 0
 > (rfc-2822:string.wday "Sat")
 6)



;; Hacky?.. for .show-nice
(defmacro (FIELD fieldname value)
  value)


;; kinda-highlevel lowlevel data implementation (used for FFI)

(jclass (localtime sec
		   min
		   hour
		   mday
		   month-1
		   year-1900
		   integer-wday
		   yday
		   integer-isdst
		   ;; This is contained neither in what Perl's `localtime` returns nor
		   ;; `struct tm` from time.h :
		   integer-timezone)

	(def-method* (show-nice v)
	  `(localtime (FIELD sec: ,sec)
		      (FIELD min: ,min)
		      (FIELD hour: ,hour)
		      (FIELD mday: ,mday)
		      (FIELD month-1: ,month-1)
		      (FIELD year-1900: ,year-1900)
		      (FIELD integer-wday: ,integer-wday)
		      (FIELD yday: ,yday)
		      (FIELD integer-isdst: ,integer-isdst)
		      (FIELD integer-timezone: ,integer-timezone)))

	(def-method* (month v)
	  (inc month-1))

	(def-method* (year v)
	  (+ year-1900 1900))

	(def-method (year-string v)
	  (number->string (localtime.year v)))

	(def-method* (mday-string v)
	  (number->string mday))

	(def-method* (hour-paddedstring v)
	  (number->padded-string 2 hour))

	(def-method* (min-paddedstring v)
	  (number->padded-string 2 min))

	(def-method* (sec-paddedstring v)
	  (number->padded-string 2 sec))

	;; seconds
	(def-method* (tzoffset tm) 
	  (- (* integer-isdst 3600)
	     integer-timezone))

	(def-method* (tzoffset-string v)
	  (let* ((tzoffset (localtime.tzoffset v))
		 ;; ^ is this really correct?
		 (tzoffsetm (abs tzoffset))
		 (tzoffset-hours (quotient tzoffsetm 3600))
		 (tzoffset-minutes (quotient (modulo tzoffsetm 3600) 60)))
	    (string-append (if (negative? tzoffset) "-" "+")
			   (number->padded-string 2 tzoffset-hours)
			   (number->padded-string 2 tzoffset-minutes))))


	;; more date -R format

	(def-method* (wday-shortstring v)
	  (vector-ref rfc-2822:wdays integer-wday))

	(def-method* (wday-longstring v)
	  (vector-ref english:weekdays integer-wday))

	(def-method* (month-shortstring v)
	  (vector-ref rfc-2822:months month-1))

	(def-method (time-string v #!optional (with-seconds? #t))
	  (string-append
	   (localtime.hour-paddedstring v)
	   ":"
	   (localtime.min-paddedstring v)
	   (if with-seconds?
	       (string-append ":"
			      (localtime.sec-paddedstring v))
	       "")))

	(def-method (rfc-2822-alike-string v maybe-zone-string
					   #!optional (show-zone? #t))
	  (string-append (localtime.wday-shortstring v)
			 ", "
			 (localtime.mday-string v)
			 " "
			 (localtime.month-shortstring v)
			 " "
			 (localtime.year-string v)
			 " "
			 (localtime.time-string v)
			 (if show-zone? " " "")
			 (if show-zone?
			     (or maybe-zone-string
				 (localtime.tzoffset-string v))
			     "")))

	(def-method (rfc-2822 v)
	  (.rfc-2822-alike-string v #f))

	(def-method (gmtime-string v)
	  (.rfc-2822-alike-string v "GMT"))

	(def-method (localtime-string v)
	  (.rfc-2822-alike-string v #f #f))

	;; Time boundary calculations

	(def-method* (month-start s)
	  (=> s
	      (.mday-set 1)
	      (.hour-set 0)
	      (.min-set 0)
	      (.sec-set 0)
	      (.integer-isdst-set -1)))
	
	(def-method* (month-inc s #!optional keep-dst?)
	  (let ((r (if (= month-1 12)
		       (.year-1900-update (.month-set s 1) inc)
		       (.month-1-update s inc))))
	    (if keep-dst?
		r
		(.integer-isdst-set r -1)))))



(TEST
 > (def l (localtime 20 28 16 12 0 118 5 11 0 0))
 > (.localtime-string (mktime (.month-start l)))
 "Mon, 1 Jan 2018 00:00:00"
 > (.localtime-string (mktime (.month-inc (.month-start l))))
 "Thu, 1 Feb 2018 00:00:00"
 > (.localtime-string (mktime (.month-inc (.month-inc (.month-inc (.month-inc (.month-inc (.month-inc (.month-start l)))))))))
 "Sun, 1 Jul 2018 00:00:00"
 > (.localtime-string (mktime (.month-inc (.month-inc (.month-inc (.month-inc (.month-inc (.month-inc (.month-inc (.month-inc (.month-inc (.month-inc (.month-inc (.month-start l))))))))))))))
 "Sat, 1 Dec 2018 00:00:00"
 > (.localtime-string (mktime (.month-inc (.month-inc (.month-inc (.month-inc (.month-inc (.month-inc (.month-inc (.month-inc (.month-inc (.month-inc (.month-inc (.month-inc (.month-start l)))))))))))))))
 "Tue, 1 Jan 2019 00:00:00")

