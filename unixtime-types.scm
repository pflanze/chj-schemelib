;;; Copyright 2013-2016 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require test
	 easy
	 more-oo ;; contained in easy?
	 cj-functional ;; contained in easy?
	 (string-util-2 number->padded-string)
	 (vector-util vector.value.pos)
	 )



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


;; kinda-highlevel lowlevel data implementation (used for FFI)

(class localtime
       (struct sec
	       min
	       hour
	       mday
	       month-1
	       year-1900
	       integer-wday
	       integer-yday
	       integer-isdst
	       ;; This is contained neither in what Perl's `localtime` returns nor
	       ;; `struct tm` from time.h :
	       integer-timezone)

       (method month
	       (compose inc localtime.month-1))
       (method (year v)
	       (+ (localtime.year-1900 v) 1900))

       (method year-string
	       (compose number->string localtime.year))
       (method mday-string
	       (compose number->string localtime.mday))

       (method (hour-paddedstring v)
	       (number->padded-string 2 (localtime.hour v)))
       (method (min-paddedstring v)
	       (number->padded-string 2 (localtime.min v)))
       (method (sec-paddedstring v)
	       (number->padded-string 2 (localtime.sec v)))

       (method (tzoffset-string v)
	       (let* ((tzoffset  (localtime.tzoffset v))
		      ;; ^ is this really correct?
		      (tzoffsetm (abs tzoffset))
		      (tzoffset-hours (quotient tzoffsetm 3600))
		      (tzoffset-minutes (quotient (modulo tzoffsetm 3600) 60)))
		 (string-append (if (negative? tzoffset) "-" "+")
				(number->padded-string 2 tzoffset-hours)
				(number->padded-string 2 tzoffset-minutes))))


       ;; seconds
       (method (tzoffset tm) 
	       (- (* (localtime.integer-isdst tm) 3600)
		  (localtime.integer-timezone tm)))


       ;; more date -R format

       (method (wday-shortstring v)
	       (vector-ref rfc-2822:wdays (.integer-wday v)))

       (method (month-shortstring v)
	       (vector-ref rfc-2822:months (.month-1 v)))


       (def (rfc-2822-alike-string v maybe-zone-string)
	    (string-append (localtime.wday-shortstring v)
			   ", "
			   (localtime.mday-string v)
			   " "
			   (localtime.month-shortstring v)
			   " "
			   (localtime.year-string v)
			   " "
			   (localtime.hour-paddedstring v)
			   ":"
			   (localtime.min-paddedstring v)
			   ":"
			   (localtime.sec-paddedstring v)
			   " "
			   (or maybe-zone-string
			       (localtime.tzoffset-string v))))

       (method (rfc-2822 v)
	       (rfc-2822-alike-string v #f))

       (method (gmtime-string v)
	       (rfc-2822-alike-string v "GMT")))


