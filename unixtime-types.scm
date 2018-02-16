;;; Copyright 2013-2018 by Christian Jaeger <ch@christianjaeger.ch>

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
	 comparison-chain-macro
	 test
	 (list-util repeatedly))


(export rfc-2822:string.wday
	rfc-2822:string.month-1
	rfc-2822:string.year-1900
	(jclass localtime))



;; date -R format

(def rfc-2822:wdays
  '#("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

(def rfc-2822:months
     '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(def months-longstring
     '#("January" "Febuary" "March" "April" "May" "June" "July" "August"
	"September" "October" "November" "December"))

(def rfc-2822:string.wday (vector.value.pos rfc-2822:wdays))
(def rfc-2822:string.month-1 (vector.value.pos rfc-2822:months))
(def month-longstring.month-1 (vector.value.pos months-longstring))
(def (rfc-2822:string.year-1900 str)
     (- (string->number str) 1900))


(TEST
 > (rfc-2822:string.month-1 "Jan")
 0
 > (rfc-2822:string.month-1 "Feb")
 1
 > (rfc-2822:string.month-1 "Dec")
 11
 > (%try-error (rfc-2822:string.month-1 "December"))
 #(error "unknown key:" "December")
 > (rfc-2822:string.wday "Mon")
 1
 > (rfc-2822:string.wday "Sun")
 0
 > (rfc-2822:string.wday "Sat")
 6
 > (rfc-2822:string.year-1900 "2017")
 117)



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

	(def-method (show-nice v)
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


	(defmacro (unixtime-types:def-comparison-method op)
	  (let* ((fields '(sec
			   min
			   hour
			   mday
			   month-1
			   year-1900))
		 (b-fields (map (lambda (f) (symbol-append "b-" f)) fields)))
	  
	    (quasiquote-source
	     (def-method (,op a b)
	       (let-localtime
		((,@b-fields
		  _
		  _
		  _
		  b-integer-timezone) b)
	   
		(assert (= integer-timezone b-integer-timezone))
		;; ignoring isdst is fine? XX still check? 
	   
		,(comparison-chain-expand `< `= op
					  (reverse fields)
					  (reverse b-fields)))))))
	
	(unixtime-types:def-comparison-method <)

	(unixtime-types:def-comparison-method <=)
	
	
	(def-method (month v)
	  (inc month-1))

	(def-method (year v)
	  (+ year-1900 1900))

	(def-method- (year-string v)
	  (number->string (localtime.year v)))

	(def-method (mday-string v)
	  (number->string mday))

	(def-method (hour-paddedstring v)
	  (number->padded-string 2 hour))

	(def-method (min-paddedstring v)
	  (number->padded-string 2 min))

	(def-method (sec-paddedstring v)
	  (number->padded-string 2 sec))

	;; seconds
	(def-method (tzoffset tm) 
	  (- (* integer-isdst 3600)
	     integer-timezone))

	(def-method (tzoffset-string v)
	  (let* ((tzoffset (localtime.tzoffset v))
		 ;; ^ is this really correct?
		 (tzoffsetm (abs tzoffset))
		 (tzoffset-hours (quotient tzoffsetm 3600))
		 (tzoffset-minutes (quotient (modulo tzoffsetm 3600) 60)))
	    (string-append (if (negative? tzoffset) "-" "+")
			   (number->padded-string 2 tzoffset-hours)
			   (number->padded-string 2 tzoffset-minutes))))


	;; more date -R format

	(def-method (wday-shortstring v)
	  (vector-ref rfc-2822:wdays integer-wday))

	(def-method (wday-longstring v)
	  (vector-ref english:weekdays integer-wday))

	(def-method (month-shortstring v)
	  (vector-ref rfc-2822:months month-1))
 
	(def-method (month-longstring v)
	  (vector-ref months-longstring month-1))

	(def-method- (time-string v #!optional (with-seconds? #t))
	  (string-append
	   (localtime.hour-paddedstring v)
	   ":"
	   (localtime.min-paddedstring v)
	   (if with-seconds?
	       (string-append ":"
			      (localtime.sec-paddedstring v))
	       "")))


	(def-method- (rfc-2822-but-date-only-string v)
	  (string-append (localtime.wday-shortstring v)
			 ", "
			 (localtime.mday-string v)
			 " "
			 (localtime.month-shortstring v)
			 " "
			 (localtime.year-string v)))


	(def-method- (month-and-year-shortstring v)
	  (string-append (localtime.month-shortstring v)
			 " "
			 (localtime.year-string v)))
 
	(def-method- (month-and-year-longstring v)
	  (string-append (localtime.month-longstring v)
			 " "
			 (localtime.year-string v)))
	

	(def-method- (rfc-2822-alike-string v maybe-zone-string
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

	(def-method- (rfc-2822 v)
	  (.rfc-2822-alike-string v #f))

	(def-method- (gmtime-string v)
	  (.rfc-2822-alike-string v "GMT"))

	(def-method- (localtime-string v)
	  (.rfc-2822-alike-string v #f #f))

	;; Time boundary calculations

	(def-method (month-start s)
	  (=> s
	      (.mday-set 1)
	      (.hour-set 0)
	      (.min-set 0)
	      (.sec-set 0)
	      (.integer-isdst-set -1)))
	
	(def-method (month-inc s #!optional keep-dst?)
	  (let ((r (if (>= month-1 11)
		       (=> s
			   (.year-1900-update inc)
			   (.month-1-set 0))
		       (.month-1-update s inc))))
	    (if keep-dst?
		r
		(.integer-isdst-set r -1)))))



(TEST
 > (def l (localtime 20 28 16 12 0 118 5 11 0 0))
 > (def (l* n)
	(let ((lt (repeatedly n .month-inc (.month-start l))))
	  (.show (values lt (.localtime-string (.unixtime lt))))))
 > (l* 0)
 (values (localtime 0 0 0 1 0 118 5 11 -1 0) "Mon, 1 Jan 2018 00:00:00")
 > (l* 1)
 (values (localtime 0 0 0 1 1 118 5 11 -1 0) "Thu, 1 Feb 2018 00:00:00")
 > (l* 6)
 (values (localtime 0 0 0 1 6 118 5 11 -1 0) "Sun, 1 Jul 2018 00:00:00")
 > (l* 11)
 (values (localtime 0 0 0 1 11 118 5 11 -1 0) "Sat, 1 Dec 2018 00:00:00")
 > (l* 12)
 (values (localtime 0 0 0 1 0 119 5 11 -1 0) "Tue, 1 Jan 2019 00:00:00"))

(TEST
 > (def (gen-unixtime)
	(def from 1519862400)
	(def to 1551398400)
	(+ from (random-integer (- to from))))

 > (def (gen-unixtimes)
	(map (lambda (i)
	       (gen-unixtime))
	     ;; increase to 10000 or so for thorough testing
	     (iota 100)))

 > (def (test-comparison method op)
	(let* ((a-t (gen-unixtime))
	       (a-l (.localtime a-t)))
	  (every (lambda (b-t)
		   (let ((b-l (.localtime b-t)))
		     (equal? (method a-l b-l)
			     (op a-t b-t))))
		 (gen-unixtimes))))

 > (test-comparison localtime.< <)
 #t
 > (test-comparison localtime.<= <=)
 #t
 )
