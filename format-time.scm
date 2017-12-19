(require easy
	 test
	 unixtime
	 (cj-math integer)
	 english)

(export format-time-relative-to
	format-time-local)

(declare (standard-bindings)
	 (extended-bindings)
	 (fixnum))




(defparameter current-now-range 3)

;; Careful, returns local time format *without* time zone information
;; if it needs to return an absolute time!
(def (format-time-relative-to #(unixtime? t)
			      #(unixtime? now)
			      #(boolean? short?)
			      #!optional
			      #(boolean? longtime-relative?))
     (let* ((d (- now t))
	    (d* (abs d))
	    (rel (lambda (d str)
		   (let ((n (integer d)))
		     (string-append
		      (.string n)
		      (if short? str (english:numerize str n))
		      " "
		      (if (< t now) "ago" "in the future"))))))
       (cond ((< d* (current-now-range)) "now")
	     ((< d* 60) (rel d* (if short? "″" " second")))
	     ((< d* (* 60 60)) (rel (/ d* 60) (if short? "′" " minute")))
	     ((< d* (* 60 60 24)) (rel (/ d* 60 60) (if short? "h" " hour")))
	     ((< d* (* 60 60 24 7)) (rel (/ d* 60 60 24) (if short? "d" " day")))
	     (else
	      (if longtime-relative?
		  (cond ((< d* (* 60 60 24 31))
			 (rel (/ d* 60 60 24 7) (if short? "wk" " week")))
			((< d* (* 60 60 24 365))
			 (rel (/ d* 60 60 24 30) (if short? "mo" " month")))
			(else
			 (rel (/ d* 60 60 24 365) (if short? "y" " year"))))
		  ;; (set-TZ! "Europe/London") which is not thread safe,
		  ;; so horrible, need locking API (with fast lock? And
		  ;; dyn wind.)
		  (.localtime-string (.localtime t)))))))

(TEST
 > (set-TZ! "Europe/London")
 > (format-time-relative-to (- 1477398871 20) 1477398871 #f)
 "20 seconds ago"
 > (format-time-relative-to 1477397634 1477398871 #f)
 "20 minutes ago"
 > (format-time-relative-to 1477398871 1477397634 #f)
 "20 minutes in the future"
 > (format-time-relative-to (- 1477397634 100000) 1477398871 #f)
 "1 day ago"
 > (format-time-relative-to (- 1477397634 1000000) 1477398871 #f #t)
 "1 week ago"
 > (format-time-relative-to (- 1477397634 2000000) 1477398871 #f #t)
 "3 weeks ago"
 > (format-time-relative-to (- 1477397634 3000000) 1477398871 #f #t)
 "1 month ago"
 > (format-time-relative-to (- 1477397634 4000000) 1477398871 #f #t)
 "1 month ago"
 > (format-time-relative-to (- 1477397634 6000000) 1477398871 #f #t)
 "2 months ago"
 > (format-time-relative-to (- 1477397634 6000000) 1477398871 #t #t)
 "2mo ago"

 > (format-time-relative-to (- 1477397634 1000000) 1477398871 #f)
 "Thu, 13 Oct 2016 23:27:14" ;; XX completely crazy to say seconds here; but what?
 > (format-time-relative-to 1477398871 1477398871 #f)
 "now"
 )

;; same in short format
(TEST
 > (format-time-relative-to (- 1477398871 20) 1477398871 #t)
 "20″ ago"
 > (format-time-relative-to 1477397634 1477398871 #t)
 "20′ ago"
 > (format-time-relative-to 1477398871 1477397634 #t)
 "20′ in the future"
 > (format-time-relative-to (- 1477397634 100000) 1477398871 #t)
 "1d ago"
 > (format-time-relative-to (- 1477397634 1000000) 1477398871 #t)
 "Thu, 13 Oct 2016 23:27:14"
 > (format-time-relative-to 1477398871 1477398871 #t)
 "now"
 )





;; XXX Bug? should follow a current-TZ and, then?

;; Always show absolute local time, but only show date if not on the
;; same day
(def (format-time-local #(unixtime? integer-t)
			#(unixtime? integer-now))
     (if (< (abs (- integer-t integer-now)) (current-now-range))
	 "now"
	 (let* ((t (unixtime.localtime integer-t))
		(now (unixtime.localtime integer-now))
		(timestr (string-append (.time-string t)))
		(sameyear? (= (.year t) (.year now)))
		(samemonth? (and sameyear? (= (.month t) (.month now))))
		(sameday? (and samemonth? (= (.mday t) (.mday now))))
		(past? (< integer-t integer-now))
		(daysdiff (let ((d (abs (- (.yday now) (.yday t)))))
			    (if (> d 150)
				(if (= (abs ((on .year -) t now))
				       1)
				    (abs (- d (year-days
					       (.year (if past?
							  t now)))))
				    ;; too far anyway, don't care
				    #f)
				d))))
	   (if sameday?
	       timestr
	       (cond ((= daysdiff 1)
		      (string-append (if past? "yesterday" "tomorrow")
				     " "
				     timestr))
		     ((< daysdiff 7)
		      (string-append (if past? "" "coming ")
				     (.wday-longstring t)
				     " "
				     timestr))
		     (else
		      (let ((longish (string-append (.wday-shortstring t)
						    ", "
						    (number.string (.mday t))
						    " "
						    (.month-shortstring t)
						    " "
						    timestr)))
			(if sameyear?
			    longish
			    (string-append longish " " (.string (.year t)))))))))))


(TEST
 > (set-TZ! "Europe/London")
 > (format-time-local (- 1477398871 20) 1477398871)
 "13:34:11"
 > (.localtime-string 1477397634)
 "Tue, 25 Oct 2016 13:13:54"
 > (format-time-local 1477397634 1477398871)
 "13:13:54"
 > (format-time-local 1477398871 1477397634)
 "13:34:31"

 > (def t 1477398871)
 > (.localtime-string t)
 "Tue, 25 Oct 2016 13:34:31"
 > (format-time-local (- t (* 24 60 60)) t)
 "yesterday 13:34:31"
 > (format-time-local (- t (* 2 24 60 60)) t)
 "Sunday 13:34:31"
 > (format-time-local (- t (* 6 24 60 60)) t)
 "Wednesday 13:34:31"
 > (format-time-local (- t (* 7 24 60 60)) t)
 "Tue, 18 Oct 13:34:31"

 > (format-time-local (+ t (* 24 60 60)) t)
 "tomorrow 13:34:31"
 > (format-time-local (+ t (* 2 24 60 60)) t)
 "coming Thursday 13:34:31"
 > (format-time-local (+ t (* 6 24 60 60)) t)
 "coming Monday 12:34:31"
 > (.localtime-string (+ t (* 6 24 60 60)))
 "Mon, 31 Oct 2016 12:34:31"
 > (format-time-local (+ t (* 7 24 60 60)) t)
 "Tue, 1 Nov 12:34:31"
 > (.localtime-string (+ t (* 7 24 60 60)))
 "Tue, 1 Nov 2016 12:34:31"

 > (format-time-local 1477398871 1477398871)
 "now"
 )

