;;; Copyright 2013-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require test
	 easy
	 unixtime-types
	 unixtime-Cpart
	 (list-util-1 map/iota)
	 cj-functional ;; contained in easy, though?
	 Maybe
	 (cj-env current-unixtime))

(export (method localtime.<
		localtime.<=
		unixtime.gmtime-string
		unixtime.rfc-2822
		unixtime.localtime-string)
	gmtime?
	current-localtime
	current-gmtime
	leap-year?
	year-days
	#!optional
	leap-years
	leap-years-len)



(def (localtime-compare/ tail-op)
     (insert-result-of
      (let* ((a-fields '(sec
			 min
			 hour
			 mday
			 month-1
			 year-1900))
	     (b-fields (map (lambda (f) (symbol-append "b-" f)) a-fields))
	     (fields (map values a-fields b-fields))
	     (fields-end (take fields 3))
	     (fields-start (drop fields 3)))

	(quasiquote-source
	 (lambda (a b)
	   (let-localtime
	    ((,@a-fields
	      _
	      _
	      integer-isdst
	      integer-timezone) a)
	    (let-localtime
	     ((,@b-fields
	       _
	       _
	       b-integer-isdst
	       b-integer-timezone) b)

	     (if (and integer-timezone b-integer-timezone)
		 ;; otherwise blindly trust that zone is fine
		 (assert (= integer-timezone b-integer-timezone)))

	     ;; Have to insert a check for isdst in the middle of the
	     ;; calculation, hence only use
	     ;; comparison-chain-littleendian-expand for the last 3
	     ;; comparisons
	     ,(fold (code-comparison-chain:or-and/ `fx< `fx=)
		    `(if (and integer-isdst b-integer-isdst
			      (fx= integer-isdst b-integer-isdst))
			 ,((comparison-chain-littleendian-expand `fx< `fx=
								 `tail-op)
			   fields-end)
			 ;; otherwise fall back on calculating
			 ;; unixtime; XX of course we still depend
			 ;; totally on the global TZ setting..
			 (tail-op (localtime.unixtime a)
				  (localtime.unixtime b)))
		    fields-start))))))))

(def. localtime.< (localtime-compare/ fx<))

(def. localtime.<= (localtime-compare/ fx<=))
	

(TEST
 > (def (gen-unixtime)
	(def from 1519862400)
	(def to 1551398400)
	(+ from (random-integer (- to from))))

 > (def (gen-unixtimes)
	(map (lambda (i)
	       (gen-unixtime))
	     ;; increase to 10000 or so for thorough testing
	     (iota 10000)))

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
 #t)



(def. (unixtime.gmtime-string v)
  (.rfc-2822-alike-string (unixtime.gmtime v) "GMT"))

(def. unixtime.rfc-2822 (compose localtime.rfc-2822 unixtime.localtime))

(def. (unixtime.localtime-string v)
  (.rfc-2822-alike-string (unixtime.localtime v) #f #f))

(TEST
 > (set-TZ! "Europe/Zurich")
 > (.gmtime 1329129873)
 #((localtime) 33 44 10 13 1 112 1 43 0 0)
 > (.gmtime-string #)
 "Mon, 13 Feb 2012 10:44:33 GMT"
 > (.gmtime-string 1329129873)
 "Mon, 13 Feb 2012 10:44:33 GMT"
 > (.localtime-string 1329129873)
 "Mon, 13 Feb 2012 11:44:33"
 > (.rfc-2822 1329129873) ;; unlike gmtime, influenced by TZ
 "Mon, 13 Feb 2012 11:44:33 +0100"
 > (.rfc-2822 1529129873)
 "Sat, 16 Jun 2018 08:17:53 +0200")


(def (gmtime? v)
     (and (localtime? v)
	  (zero? (localtime.integer-timezone v))
	  (zero? (localtime.integer-isdst v))))


(def current-localtime
     (compose unixtime.localtime current-unixtime))

(def current-gmtime
     (compose unixtime.gmtime current-unixtime))


(TEST
 ;; use tzselect to find out about TZ values
 > (set-TZ! "Europe/London")
 > (ctime 1366681842)
 "Tue Apr 23 02:50:42 2013"
 > (.gmtime 1366681842)
 #((localtime) 42 50 1 23 3 113 2 112 0 0)
 > (.gmtime-string 1366681842)
 "Tue, 23 Apr 2013 01:50:42 GMT"
 > (.rfc-2822 (.gmtime 1366681842))
 "Tue, 23 Apr 2013 01:50:42 +0000"
 > (.localtime 1366681842)
 #((localtime) 42 50 2 23 3 113 2 112 1 0)
 > (.rfc-2822 (.localtime 1366681842))
 "Tue, 23 Apr 2013 02:50:42 +0100"

 > (set-TZ! "Europe/Zurich")
 > (ctime 1366681842)
 "Tue Apr 23 03:50:42 2013"
 > (.rfc-2822 (.localtime 1366681842))
 "Tue, 23 Apr 2013 03:50:42 +0200"

 > (.localtime 1356209442)
 #((localtime) 42 50 21 22 11 112 6 356 0 -3600)
 > (ctime 1356209442)
 "Sat Dec 22 21:50:42 2012"
 > (.rfc-2822 (.localtime 1356209442))
 "Sat, 22 Dec 2012 21:50:42 +0100"
 
 > (set-TZ! "America/La_Paz")
 > (ctime 1366681842)
 "Mon Apr 22 21:50:42 2013"
 > (.rfc-2822 (.localtime 1366681842))
 "Mon, 22 Apr 2013 21:50:42 -0400"
 )

(TEST
 > (.rfc-2822 (dec (expt 2 31)))
 "Mon, 18 Jan 2038 23:14:07 -0400"
 ;; > (.rfc-2822 (expt 2 31))
 ;; "Mon, 18 Jan 2038 23:14:08 -0400"
 ;; > (.rfc-2822 (expt 2 32))
 ;; "Sun, 7 Feb 2106 02:28:16 -0400"
 )


(def leap-years
     ;; http://www.accuracyproject.org/leapyears.html
     '#u16(1804
	   1808
	   1812
	   1816
	   1820
	   1824
	   1828
	   1832
	   1836
	   1840
	   1844
	   1848
	   1852
	   1856
	   1860
	   1864
	   1868
	   1872
	   1876
	   1880
	   1884
	   1888
	   1892
	   1896
	   1904
	   1908
	   1912
	   1916
	   1920
	   1924
	   1928
	   1932
	   1936
	   1940
	   1944
	   1948
	   1952
	   1956
	   1960
	   1964
	   1968
	   1972
	   1976
	   1980
	   1984
	   1988
	   1992
	   1996
	   2000
	   2004
	   2008
	   2012
	   2016
	   2020
	   2024
	   2028
	   2032
	   2036
	   2040
	   2044
	   2048
	   2052
	   2056
	   2060
	   2064
	   2068
	   2072
	   2076
	   2080
	   2084
	   2088
	   2092
	   2096))

(def leap-years-len (u16vector-length leap-years))

(def (leap-year? #(natural0? year))
     (if (<= 1804 year 2096)
	 ;; now fancy binsearch or:
	 (let lp ((i 0))
	   (if (< i leap-years-len)
	       (let ((y (u16vector-ref leap-years i)))
		 (if (= y year)
		     #t
		     (lp (+ i 1))))
	       #f))
	 (error "year out of range:" year)))

(def (year-days #(natural0? year))
     (if (leap-year? year)
	 366
	 365))

(TEST
 > (year-days 2096)
 366
 > (year-days 2016)
 366
 > (year-days 2015)
 365)

