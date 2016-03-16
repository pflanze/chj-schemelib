;;; Copyright 2013-2016 by Christian Jaeger <chrjae@gmail.com>

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


(def. (unixtime.gmtime-string v)
  (rfc-2822-alike-string (unixtime.gmtime v) "GMT"))

(def. unixtime.rfc-2822 (compose localtime.rfc-2822 unixtime.localtime))


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
 #(localtime 42 50 1 23 3 113 2 112 0 0)
 > (.gmtime-string 1366681842)
 "Tue, 23 Apr 2013 01:50:42 GMT"
 > (.rfc-2822 (.gmtime 1366681842))
 "Tue, 23 Apr 2013 01:50:42 +0000"
 > (.localtime 1366681842)
 #(localtime 42 50 2 23 3 113 2 112 1 0)
 > (.rfc-2822 (.localtime 1366681842))
 "Tue, 23 Apr 2013 02:50:42 +0100"

 > (set-TZ! "Europe/Zurich")
 > (ctime 1366681842)
 "Tue Apr 23 03:50:42 2013"
 > (.rfc-2822 (.localtime 1366681842))
 "Tue, 23 Apr 2013 03:50:42 +0200"

 > (.localtime 1356209442)
 #(localtime 42 50 21 22 11 112 6 356 0 -3600)
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
 > (.rfc-2822 (expt 2 31))
 "Mon, 18 Jan 2038 23:14:08 -0400"
 > (.rfc-2822 (expt 2 32))
 "Sun, 7 Feb 2106 02:28:16 -0400")

