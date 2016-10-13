
;; HACK. ~Horribly slow except when cached, even then probably slow.

(require easy
	 fscache
	 (cj-io-util xbacktick)
	 (unixtime-Cpart unixtime?)
	 test)

;; using `dateparse` from my chj-bin repository
(def (slow-dateparse #(string? s)) -> unixtime?
     (string->number (xbacktick "dateparse" "-u" "-B" s)))

(def dateparse:basedir ".dateparse-cachedir/")
(%try (create-directory dateparse:basedir)) ;;XX should be done by fscache
(def dateparse:cache (fscache dateparse:basedir
			      car
			      slow-dateparse
			      number->string
			      string->number))

(def dateparse (.call* dateparse:cache))

(TEST
 > (dateparse "2001/10/3 12:33 GMT")
 1002112380
 > (dateparse "2001/10/3 12:33 MET")
 1002108780
 > (dateparse "2001/10/3 12:33 MEST")
 1002105180)

