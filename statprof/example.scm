(require statprof
	 statprof/ring)

(define (main)

  (profile-start!)
  (ring 10000)
  (profile-stop!)

  (write-profile-report "ring"))
