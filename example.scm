#!/usr/bin/env gsi-script

(load "statprof.scm")

(define (main)

  (profile-start!)
  (load "ring.scm")
  (profile-stop!)

  (write-profile-report "ring"))
