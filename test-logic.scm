;;; Copyright 2010-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy test)

(export forall
	#!optional
	Lforall)


;; Logic? Or just

;; interface: null when no failure. Failures otherwise.


;; -- Universal quantification ---
;; forall

(def (Lforall vs pred)
     (stream-filter (complement pred) vs))

(def ∀ Lforall)
;; ok? also, should it be curried? also.. ?

;; also, value sources actually abstract i guess 'for randomized
;; trials' .  hm.  branch-tracking  whatever.
;; [hmm  |natural0| as  a source. possibly. ?]

(TEST
 > (F (Lforall '(1 3) number?))
 ()
 > (F (Lforall '(1 3 a 5 c 8) number?))
 (a c)
 )


(def (forall vs pred)
     (force (Lforall vs pred)))
;; yes really the same as the current qcheck, but the latter may
;; change. OK?

;; shouldn't the order of arguments be reversed, both for wording (we
;; check pred, not vs), and to make n-ary in the future?
(def (qcheck vs pred)
     (force (Lforall vs pred)))

(TEST
 > (qcheck (iota 5) integer?)
 ()
 > (promise? (cdr (qcheck (iota 5) natural?)))
 #t
 > (F (qcheck (iota 5) natural?))
 ;; *expected* failure
 (0))

(defstruct testfailure
  value
  results)

(def (Lforall* vs equal? . fs)
     (stream-fold-right
      (lambda (v res)
	(let ((vs (map (lambda (f)
			 (f v)) fs)))
	  (if (apply equal? vs)
	      res
	      (cons (testfailure v vs)
		    res))))
      '()
      vs))

(def (qcheck* vs #!key (equal? equal?) #!rest fs)
     (force (apply Lforall* vs equal? fs)))

(TEST
 > (F (qcheck* (iota 4) square identity))
 (#((testfailure) 2 (4 2)) #((testfailure) 3 (9 3))))



;; -- Existential quantification ---
;; there exists

;; again, returns null if true. --- or should this be something else
;; entirely anyway (conts? what else to try?)?

'(def ( )
     )
;; ∃




;; -- utilities --

(def (random:permutate l)
     (map cdr
	  (sort (map (lambda (v)
		       (cons (random-real) v))
		     (stream->list l))
		(on car <))))

