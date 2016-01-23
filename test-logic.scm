;;; Copyright 2010-2014 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy test)

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


;; -- Existential quantification ---
;; there exists

;; again, returns null if true. --- or should this be something else
;; entirely anyway (conts? what else to try?)?

'(def ( )
     )
;; ∃

