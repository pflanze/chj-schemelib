;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy)


"Data structure to carry the result of |time| syntax (currently only supported via external script to parse the output from Gambit, to be integrated properly in the future)"


;; change fixnum-natural0? in times to real? for wider usage?

(defclass (time-ms [fixnum-natural0? ms])
  (defclass (real-time))
  (defclass (cpu-time [user+system? user+system])
    (defmethod (check! _)
      (assert (= ms (user+system.total-ms user+system))))))

;; should this be is-a time-something ?
(defclass (user+system [fixnum-natural0? user-time]
                       [fixnum-natural0? system-time])
  (defmethod (total-ms _)
    (+ user-time system-time)))


(defclass (tagged-fixnum-natural0 [fixnum-natural0? value])
  (defclass (bytes-allocated))
  (defclass (minor-faults))
  (defclass (major-faults)))


(defclass (collections [fixnum-natural0? number]
                       [fixnum-natural0? real-ms]
                       [user+system? user+system])
  (defmethod (check! _)
    ;; odd, actually doesn't match up, real-ms appears to be larger
    ;; though. Enforce this..
    (assert (>= real-ms
                (user+system.total-ms user+system)))))

(defclass (time-result [real-time? real-time]
                       [cpu-time? cpu-time]
                       [collections? collections]
                       [bytes-allocated? bytes-allocated]
                       [minor-faults? minor-faults]
                       [major-faults? major-faults])
  ;; (def (time-result* . args)
  ;;      ;; expect args to be in order already? Wouldn't be necessary;
  ;;      ;; stupid?
  ;;      (apply time-result args))
  (defmethod (check! _)
    (.check! cpu-time)
    (.check! collections)))


(TEST
 > (def t (time-result (real-time 8118)
                       (cpu-time 8020 (user+system 7372 648))
                       (collections 3 1893 (user+system 1600 280))
                       (bytes-allocated 1318727608)
                       (minor-faults 260763)
                       (major-faults 0)))
 ;; ah heh nice thing about all of this wrapping is that it shows up
 ;; in |show| etc! No more positional type dependency, "of course",
 ;; "heh"!
 > (.check! t))



