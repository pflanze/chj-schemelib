;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         subtitles
         Maybe
         monad/syntax
         ;; for .adjust-scale
         math/least-squares
         ;; for .interpolate:
         wbtable
         math/interpolate
         test)

(export (methods
         ;; alternatives for `subtitles-directives.subtitles-items`:
         subtitles-directives.adjust-scale
         subtitles-directives.interpolate
         subtitles-directives.renumber

         ;; after processing to subtitles-items:
         subtitles-items.cut-overlaps
         subtitles-items.drop-parentized

         ;; in any stage:
         subtitles-directives.map-Ts

         ;; other:
         subtitles-directives.shift-points
         subtitles-directives.shift-points-wbtable
         wbtable.interpolate-function-for
         subtitles-directives.interpolate-function-for)

        #!optional
        string.parentized?)

(include "cj-standarddeclares.scm")

"Extra functionality: transformations on subtitles"


(def. (subtitles-directives.shift-points l extract)
  (let lp ((l l)
           (shiftpoints '()))
    (if-let-pair
     ((a l*) l)

     (xcond ((subtitles-item? a) (lp l* shiftpoints))
            ((subtitles-time? a)
             (if-let-pair
              ((b l**) l*)
              (lp l*
                  (cons (cons (extract (.from b))
                              (extract a))
                        shiftpoints))
              (error "missing T after subtitles-time")))
            ((Tdelay? a)
             ;; An idea is to actually wrap the mapping points,
             ;; and run |.subtitles-items| before |.adjust-scale|.
             (error ($ "don't currently know how to handle "
                       "Tdelay in this context"))))
     
     shiftpoints)))



(def. (subtitles-directives.adjust-scale l #!optional [boolean? keep-times?])
  -> (if keep-times? (list-of subtitles-directive?) (list-of subtitles-item?))
  "'Clean up' `subtitles-directive`s to just `T`s (unless `keep-times?` is #t),
taking subtitle-time elements as data points for *scaling* the time
line (the whole time line is scaled by a single linear factor)."
  (let* ((ps (subtitles-directives.shift-points l .milliseconds))
         (f (.fit ps))
         (f* (=>* .milliseconds f integer milliseconds->tim)))
    (.filter-map l (lambda (a)
                     (xcond ((subtitles-item? a)
                             (=> a
                                 (.from-update f*)
                                 (.to-update f*)))
                            ((subtitles-time? a)
                             (and keep-times? a))
                            ((Tdelay? a)
                             ;; ditto
                             (error ($ "don't currently know how to handle "
                                       "Tdelay with .adjust-scale"))))))))

(TEST
 > (=> (list (T 3 (tim 0 6 54 144) (tim 0 6 56 847) "a")
             (tim 0 6 50 0)
             (T 4 (tim 0 6 57 14) (tim 0 6 59 136) "b")
             (T 5 (tim 0 7 1 812) (tim 0 7 3 589) "c")
             (tim 0 8 1 0)
             (T 7 (tim 0 7 3 591) (tim 0 7 6 657) "d"))
       .adjust-scale subtitles-show)
 (subtitles:list (T 3 (tim 0 6 19 17) (tim 0 6 48 197) "a")
                 (T 4 (tim 0 6 50 0) (tim 0 7 12 907) "b")
                 (T 5 (tim 0 7 41 795) (tim 0 8 0 978) "c")
                 (T 7 (tim 0 8 1 0) (tim 0 8 34 98) "d"))
 > (=> (list (tim 0 6 20 500)
             (T 3 (tim 0 6 54 144) (tim 0 6 56 847) "a")
             (tim 0 6 50 0)
             (T 4 (tim 0 6 57 14) (tim 0 6 59 136) "b")
             (T 5 (tim 0 7 1 812) (tim 0 7 3 589) "c")
             (tim 0 8 1 0)
             (T 7 (tim 0 7 3 591) (tim 0 7 6 657) "d"))
       .adjust-scale subtitles-show)
 (subtitles:list (T 3 (tim 0 6 20 44) (tim 0 6 48 873) "a")
                 (T 4 (tim 0 6 50 654) (tim 0 7 13 286) "b")
                 (T 5 (tim 0 7 41 827) (tim 0 8 0 779) "c")
                 (T 7 (tim 0 8 0 801) (tim 0 8 33 501) "d")))


(def list->real/real-wbtable (list->wbtable-of real? real-cmp real?))

(def. subtitles-directives.shift-points-wbtable
  (=>* (.shift-points .milliseconds) list->real/real-wbtable))

;; wbtable? -> [real? t-ms] -> [real? t-ms] -> exact-integer?
(def. (wbtable.interpolate-function-for tbl) -> function?
  (lambda ([real? t-ms]) -> (Maybe function?)
     (let ((with-prev+next
            (lambda (prev next)
              (return (lambda ([real? t-ms]) -> exact-integer?
                         (=>> t-ms
                              (interpolate prev next)
                              integer))))))
       (Maybe:if (.Maybe-ref tbl t-ms)
                 (Maybe:if-let ((n (.Maybe-next tbl t-ms)))
                               (with-prev+next it n)
                               ;; for the `to` value in the last
                               ;; entry, interpolate from before:
                               (>>= (.Maybe-prev tbl t-ms)
                                    (C with-prev+next _ it)))
                 (mlet ((prev (.Maybe-prev tbl t-ms))
                        (next (.Maybe-next tbl t-ms)))
                       (with-prev+next prev next))))))

;; l -> [real? t-ms] -> [real? t-ms] -> exact-integer?
(def. (subtitles-directives.interpolate-function-for l) -> function?
  (.interpolate-function-for (.shift-points-wbtable l)))


(def. (subtitles-directives.interpolate l #!optional [boolean? keep-times?])
  -> (if keep-times? (list-of subtitles-directive?) (list-of subtitles-item?))
  "'Clean up' `subtitles-directive`s to just `T`s (unless `keep-times?` is #t),
interpolating linearly between subtitle-time elements (tieing each
subtitle-time element exactly to its following T entry)."
  (let* ((Maybe-f-for
          (subtitles-directives.interpolate-function-for l))
         ;; The above works on milliseconds, but we'll need to deal
         ;; with any subtitles-time:
         (Maybe-timf-for
          (lambda (for-t) -> (Maybe function?)
             (>>= (Maybe-f-for (.milliseconds for-t))
                  (lambda (f)
                    (return (=>* .milliseconds
                                 f
                                 milliseconds->tim)))))))
    (let lp ((l l)
             (out '()))
      
      (if-let-pair
       ((a l*) l)
       (xcond
        ((T-meta? a)
         (lp l* (cons a out)))

        ((subtitles-item? a)
         (Maybe:if-let ((f (Maybe-timf-for (.from a))))
                       (=> a
                           (.from-update f)
                           (.to-update f)
                           ((lambda (a*)
                              (lp l* (cons a* out)))))
                       ;; (and, already don't know details about the error
                       ;; due to using Maybe not Result...: )
                       (raise-location-error
                        (.maybe-location a)
                        "T out of range, please make sure the start and the end of the subtitles are paired with subtitle-time entries"
                        a)))

        ((subtitles-time? a)
         (if keep-times?
             (lp l* (cons a out))
             (lp l* out)))

        ((Tdelay? a)
         ;; ditto
         (error ($ "don't currently know how to handle "
                   "Tdelay with .interpolate"))))
       
       (reverse out)))))

(TEST
 > (=> (list (tim 0 0 0 100)
             (T 3 (tim 0 5 0 100) (tim 0 5 0 200) "a")
             (T 4 (tim 0 5 0 300) (tim 0 5 0 400) "b")
             (tim 0 0 0 200)
             (T 5 (tim 0 5 0 500) (tim 0 5 0 600) "c"))
       .interpolate subtitles-show)
 (subtitles:list (T 3 (tim 0 0 0 100) (tim 0 0 0 125) "a")
                 (T 4 (tim 0 0 0 150) (tim 0 0 0 175) "b")
                 (T 5 (tim 0 0 0 200) (tim 0 0 0 225) "c"))
 > (=> (list (tim 0 0 0 100)
             (T 3 (tim 0 5 0 100) (tim 0 5 0 200) "a")
             (T 4 (tim 0 5 0 300) (tim 0 5 0 400) "b")
             (tim 0 0 0 200)
             (T 5 (tim 0 5 0 500) (tim 0 5 0 600) "c")
             (T 6 (tim 0 5 0 800) (tim 0 5 0 900) "d")
             (tim 0 0 0 300)
             (T 7 (tim 0 5 1 000) (tim 0 5 1 100) "e"))
       .interpolate subtitles-show)
 (subtitles:list (T 3 (tim 0 0 0 100) (tim 0 0 0 125) "a")
                 (T 4 (tim 0 0 0 150) (tim 0 0 0 175) "b")
                 (T 5 (tim 0 0 0 200) (tim 0 0 0 220) "c")
                 (T 6 (tim 0 0 0 260) (tim 0 0 0 280) "d")
                 (T 7 (tim 0 0 0 300) (tim 0 0 0 320) "e")))


(def. (subtitles-directives.renumber l #!optional ([fixnum? from] 1))
  (let lp ((l l)
           (no from)
           (out '()))
    (if-let-pair
     ((a l*) l)
     (cond
       ;; should I make no-set part of subtitles-item, then check for
       ;; subtitles-item (and the alternative would then be the classes
       ;; outside that tree only, i.e. modifiers), would that be
       ;; safer?
      ((T/location? a)
       (lp l*
           (inc no)
           (cons (.no-set a no)
                 out)))
      (else
       (lp l*
           no
           (cons a out))))
     (reverse out))))

(TEST
 > (=> (list (tim 0 0 0 100)
             (T 3 (tim 0 5 0 100) (tim 0 5 0 200) "a")
             (T 4 (tim 0 5 0 300) (tim 0 5 0 400) "b")
             (tim 0 0 0 200)
             (T 5 (tim 0 5 0 500) (tim 0 5 0 600) "c"))
       .renumber subtitles-show)
 (subtitles:list (tim 0 0 0 100)
                 (T 1 (tim 0 5 0 100) (tim 0 5 0 200) "a")
                 (T 2 (tim 0 5 0 300) (tim 0 5 0 400) "b")
                 (tim 0 0 0 200)
                 (T 3 (tim 0 5 0 500) (tim 0 5 0 600) "c")))


(def. (subtitles-items.cut-overlaps [list-of-subtitles-item? l])
  ;; ^ Force full check always, but use |subtitles-items?| for cheap dispatch. Ok?
  "In each T, cut the end time to the start of the next T if they
overlap."
  (let lp ((l l)
           (out '()))
    (let (end (& (reverse out)))
      (if-let-pair
       ((a l*) l)
       (let (unchanged (& (lp l* (cons a out))))
         (cond ((T/location? a)
                (if-let (b (find T/location? l*))
                        (let ((ato (.to a))
                              (bfrom (.from b)))
                          (if (.> ato bfrom)
                              (lp l* (cons (.to-set a bfrom) out))
                              (unchanged)))
                        (unchanged)))
               (else
                (unchanged))))
       (end)))))

(TEST
 > (def v (list (T 3 (tim 0 5 0 100) (tim 0 5 0 350) "a")
                (T 4 (tim 0 5 0 300) (tim 0 5 0 550) "b")
                ;; still runs since subtitles-items? doesnt detect the following
                ;; entry, but this will prevent the cutting from
                ;; happening:
                (Tdelay 30)
                (T 5 (tim 0 5 0 500) (tim 0 5 0 600) "c")
                (T 7 (tim 0 5 0 580) (tim 0 5 0 590) "c")))
 > (%try (=> v .cut-overlaps subtitles-show))
 (exception text: "l does not match list-of-subtitles-item?:\n([(T/location) [\"/home/chrisjazz/Alien-subtitles/lib/subtitles-transformatio...\n")
 > (=> v .subtitles-items .cut-overlaps subtitles-show)
 (subtitles:list (T 3 (tim 0 5 0 100) (tim 0 5 0 300) "a")
                 (T 4 (tim 0 5 0 300) (tim 0 5 0 530) "b")
                 (T 5 (tim 0 5 0 530) (tim 0 5 0 610) "c")
                 (T 7 (tim 0 5 0 610) (tim 0 5 0 620) "c")))



;; XX lib?

(def. (string.parentized? str)
  (let* ((str (string-trim-right str))
         ;; ^ (optim: string-trim-right is evil, not only does it copy on
         ;;   no-change, but it goes via lists.)
         (len (string.length str)))
    (and (>= len 2)
         (eq? (string.ref str 0) #\()
         (eq? (string.ref str (dec len)) #\)))))

(TEST
 > (.parentized? "()")
 #t
 > (.parentized? "(")
 #f
 > (.parentized? "((")
 #f
 > (.parentized? "(]")
 #f
 > (.parentized? "[]")
 #f
 > (.parentized? "(13")
 #f
 > (.parentized? "(13)")
 #t
 > (.parentized? "a(13) ")
 #f
 > (.parentized? "(13) ")
 #t
 > (.parentized? "(13) 4")
 #f)


(def. (subtitles-items.drop-parentized l)
  (.filter l (complement (=>* .titles .parentized?))))


(def. (subtitles-directives.map-Ts l fn)
  "Map T?s in l via fn, other values are left untouched."
  (.map l (lambda (v)
            (if (T? v)
                (fn v)
                v))))
