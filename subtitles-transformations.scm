;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         subtitles
         Maybe
         monad/syntax
         (html-remove string.remove-html-markup)
         ;; for .adjust-scale
         math/least-squares
         ;; for .interpolate:
         wbtable
         math/interpolate
         (oo-util string.findpos string.rfindpos) ;; in easy anyway?
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
        
        (class delete-parentized-config)
        (methods chars.delete-parentized string.delete-parentized)

        #!optional
        string.parentized?)

(include "cj-standarddeclares.scm")

"Extra functionality: transformations on subtitles"


;;XX lib
(defmacro (docstring-from fnname)
  ;; Add some kind of (also human readable?) marker to let future
  ;; docstring retrieval tool retrieve docstring from fnname at
  ;; runtime (presumably):
  (assert* symbol? fnname
           (lambda (fnname)
             ($ "look up docstring of: $fnname"))))

(defmacro (def.-string-charlist-proxy
            arity
            [(source-of symbol?) toname]
            [(source-of symbol?) fromname])
  ;;(assert* fixnum-natural0? arity)
  (let (args (map (lambda (i) (gensym)) (iota (-> fixnum-natural? (eval arity)))))
    `(def. (,toname ,@args)
       (docstring-from ,fromname)
       (=> ,(first args)
           string.list
           (,fromname ,@(rest args))
           char-list.string))))

;;/lib



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
                           (.from-to-update f)
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
                              (if (.> bfrom (.from a))
                                  (lp l* (cons (.to-set a bfrom) out))
                                  (raise-location-error
                                   (.maybe-location b)
                                   "T starts on or before the start of the previous T"
                                   b))
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

(def. (string.parentized? str) -> (maybe true?) ;; heh, for the `maybe` monad
  "Whether str is a (possibly empty) string enclosed in parens and
optionally whitespace."
  (let (len (string.length str))
    (mlet ((start (string.findpos str (complement char-whitespace?)))
           (end (string.rfindpos str (complement char-whitespace?))))
          ;; end is the index of the last character, not the len of
          ;; the slice
          (let lp ((i start)
                   (level 0))
            (if (<= i end)
                (let ((c (string.ref str i))
                      (lp/level (C lp (inc i) _)))
                  (if (eq? c #\()
                      (lp/level (inc level))
                      (if (= i start)
                          #f
                          (if (eq? c #\))
                              (let (level (dec level))
                                (if (positive? level)
                                    (lp/level level)
                                    (= i end)))
                              (lp/level level)))))
                (zero? level))))))

(TEST
 > (.parentized? "")
 #f
 > (.parentized? " \n ")
 #f
 > (.parentized? "()")
 #t
 > (.parentized? " (x) \n ")
 #t
 > (.parentized? " \n\r (\n) \n  \r\t ")
 #t
 > (.parentized? "(")
 #f
 > (.parentized? ")")
 #f
 > (.parentized? "x)")
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
 > (.parentized? "(13)a")
 #f
 > (.parentized? "a(13) ")
 #f
 > (.parentized? " ( (13) ")
 #f
 > (.parentized? " ( (13) ) ")
 #t
 > (.parentized? "  (13) ) ")
 #f
 > (.parentized? " x( (13) ) ")
 #f
 > (.parentized? "(13) 4")
 #f
 > (.parentized? "(13) (4)")
 #f
 > (.parentized? "(13)(4)")
 #f
 > (.parentized? "(13()4)")
 #t)


(defclass (delete-parentized-config [function? char-match-pred]
                                    [chars? replacement]
                                    [function? drop-after-pred])
  "`char-match-pred` must return #t for all characters allowed in
parentized groups that are to be replaced by `replacement`. After a
replaced group, further characters are dropped as long as
`drop-after-pred` is true.")

(def. (chars.delete-parentized cs [delete-parentized-config? config])
  "Replace subsequences wrapped in parens according to the
config. Nested parens are properly matched."
  (let.-static
   (delete-parentized-config.
    (char-match-pred replacement drop-after-pred) config)
   (let rec ((cs cs))
     (if-let-pair
      ((c cs*) cs)
      (case c
        ((#\()
         (let lp ((cs cs*)
                  (level 1)
                  (skipped '(#\()))
           (if-let-pair
            ((c cs*) cs)
            (let (skipped* (cons c skipped))
              (case c
                ((#\()
                 (lp cs* (inc level) skipped*))
                ((#\))
                 (let (level* (dec level))
                   (if (zero? level*)
                       (append replacement
                               (rec (.drop-while cs* drop-after-pred)))
                       (lp cs* level* skipped*))))
                (else
                 (if (char-match-pred c)
                     (lp cs* level skipped*)
                     ;; not a group
                     (rappend skipped (rec cs))))))
            ;;XX use continuation-capture for proper 'placement'? and
            ;;is it still faster then b wl ?
            (error "unmatched paren"))))
        (else
         (cons c (rec cs*))))
      '()))))

(def.-string-charlist-proxy 2 string.delete-parentized chars.delete-parentized)

(TEST
 > (def c1 (delete-parentized-config any? (.list "yo") false/1))
 > (def c2 (=> c1 (.drop-after-pred-set char-space?)))
 > (.delete-parentized "(Hi) there!" c1)
 "yo there!"
 > (.delete-parentized "(Hi) there!" c2)
 "yothere!"
 > (def c3 (=> c2 (.replacement-set '())))
 > (.delete-parentized "(Hi) there!" c3)
 "there!"
 > (.delete-parentized "(Hi) there! (too) yes." c3)
 "there! yes."
 > (def c4 (=> c1 (.replacement-set '(#\-))))
 > (.delete-parentized "(Hi) there! (too) yes." c4)
 "- there! - yes."
 > (.delete-parentized "(Hi there! too) yes, (yes)." c4)
 "- yes, -."
 > (.delete-parentized "(Hi (there!) too) yes, (yes)." c4)
 "- yes, -."
 > (TRY (.delete-parentized "(Hi (there! too) yes, (yes)." c4))
 (error-exception "unmatched paren" (list))
 > (def c5 (=> c4 (.char-match-pred-set char-alpha?)))
 > (.delete-parentized "(Hi there! too) yes, (yes)." c5)
 "(Hi there! too) yes, -.")




(def. (subtitles-items.drop-parentized l)
  (.filter l (complement (=>* .titles
                              string.remove-html-markup
                              string.parentized?))))


(def. (subtitles-directives.map-Ts l fn)
  "Map T?s in l via fn, other values are left untouched."
  (.map l (lambda (v)
            (if (T? v)
                (fn v)
                v))))
