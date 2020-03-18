;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         template
         Result
         monad/lib-for-Result
         (string-util-1 position-update-in-string))

(export filepath.Tshow
        (class tim)
        (class T)
        (methods srt-items.Ts
                 string-stream.Tshow
                 filepath.Tshow
                 srt-items.display
                 srt-items.string
                 srt-items.save-to!)
        #!optional
        srtlines->Result-of-Ts
        ;; util stuff:
        Result:string->number
        Result:string-of-len->number)

(include "cj-standarddeclares.scm")


"Parse and print video subtitle files in the SubRip `.srt` Format."


;; Sep. lib? How much of it?

(def (Result:string->number str/location)
     (if-let (x (string->number (source-code str/location)))
             (Ok x)
             (Error-source str/location "not a number")))

(def (Result:string-of-len->number str/location len)
     (let* ((str (source-code str/location))
            (strlen (string-length str)))
       (if (= strlen len)
           (if-let (x (string->number str))
                   (Ok x)
                   (Error-source str/location "not a number"))
           (Error-source str/location "expecting string of length" len))))


(def (parse-hms str/location)
     (let ((ss (string-split/location str/location #\:)))
       (if (= (length ss) 3)
           (Result:mapM (C Result:string-of-len->number _ 2) ss)
           (Error-source str/location
                         "need two ':' characters in hh:mm:ss"))))


(def 0..59? (both fixnum? (C <= 0 _ 59)))
(def 0..999? (both fixnum? (C <= 0 _ 999)))

(def string/location? (possibly-source-of string?))

(defclass (tim [natural0? hours-part]
               [0..59? minutes-part]
               [0..59? seconds-part]
               [0..999? milliseconds-part])

  (def (string/location.tim v) -> (Result-of tim? error?)
       (let (commaparts (string-split/location v #\,))
         (if (= (length commaparts) 2)
             (let-list
              ((hmsstr msstr) commaparts)
              (mlet ((hms (parse-hms hmsstr))
                     (ms (Result:string-of-len->number msstr 3)))
                    (let-list ((h m s) hms)
                              (return (tim h m s ms)))))
             (Error-source v "need exactly one comma"))))
  
  (def (milliseconds->tim [fixnum-natural0? ms])
       (let*-values (((sec msp) (quotient+modulo ms 1000))
                     ((min secp) (quotient+modulo sec 60))
                     ((hours minp) (quotient+modulo min 60)))
         (tim hours
              minp
              secp
              msp)))

  (defmethod (display s port)
    (def (pr n padn)
         (let* ((s (number.string n))
                (len (string.length s)))
           (if (<= len padn)
               (begin
                 (for..< (i len padn)
                         (display #\0 port))
                 (display s port))
               (error "number must be 1 or 2 digits:" s))))
    (pr hours-part 2)
    (display #\: port)
    (pr minutes-part 2)
    (display #\: port)
    (pr seconds-part 2)
    (display #\, port)
    (pr milliseconds-part 3))

  (defmethod (milliseconds s)
    (+ milliseconds-part
       (* 1000 (+ seconds-part
                  (* 60 (+ minutes-part
                           (* 60 hours-part)))))))

  (defmethod -ms
    (on tim.milliseconds -))

  (defmethod -
    (comp// 2 milliseconds->tim (on tim.milliseconds -)))

  ;; (defmethod +
  ;;   (comp// 2 milliseconds->tim (on tim.milliseconds +)))
  ;; ehr

  (defmethod (+ s [fixnum? milliseconds])
    (milliseconds->tim (+ (tim.milliseconds s) milliseconds))))



(TEST
 > (milliseconds->tim 1)
 [(tim) 0 0 0 1]
 > (milliseconds->tim 999)
 [(tim) 0 0 0 999]
 > (milliseconds->tim 1000)
 [(tim) 0 0 1 0]
 > (def ms (+ (* 63 1000) 7))
 > ms
 63007
 > (milliseconds->tim ms)
 [(tim) 0 1 3 7]
 > (.milliseconds #)
 63007
 > (tim 1 30 7 500)
 [(tim) 1 30 7 500]
 > (.milliseconds #)
 5407500
 > (milliseconds->tim #)
 [(tim) 1 30 7 500]
 > (.- (tim 1 10 5 0) (tim 1 10 3 0))
 [(tim) 0 0 2 0]
 > (.- (tim 1 10 5 0) (tim 1 10 3 100))
 [(tim) 0 0 1 900]
 > (.+ (tim 1 10 5 0) 10)
 [(tim) 1 10 5 10]
 > (.+ (tim 1 10 5 0) 1000)
 [(tim) 1 10 6 0]
 > (.+ (tim 1 10 5 0) -1000)
 [(tim) 1 10 4 0])

(TEST
 > (.show (string/location.tim "Hi"))
 (Error (make-source-error "Hi" "need exactly one comma" (list)))
 > (.show (string/location.tim "Hi,23"))
 (Error (make-source-error "Hi" "need two ':' characters in hh:mm:ss" (list)))
 > (.show (string/location.tim (source "Hi,23" (location '(f) (position 1 1)))))
 (Error
  (make-source-error
   (source* "Hi" (list 'f) 1 1)
   "need two ':' characters in hh:mm:ss" (list)))
 > (.show (string/location.tim
           (source "10:bb:c,f23" (location '(f) (position 1 1)))))
 (Error (make-source-error (source* "bb" (list 'f) 1 4)
                           "not a number" (list)))
 > (.show (string/location.tim
           (source "10:13:22,f23" (location '(f) (position 1 1)))))
 (Error (make-source-error (source* "f23" (list 'f) 1 10)
                           "not a number" (list)))
 > (.show (string/location.tim "01:52:47,670"))
 (Ok (tim 1 52 47 670))

 ;; faulty formatting detection:
 > (.show (string/location.tim "01:30:44,200"))
 (Ok (tim 1 30 44 200))
 > (.show (string/location.tim "01:30:44,2"))
 (Error (make-source-error "2" "expecting string of length" (list 3)))
 > (.show (string/location.tim "01:3:44,200"))
 (Error (make-source-error "3" "expecting string of length" (list 2))))


(defclass (Tdelay [fixnum? milliseconds]))


(defclass (T [(maybe fixnum?) no]
             [tim? from]
             [tim? to]
             [string? titles])
  "A subtitle entry"

  (defmethod (display s port)
    (displayln no port)
    (.display from port)
    (display " --> " port)
    (.display to port)
    (newline port)
    (displayln (chomp titles) port)
    (newline port))

  (defmethod (add-ms s [real? ms])
    (if (zero? ms)
        s
        (=> s
            (.from-set (.+ from ms))
            (.to-set (.+ to ms)))))

  (defclass (Toff)
    "A subtitle entry that's disabled"
    (defmethod (display s port)
      (void)))
  (defclass (Treal)
    "A `T` holding real time stamps, matched to the video, not to be shifted."
    (defmethod (add-ms s [real? ms])
      s)))




(def string/location-all-whitespace?
     (comp string-all-whitespace? source-code))


;; (Could take improper list approach in error case, tail being the
;; Error; but, try to be "proper", 'Rust conforming' or something.)

(def (srtlines->Result-of-Ts lines)
     -> (istream-of (Result-of T? error?))
     (let rec ((lines lines))
       (delay
         (if-let-pair
          ((no-str lines) (force (.drop-while lines
                                              string/location-all-whitespace?)))
          (if-Ok
           (-> Result?
               (mlet
                (no (Result:string->number (source-code no-str)))
                (if (fixnum-natural0? no)
                    (if-let-pair
                     ((times-str lines) (force lines))
                     (let (timestrs
                           (string-split/location times-str "-->"))
                       (if (= (length timestrs) 2)
                           (mlet
                            (times* (Result:mapM (comp string/location.tim
                                                       trim-both)
                                                 timestrs))
                            (letv ((titles lines)
                                   (.take-while&rest
                                    lines
                                    (complement
                                     string/location-all-whitespace?)))
                                  (Ok (cons (Ok (T no
                                                   (first times*)
                                                   (second times*)
                                                   (strings-join
                                                    (map source-code titles)
                                                    "\n")))
                                            (rec lines)))))
                           (Error-source
                            times-str
                            "expecting line with exactly one \"-->\"")))
                     (Error-source-unfinished-after no-str))
                    (Error-source no-str "expecting a fixnum-natural0"))))

           ;; Ok case: list
           it
           ;; Error case: make list
           (cons it-Result
                 (rec (.drop-while lines
                                   (complement
                                    string/location-all-whitespace?)))))
          '()))))



(def srt-item? (either T?
                       ;; for new positioning:
                       tim?
                       Tdelay?))

(def srt-items? (ilist-of srt-item?))

(def. (srt-items.Ts l) -> (list-of T?)
  "'Clean up' `srt-item`s to just `T`s."
  (let rec ((l l)
            (dt-ms 0))
    (if-let-pair
     ((a l*) l)

     (xcond ((T? a) (cons (.add-ms a dt-ms) (rec l* dt-ms)))
            ((tim? a)
             ;; use a instead of .from time of the next element, from
             ;; then on
             (if-let-pair
              ((b l**) l*)
              (rec l*
                   (.-ms a (.from b)))
              '()))
            ((Tdelay? a)
             (rec l*
                  (+ dt-ms (.milliseconds a)))))
     
     '())))


(def filepath? (both path-string? -f?))

(def. filepath.lines/location file-line/location-stream)

(def string/location-stream? (istream-of string/location?))
(def. string/location-stream.Tshow
  (=>* srtlines->Result-of-Ts
       Results.Result
       .show))

(def string/locations? (ilist-of string/location?))
(def. string/locations.Tshow string/location-stream.Tshow)

(def. filepath.Tshow
  ;; "Convert an `.srt` file to Scheme." -- XX fix def. to allow docstrings
  (=>* file-line/location-stream
       .Tshow))

(def. (srt-items.display [(list-of srt-item?) items]
                         #!optional
                         ([(maybe output-port?) p] (current-output-port)))
  (=> items
      .Ts
      (.for-each (C .display _ p))))

(def. (srt-items.string items)
  (call-with-output-string "" (C .display items _)))

(def. (srt-items.save-to! [(list-of srt-item?) items] [path-settings? path])
  ;; "Convert Scheme to a `.srt` file" -- XX ditto
  (call-with-output-file path
    (lambda (p)
      ;; (display #\xFEFF p) ehr. Not working either for smplayer.
      (.display items p))))



(TEST
 > (def v (list
           (T 3 (tim 0 6 54 144) (tim 0 6 56 847)
              "Have any biscuits over there?")
           (Tdelay 1001)
           (T 4 (tim 0 6 57 14) (tim 0 6 59 136)
              "Here's some cornbread.")
           (tim 0 7 0 321)
           (T 5 (tim 0 7 1 812) (tim 0 7 3 589)
              "I am cold.")
           (T 7 (tim 0 7 3 591) (tim 0 7 6 657)
              "- Still with us, Brett?\n- Right.")))
 > (def s (.string v))
 > s ;; use .display for proper view in the repl instead!
 "3
00:06:54,144 --> 00:06:56,847
Have any biscuits over there?

4
00:06:58,015 --> 00:07:00,137
Here's some cornbread.

5
00:07:00,321 --> 00:07:02,098
I am cold.

7
00:07:02,100 --> 00:07:05,166
- Still with us, Brett?
- Right.

"
 ;; ^ Note that it is (currently) keeping the numbering unchanged;
 ;;   this may help when going forth and back between SubRip and
 ;;   Scheme representation.
 > (def v2 (=> (source s (location '(f) (position 1 1)))
               (string-split/location "\n")
               .Tshow))
 > v2
 (Ok (list (T 3 (tim 0 6 54 144) (tim 0 6 56 847)
              "Have any biscuits over there?")
           (T 4 (tim 0 6 58 15) (tim 0 7 0 137)
              "Here's some cornbread.")
           (T 5 (tim 0 7 0 321) (tim 0 7 2 98)
              "I am cold.")
           (T 7 (tim 0 7 2 100) (tim 0 7 5 166)
              "- Still with us, Brett?\n- Right.")))
 > (.show (-> Result? (>>= (eval v2) (lambda (v*) (return (equal? v* (.Ts v)))))))
 (Ok #t)
 > (equal? v2 (=> s (string-split "\n") .Tshow))
 #t)

