;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         template
         Result
         monad/lib-for-Result
         monad/syntax
         string-util-1 ;; still used?
         (latin1 latin1-string?)
         (string-util-2 string-tr)
         (cj-gambit-sys-io os-exception-codesymbol)
         port-settings
         (cj-port pretty-string)
         test)

(export (class subtitles-time
          (class subtitles-milliseconds)
          (class tim)
          (class tm))
        (parameter current-tm-delay)
        (class Tdelay)
        Td ;; alias for Tdelay
        (interface T-interface
          (class Tcomment)
          (class T/location
            (class Toff/location)
            (class Treal/location)))
        (macros T Toff Treal)
        (methods srt-items.Ts
                 string/location-stream.Result-of-Ts
                 string/locations.Result-of-Ts
                 path-or-port-settings.Result-of-Ts
                 Tshow
                 path-or-port-settings.srt->scheme
                 path-or-port-settingss.srt->scheme
                 srt-items.display
                 srt-items.string
                 srt-items.save-to!)
        list-of-T?
        Ts?
        #!optional
        srtlines->Result-of-Ts
        ;; util stuff:
        Result:string->number
        Result:string-of-len->number)

(include "cj-standarddeclares.scm")


"Parse and print video subtitle files in the SubRip `.srt` Format."


(def (catching-encoding-error thunk triedmsg then)
     (with-exception-catcher
      (lambda (e)
        (if (os-exception? e)
            (case (os-exception-codesymbol e)
              ((cannot-convert-from-utf-8
                cannot-convert-from-c-char-string)
               ;; want 'carp'?...
               (warn ";; catching-encoding-error: falling back"
                     triedmsg)
               (then))
              (else
               (raise e)))
            (raise e)))
      thunk))

;; lib lib lib!



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

(defparameter current-tm-delay -500) ;; milliseconds

(defclass subtitles-time

  (defmethod -ms
    (on .milliseconds -))

  (defmethod -
    (comp// 2 subtitles-milliseconds (on .milliseconds -)))

  ;; could optimize those
  (defmethod < (on .milliseconds <))
  (defmethod > (on .milliseconds >))
  (defmethod <= (on .milliseconds <=))
  (defmethod >= (on .milliseconds >=))
  (defmethod = (on .milliseconds =))

  (defmethod (display s port)
    (.display (.tim s) port))


  (defclass (subtitles-milliseconds [fixnum? milliseconds])

    (defmethod (subtitles-milliseconds s) s)
    (defmethod (tim s)
      (milliseconds->tim milliseconds))
    (defmethod (tm s)
      (milliseconds->tm milliseconds))

    (defmethod (+ s [fixnum? ms])
      (subtitles-milliseconds (+ milliseconds ms))))
  
  
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

    (defmethod (subtitles-milliseconds s)
      (subtitles-milliseconds (.milliseconds s)))
    (defmethod (tim s) s)
    (defmethod (tm s)
      (milliseconds->tm (.milliseconds s)))

    ;; (defmethod +
    ;;   (comp// 2 milliseconds->tim (on tim.milliseconds +)))
    ;; ehr

    (defmethod (+ s [fixnum? milliseconds])
      (milliseconds->tim (+ (tim.milliseconds s) milliseconds))))
  

  (defclass (tm [real? seconds])
    "|tm| is meant to be used with mplayer from the command line, to
copy-paste the positions mplayer is writing to the terminal. To try to
make it easy to simply copy paste after stopping upon hearing the
start of an utterance, the value in |current-tm-delay| is added to get
the actual time value used for positioning the subtitle."

    (def (milliseconds->tm [fixnum? milliseconds])
         (tm (/ (- milliseconds (-> fixnum? (current-tm-delay)))
                1000)))
    
    (defmethod (milliseconds s)
      (+ (integer (if (exact? seconds)
                      (+ 1/2 (* seconds 1000))
                      (+ 0.5 (* seconds 1000.))))
         (-> fixnum? (current-tm-delay))))

    (defmethod (subtitles-milliseconds s)
      (subtitles-milliseconds (.milliseconds s)))
    (defmethod (tim s)
      (milliseconds->tim (.milliseconds s)))
    (defmethod (tm s) s)

    (defmethod (+ s [fixnum? ms])
      (tm (+ seconds (/ ms 1000))))))



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
 > (.tim (.- (tim 1 10 5 0) (tim 1 10 3 0)))
 [(tim) 0 0 2 0]
 > (.tim (.- (tim 1 10 5 0) (tim 1 10 3 100)))
 [(tim) 0 0 1 900]
 > (.+ (tim 1 10 5 0) 10)
 [(tim) 1 10 5 10]
 > (.+ (tim 1 10 5 0) 1000)
 [(tim) 1 10 6 0]
 > (.+ (tim 1 10 5 0) -1000)
 [(tim) 1 10 4 0]

 > (.< (tim 0 1 8 862) (tim 0 1 8 862))
 #f
 > (.< (tim 0 1 8 861) (tim 0 1 8 862))
 #t
 > (.< (tim 0 1 6 862) (tim 0 1 8 862))
 #t
 > (.> (tim 0 1 8 862) (tim 0 1 8 862))
 #f
 > (.= (tim 0 1 8 862) (tim 0 1 8 862))
 #t
 > (.>= (tim 0 1 8 862) (tim 0 1 8 862))
 #t)

(TEST
 > (def old-current-tm-delay (current-tm-delay))
 > (current-tm-delay -100)
 > (tm 60.53)
 [(tm) 60.53]
 > (.milliseconds #)
 60430
 > (.- (tm 60.53) (tm 1))
 [(subtitles-milliseconds) 59530]
 > (.- (tm 60.53) (tm 1.00001))
 [(subtitles-milliseconds) 59530]
 ;; conversions
 > (.tm (tm 334.9))
 [(tm) 334.9]
 > (.tim (tm 334.9))
 [(tim) 0 5 34 800]
 > (.subtitles-milliseconds (tm 334.9))
 [(subtitles-milliseconds) 334800]
 > (.tim #)
 [(tim) 0 5 34 800]
 > (.tm #)
 [(tm) 3349/10]
 > (current-tm-delay old-current-tm-delay))


(TEST
 > (show (string/location.tim "Hi"))
 (Error (make-source-error "Hi" "need exactly one comma" (list)))
 > (show (string/location.tim "Hi,23"))
 (Error (make-source-error "Hi" "need two ':' characters in hh:mm:ss" (list)))
 > (show (string/location.tim (source "Hi,23" (location '(f) (position 1 1)))))
 (Error
  (make-source-error
   (source* "Hi" (list 'f) 1 1)
   "need two ':' characters in hh:mm:ss" (list)))
 > (show (string/location.tim
           (source "10:bb:c,f23" (location '(f) (position 1 1)))))
 (Error (make-source-error (source* "bb" (list 'f) 1 4)
                           "not a number" (list)))
 > (show (string/location.tim
           (source "10:13:22,f23" (location '(f) (position 1 1)))))
 (Error (make-source-error (source* "f23" (list 'f) 1 10)
                           "not a number" (list)))
 > (show (string/location.tim "01:52:47,670"))
 (Ok (tim 1 52 47 670))

 ;; faulty formatting detection:
 > (show (string/location.tim "01:30:44,200"))
 (Ok (tim 1 30 44 200))
 > (show (string/location.tim "01:30:44,2"))
 (Error (make-source-error "2" "expecting string of length" (list 3)))
 > (show (string/location.tim "01:3:44,200"))
 (Error (make-source-error "3" "expecting string of length" (list 2))))


(defclass (Tdelay [fixnum? milliseconds]))

(def Td Tdelay)


(definterface T-interface
  (method (display s port))
  (method (add-ms s ms))
  

  (defclass (Tcomment [string? comment])

    (defmethod (display s port)
      (void))

    (defmethod (add-ms s ms)
      s))

  (defclass (T/location [(maybe location?) maybe-location]
                        [(maybe fixnum?) no]
                        [subtitles-time? from]
                        [subtitles-time? to]
                        [string? titles])
    "A subtitle entry"

    ;; XX should instead fix class stuff so that |to| can be
    ;; restricted in terms of from directly.
    (defmethod (xcheck s)
      (unless (.< from to)
        (raise-location-error maybe-location "from is not before to" from to)))
    
    (defmethod (display s port [boolean? latin1?])
      (.xcheck s)
      (let (titles
            (if latin1?
                (let (titles (string-tr titles "’„“‚‘" "'\"\"''"))
                  (if (latin1-string? titles)
                      titles
                      (raise-location-error
                       maybe-location "not latin-1" titles)))
                titles))
        (displayln no port)
        (.display from port)
        (display " --> " port)
        (.display to port)
        (newline port)
        (displayln (chomp titles) port)
        (newline port)))

    (defmethod (add-ms s [real? ms])
      (if (zero? ms)
          s
          (=> s
              (.from-set (.+ from ms))
              (.to-set (.+ to ms)))))


    (defclass (Toff/location)
      "A subtitle entry that's disabled"
      (defmethod (display s port)
        (void)))
    (defclass (Treal/location)
      "A `T` holding real time stamps, matched to the video, not to be shifted."
      (defmethod (add-ms s [real? ms])
        s))))

(defmacro (T . args) `(T/location ',(maybe-source-location stx) ,@args))
(defmacro (Toff . args) `(Toff/location ',(maybe-source-location stx) ,@args))
(defmacro (Treal . args) `(Treal/location ',(maybe-source-location stx) ,@args))

;; so hacky...  show for subtitles.scm

(def. (any.subtitles-show v)
  (.show v subtitles-show))

(def. (T/location.subtitles-show v)
  (let-pair ((n r) (show v))
            (cons (=> n
                      symbol.string
                      (string.replace-substring "/location" "")
                      string.symbol)
                  (rest r))))

(def (subtitles-show v)
     (.subtitles-show v))


(def string/location-all-whitespace?
     (comp string-all-whitespace? source-code))


;; (Could take improper list approach in error case, tail being the
;; Error; but, try to be "proper", 'Rust conforming' or something.)

(def (srtlines->Result-of-Ts lines)
     -> (istream-of (Result-of T-interface? error?))
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



(def srt-item? (either T-interface?
                       ;; for new positioning:
                       subtitles-time?
                       Tdelay?))

(def srt-items? (ilist-of srt-item?))

(def list-of-T? (list-of T-interface?))
(def Ts? (ilist-of T-interface?))

(def. (srt-items.Ts l) -> list-of-T?
  "'Clean up' `srt-item`s to just `T`s, taking subtitle-time elements
as shift points in the time line (time is shifted from there on, not
scaled)."
  (let rec ((l l)
            (dt-ms 0))
    (if-let-pair
     ((a l*) l)

     (xcond ((T-interface? a) (cons (.add-ms a dt-ms) (rec l* dt-ms)))
            ((subtitles-time? a)
             ;; use a instead of .from time of the next element, from
             ;; then on
             (if-let-pair
              ((b l**) l*)
              (begin
                (assert (T-interface? b))
                (rec l*
                     (.-ms a (.from b))))
              (error "missing T after subtitles-time")))
            ((Tdelay? a)
             (rec l*
                  (+ dt-ms (.milliseconds a)))))
     
     '())))


(def filepath? (both path-string? -f?))

(def. filepath.lines/location file-line/location-stream)

(def string/location-stream? (istream-of string/location?))
(def. string/location-stream.Result-of-Ts
  (=>* srtlines->Result-of-Ts
       Results.Result))

(def string/locations? (ilist-of string/location?))
(def. string/locations.Result-of-Ts string/location-stream.Result-of-Ts)

(def. (path-or-port-settings.Result-of-Ts pps)
  "Convert an `.srt` file to Scheme."
  (let (RTs (=>* file-line/location-stream
                 .Result-of-Ts))
    (catching-encoding-error
     (& (RTs pps))
     "from user-specified encoding to UTF-16"
     (& (catching-encoding-error
         (& (RTs (.encoding-set pps 'UTF-16)))
         "from UTF-16 to ISO-8859-1"
         (& (RTs (.encoding-set pps 'ISO-8859-1))))))))

(def Tshow (=>* .Result-of-Ts subtitles-show))


;; XX lib ?

(def ellipsis (box '...))
;; ^ hmm, should I call it |...|, so that (list 0 1 ...) actually,
;; well, still not works? What about the unicode ellipsis?
(def ellipsis? (C eq? _ ellipsis))
(def. (ellipsis.show v show)
  '...)

(def show-prepare:maxlen 5)

(def (show-prepare v)
     (if (list? v)
         (if (length-> v show-prepare:maxlen)
             (=> v
                 (take show-prepare:maxlen)
                 (append (list ellipsis)))
             v)
         v))

(TEST
 > (=> (iota 10) show-prepare show)
 (list 0 1 2 3 4 ...))


(def. (path-or-port-settings.srt->scheme pps #!optional [(maybe exact-natural0?) i])
  ;; commented file name
  (display ";; ") (writeln (.path-string pps))
  (if-Ok (.Result-of-Ts pps)
         (pretty-print `(def ,(if i (symbol-append 'v (.string (inc i))) 'v)
                             ,(subtitles-show it)))
         ;; errors:
         (=> it
             show-prepare
             try-show
             ;; comment out:
             pretty-string
             (.split "\n")
             (.map (C string-append ";; " _))
             (.for-each displayln)))
  (newline))

(def path-or-port-settingss? (iseq-of path-or-port-settings?))

(def. (path-or-port-settingss.srt->scheme ppss)
  (.for-each/iota ppss .srt->scheme))


;; -----------

(def. (srt-items.display [(list-of srt-item?) items]
                         #!optional
                         ([(maybe output-port?) p] (current-output-port))
                         latin1?)
  (=> items .Ts (.for-each (C .display _ p latin1?))))

(def. (srt-items.string items)
  (call-with-output-string "" (C .display items _)))

(def. (srt-items.save-to! [(list-of srt-item?) items]
                          [path-or-port-settings? ps]
                          #!optional
                          [(maybe gambit-encoding?) encoding])
  "Convert srt objects to a `.srt` file. If `ps` declares latin-1
encoding, does some substitutions and if it still fails, reports the
offending object. If `final?` is #t, "
  (let* ((ps (.encoding-set-default ps 'UTF-16))
         (ps (if encoding
                 (.encoding-set ps encoding)
                 ps)))
    (let (latin1? (in-monad maybe
                            (==> (.maybe-encoding ps)
                                 maybe-canonical-gambit-encoding
                                 (eq? 'latin1))))
      (call-with-output-file ps
        (lambda (p)
          ;; (display #\xFEFF p) ehr. Not working for smplayer.
          (.display items p latin1?))))))



(TEST
 > (def v (list
           (T 3 (tim 0 6 54 144) (tim 0 6 56 847)
              "A b c d?")
           (Tdelay 1001)
           (T 4 (tim 0 6 57 14) (tim 0 6 59 136)
              "E f g h.")
           (tim 0 7 0 321)
           (T 5 (tim 0 7 1 812) (tim 0 7 3 589)
              "I j k.")
           (T 7 (tim 0 7 3 591) (tim 0 7 6 657)
              "- L m, n?\n- O.")))
 > (def s (.string v))
 > s ;; use .display for proper view in the repl instead!
 "3
00:06:54,144 --> 00:06:56,847
A b c d?

4
00:06:58,015 --> 00:07:00,137
E f g h.

5
00:07:00,321 --> 00:07:02,098
I j k.

7
00:07:02,100 --> 00:07:05,166
- L m, n?
- O.

"
 ;; ^ Note that it is (currently) keeping the numbering unchanged;
 ;;   this may help when going forth and back between SubRip and
 ;;   Scheme representation.
 > (def v2 (=> (source s (location '(f) (position 1 1)))
               (string-split/location "\n")
               Tshow))
 > v2
 (Ok (list (T 3 (tim 0 6 54 144) (tim 0 6 56 847)
              "A b c d?")
           (T 4 (tim 0 6 58 15) (tim 0 7 0 137)
              "E f g h.")
           (T 5 (tim 0 7 0 321) (tim 0 7 2 98)
              "I j k.")
           (T 7 (tim 0 7 2 100) (tim 0 7 5 166)
              "- L m, n?\n- O.")))
 > (show (-> Result?
             (>>= (eval v2)
                  (lambda (v*)
                    ;; need to |subtitles-show| here to ditch the
                    ;; differing location information
                    (return (equal? (subtitles-show v*)
                                    (subtitles-show (.Ts v))))))))
 (Ok #t)
 > (equal? v2 (=> s (string-split "\n") .Tshow))
 #t)

