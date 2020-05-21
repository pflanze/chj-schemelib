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

(export (class T-config)
        (class subtitles-directive
          (class Tdelay)
          (class subtitles-time
            (class subtitles-milliseconds)
            (class tim)
            (class tm))
          (interface subtitles-item
            (class T-meta
              (class Tcomment)
              (class Twrap))
            (class T/location
              (class T/config/location)
              (class Toff/location)
              (class Treal/location))))
        (parameter current-tm-delay)
        Td ;; alias for Tdelay
        (macros T T/config Toff Treal)
        T? T/config? Toff? Treal?

        (methods subtitles-directives.subtitles-items
                 string/location-stream.Result-of-subtitles-items
                 string/locations.Result-of-subtitles-items
                 path-or-port-settings.Result-of-subtitles-items
                 Tshow
                 path-or-port-settings.srt->scheme
                 path-or-port-settingss.srt->scheme
                 subtitles-directives.display
                 subtitles-directives.string
                 subtitles-directives.save-to!)
        list-of-subtitles-item?
        subtitles-items?

        string/location.tim
        
        #!optional
        srtlines->Result-of-subtitles-items
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


(defclass (T-config #!key [boolean? keep-length-unchanged?])
  "Configuration for T/config objects.")


(defparameter current-tm-delay -500) ;; milliseconds

(def (subtitles:time->milliseconds time) -> fixnum?
     "`time` is understood to be in milliseconds if it's a bare real
number. Otherwise must support `.milliseconds` method."
     (cond ((fixnum? time) time)
           ((real? time) (integer time))
           (else (.milliseconds time))))


;; Placed here only since it can't fit inside `defclass
;; subtitles-directive` (interface can't be nested within class; XX
;; TODO: make possible syntactically?)
(definterface subtitles-item-interface
  (method (display s port [boolean? latin1?]))
  (method (+ s ms))
  ;; Place here because .adjust-scale in subtitles-transformations is
  ;; checking for subtitles-item; and don't want to make one more
  ;; subcategory? (And subtitles-item would be the wrong name if it
  ;; didn't contain these?):
  (method (from-update s fn))
  (method (to-update s fn)))


(defclass subtitles-directive
  (defmethod (subtitles-directive-unwrap s) s)

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

      (defmethod (+ s time)
        (let (ms (subtitles:time->milliseconds time))
          (subtitles-milliseconds (+ milliseconds ms)))))
  
  
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
                   (error ($ "number must be <= $padn digits long:") s))))
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
      (defmethod tm-seconds-real
        (=>* .tm .seconds-real))

      (defmethod (subtitles-milliseconds s)
        (subtitles-milliseconds (.milliseconds s)))
      (defmethod (tim s) s)
      (defmethod (tm s)
        (milliseconds->tm (.milliseconds s)))

      ;; (defmethod +
      ;;   (comp// 2 milliseconds->tim (on tim.milliseconds +)))
      ;; ehr

      (defmethod (+ s time)
        (let (ms (subtitles:time->milliseconds time))
          (milliseconds->tim (+ (tim.milliseconds s) ms)))))
  

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

      (defmethod (seconds-real v)
        (inexact seconds))

      (defmethod (subtitles-milliseconds s)
        (subtitles-milliseconds (.milliseconds s)))
      (defmethod (tim s)
        (milliseconds->tim (.milliseconds s)))
      (defmethod (tm s) s)

      (defmethod (+ s time)
        (let (ms (subtitles:time->milliseconds time))
          (tm (+ seconds (/ ms 1000)))))))


  (defclass (Tdelay [fixnum? milliseconds]))


  (defclass subtitles-item
    implements: subtitles-item-interface

    (defclass T-meta
      (defmethod (display s port [boolean? latin1?]) (void))
      (defmethod (+ s ms) s)
      (defmethod (from-update s fn) s)
      (defmethod (to-update s fn) s)

      (defclass (Tcomment [string? comment]))

      (defclass (Twrap [subtitles-directive? subtitles-directive-unwrap])
        "An inactivated subtitles-directive"))

    (defclass (T/location [(maybe location?) maybe-location]
                          [(maybe fixnum?) no]
                          [subtitles-time? from]
                          [subtitles-time? to]
                          [string? titles])
      "A subtitle entry"

      (defmacro (@T/location.from-set! s val) `(##vector-set! ,s 3 ,val))
      (defmacro (@T/location.to-set! s val) `(##vector-set! ,s 4 ,val))
      
      (defmethod (from-to-set s from to)
        (let (s* (vector-copy s))
          (@T/location.from-set! s* from)
          (@T/location.to-set! s* to)
          s*))

      (defmethod (from-to-update s f)
        (let (s* (vector-copy s))
          (@T/location.from-set! s* (f from))
          (@T/location.to-set! s* (f to))
          s*))
      
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

      (defmethod (+ s time)
        (let (ms (subtitles:time->milliseconds time))
          (if (zero? ms)
              s
              (=> s
                  (.from-to-set (.+ from ms) (.+ to ms))))))


      (defclass (T/config/location [T-config? config])

        (defmethod (from-to-update s f)
          (if (@T-config.keep-length-unchanged? config)
              (let (from* (f from))
                (.from-to-set s from* (.+ to (.- from* from))))
              ;; call super class method
              (T/location.from-to-update s f))))

      (defclass (Toff/location)
        "A subtitle entry that's disabled"
        (defmethod (display s port [boolean? latin1?])
          (void)))

      (defclass (Treal/location)
        "A `T` holding real time stamps, matched to the video, not to be shifted."
        (defmethod (+ s time)
          s)))))


(def Td Tdelay)

(defmacro (T . args) `(T/location ',(maybe-source-location stx) ,@args))
(defmacro (T/config . args) `(T/config/location ',(maybe-source-location stx) ,@args))
(defmacro (Toff . args) `(Toff/location ',(maybe-source-location stx) ,@args))
(defmacro (Treal . args) `(Treal/location ',(maybe-source-location stx) ,@args))

(def T? T/location?)
(def T/config? T/config/location?)
(def Toff? Toff/location?)
(def Treal? Treal/location?)


;; for tests, see subtitles-test.scm


;; so hacky...  show for subtitles.scm

(def. (any.subtitles-show v)
  (.show v subtitles-show))

(def. (list.subtitles-show v)
  `(subtitles:list ,@(map subtitles-show v)))

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

(def (srtlines->Result-of-subtitles-items lines)
     -> (istream-of (Result-of subtitles-item? error?))
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



(def subtitles-directives? (ilist-of subtitles-directive?))

(def list-of-subtitles-item? (list-of subtitles-item?))
(def subtitles-items? (ilist-of subtitles-item?))

(def. (subtitles-directives.subtitles-items l) -> list-of-subtitles-item?
  "'Clean up' `subtitles-directive`s to just `T`s, taking subtitle-time elements
as shift points in the time line (time is shifted from there on, not
scaled)."
  (let rec ((l l)
            (dt-ms 0))
    (if-let-pair
     ((a l*) l)

     (xcond ((subtitles-item? a) (cons (.+ a dt-ms) (rec l* dt-ms)))
            ((subtitles-time? a)
             ;; use a instead of .from time of the next element, from
             ;; then on
             (if-let-pair
              ((b l**) l*)
              (begin
                (assert (subtitles-item? b))
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
(def. string/location-stream.Result-of-subtitles-items
  (=>* srtlines->Result-of-subtitles-items
       Results.Result))

(def string/locations? (ilist-of string/location?))
(def. string/locations.Result-of-subtitles-items string/location-stream.Result-of-subtitles-items)

(def. (path-or-port-settings.Result-of-subtitles-items pps)
  "Convert an `.srt` file to Scheme."
  (let (RTs (=>* file-line/location-stream
                 .Result-of-subtitles-items))
    (catching-encoding-error
     (& (RTs pps))
     "from user-specified encoding to UTF-16"
     (& (catching-encoding-error
         (& (RTs (.encoding-set pps 'UTF-16)))
         "from UTF-16 to ISO-8859-1"
         (& (RTs (.encoding-set pps 'ISO-8859-1))))))))

(def Tshow (=>* .Result-of-subtitles-items subtitles-show))


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
  (if-Ok (.Result-of-subtitles-items pps)
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

(def. (subtitles-directives.display [(list-of subtitles-directive?) items]
                         #!optional
                         ([(maybe output-port?) p] (current-output-port))
                         latin1?)
  (=> items .subtitles-items (.for-each (C .display _ p latin1?))))

(def. (subtitles-directives.string items)
  (call-with-output-string "" (C .display items _)))

(def. (subtitles-directives.save-to! [(list-of subtitles-directive?) items]
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
 (Ok (subtitles:list
      (T 3 (tim 0 6 54 144) (tim 0 6 56 847)
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
                                    (subtitles-show (.subtitles-items v))))))))
 (Ok #t)
 > (equal? v2 (=> s (string-split "\n") Tshow))
 #t)

