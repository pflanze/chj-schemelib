;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         subtitles
         html-remove
         )

(export (methods strings.save-to!)
        T-no-add
        i
        (methods chars.subtitle-strip-newlines
                 string.subtitle-strip-newlines)

        (class delete-parentized-config)
        (methods chars.delete-parentized string.delete-parentized)

        (generic .subtitle-item)
        bare->subtitle-items
        (macro subtitles:list)
        #!optional
        strings?)

(include "cj-standarddeclares.scm")


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
;; BTW:
;;  .list -> .char-list or .chars ?
;;  char-list.string -> chars.string ?

;;/lib




(def strings? (ilist-of string?))
(def. (strings.save-to! vs [path-or-port-settings? pops] #!optional encoding)
  ;; ignore encoding, OK? Always want it in default (UTF-8) for this.
  (=> vs
      (.map (=>* string.remove-html-markup
                 (string-split char-whitespace?)
                 (.filter (complement string-empty?))))
      flatten1
      ((lambda (ss)
         (call-with-output-file pops
           (lambda (p)
             (.for-each ss (C displayln _ p))))))))


(def (T-no-add n)
     (lambda (v)
       (if (subtitles-item? v)
           (=> v
               (.no-update (C + _ n)))
           v)))

(def subtitles-utils#i
     (=>* (string.split #\newline)
          (.map (lambda (line)
                  (if (string.null? (trim-both line))
                      line
                      ($ "<i>${line}</i>"))))
          (strings-join "\n")))

(TEST
 > (##namespace ("subtitles-utils#" i))
 > (i "fun")
 "<i>fun</i>"
 > (i "fun \n")
 "<i>fun </i>\n"
 > (i "fun \nThere")
 "<i>fun </i>\n<i>There</i>")


(defclass (strip-newlines-options [boolean? nbsp-after-minus?]
                                  [boolean? strip-all-newlines?])
  "Options for .subtitle-strip-newlines:

nbsp-after-minus?: whether to replace spaces between a `-` and the
next non-space character with non-breaking spaces.

strip-all-newlines?: whether to strip all newline characters, even
those left of a `-`.
")

(def (chars.maybe-first-char-after l pred)
     (if-let-pair ((a r) (drop-while pred l))
                  a
                  #f))

(def. (chars.subtitle-strip-newlines l [strip-newlines-options? options])
  "Strip unnecessary newlines from a subtitle segment string."
  ;; good that I don't keep them split up?
  (let.-static
   (strip-newlines-options. (nbsp-after-minus?
                             strip-all-newlines?) options)

   (reverse
    (let lp ((l l)
             (prev-space? #f) ;; (maybe char?)
             (r '()))
      (def (process- l r [(maybe char?) prev-space?])
           "process part after #\\- character, which is to be handled
as either a hyphen (removed), the start of another person
talking (space afterwards replaced by nbsp), a lone minus at the end
of a line (left alone), or a 'connection hyphen' in \"a-b\" (left
alone)."
           (if (eq? (chars.maybe-first-char-after l (C eq? _ #\space))
                    #\newline)
               (if prev-space?
                   ;; "a -\n..." or "a\n-\n...", lone minus at end of
                   ;; line, keep:
                   (lp l #f (cons #\- r))
                   ;; "a- \n...": hyphenation, remove
                   (lp (drop-while char-whitespace? l)
                       #f
                       ;; drop '-':
                       r))

               ;; "a-b" or "a- b" or "a - b" or "a\n- b"
               (let (r* (cons #\- r))
                 (if prev-space?
                     ;; "a - b" or "a\n- b", start of another person talking
                     (if nbsp-after-minus?
                         (let lp- ((l l)
                                   (r r*))
                           (if-let-pair
                            ((a l*) l)
                            (case a
                              ((#\space)
                               (lp- l* (cons nbsp r)))
                              ((#\newline)
                               ;; should only happen in other parent
                               ;; branch
                               (error "BUG"))
                              (else
                               ;; non-whitespace after /-\s*/
                               (lp l #f r)))
                            ;; EOF
                            r))
                         (lp l prev-space? r*))

                     ;; "a-b" or "a- b" (never "a-\nb"), leave as is
                     (lp l #f r*)))))
      (if-let-pair
       ((a l*) l)
       (case a
         ((#\-)
          ;; "-" or "a-" or " -" or "\n-" (could be a hyphen)
          (process- l* r prev-space?))
         ((#\newline)
          (if-let-pair
           ((b l**) l*)
           (case b
             ((#\-)
              ;; "\n-"
              (process- l**
                        (cons (if strip-all-newlines?
                                  #\space
                                  a)
                              r)
                        #\newline))
             (else
              ;; replace newline with a space, unless previous
              ;; character was a space already
              (lp l*
                  ;; consider newline a space regardless of
                  ;; whether it was replaced by a space? XX make a
                  ;; whitespace flag, fully?
                  #t
                  (if prev-space? r (cons #\space r)))))
           ;; EOF
           (cons a r)))
         (else
          (lp l*
              (and (char-whitespace? a) a)
              (cons a r))))
       ;; EOF
       r)))))

(def.-string-charlist-proxy 2 string.subtitle-strip-newlines
  chars.subtitle-strip-newlines)



(TEST
 > (def t (C .subtitle-strip-newlines _ (strip-newlines-options #t #f)))
 > (def t2 (C .subtitle-strip-newlines _ (strip-newlines-options #t #t)))
 > (t "a b")
 "a b"
 > (t "a\nb")
 "a b"
 > (t "a \nb")
 "a b"
 > (t "a \n b")
 "a  b"
 > (t "\na\n")
 ;; should it remove the \n to the left?
 " a\n"
 > (t "\n\na\n\n")
 " a \n"
 > (t "\nder uns  sagen\nkonnte, \ndass die Früchte verdorben waren.\n")
 " der uns  sagen konnte, dass die Früchte verdorben waren.\n"
 > (t "Kriege ich den Schlüssel \nvon\nIhnen, Sir?\n- Ja,\nich werde ihn übergeben. -  Gut so!")
 "Kriege ich den Schlüssel von Ihnen, Sir?\n- Ja, ich werde ihn übergeben. -  Gut so!"
 > (t2 "Kriege ich den Schlüssel \nvon\nIhnen, Sir?\n- Ja,\nich werde ihn übergeben. -  Gut so!")
 "Kriege ich den Schlüssel von Ihnen, Sir? - Ja, ich werde ihn übergeben. -  Gut so!"
 > (t "a-\nb")
 "ab"
 > (t2 "a\n-\nb")
 "a - b"
 > (t2 "a\n-b")
 "a -b"
 > (t2 "a\n- b")
 "a - b"
 > (t2 "a- \nb")
 "ab"
 ;; ^ still understood to be hyphenation; same here:
 > (t "a- \nb")
 "ab"
 > (t "a-b-c")
 "a-b-c"
 > (t2 "a-b-c")
 "a-b-c"
 > (t "a- b- c")
 "a- b- c"
 > (t2 "a- b- c")
 "a- b- c")


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






(def. (real.subtitle-item v loc)
  (tm v))

(def. (string.subtitle-item str loc)
  (let (len (string-length str))
    (or (>= len 3)
        (raise-location-error loc "too short"))
    (if (string.starts-with? str "A:")
        (cond ((string.maybe-number (substring str 2 len)) => tm)
              (else (raise-location-error loc "not a number after \"A:\"" str)))
        (if-Ok (string/location.tim str)
               it
               (raise-location-error
                loc "does not start with \"A:\" and doesn't parse as srt time"
                str it)))))

(def. (symbol.subtitle-item v loc)
  (=> v
      symbol.string
      (string.subtitle-item loc)))

(def. (subtitles-directive.subtitle-item v loc) v)

(def. (ilist.subtitle-item v loc) v)

(TEST
 > (def l (location "foo" (position 10 11)))
 > (TRY (.subtitle-item 'foo l))
 (location-error (location "foo" (position 10 11))
                 "does not start with \"A:\" and doesn't parse as srt time"
                 (list "foo" (make-source-error "foo" "need exactly one comma" (list))))
 > (TRY (.subtitle-item 'A:foo l))
 (location-error (location "foo" (position 10 11)) "not a number after \"A:\"" (list "A:foo"))
 > (.subtitle-item 'A:12 l)
 [(tm) 12]
 > (.subtitle-item 'A:12.56 l)
 [(tm) 12.56]
 > (TRY (.subtitle-item 'B:12.56 l))
 (location-error (location "foo" (position 10 11))
                 "does not start with \"A:\" and doesn't parse as srt time"
                 (list "B:12.56" (make-source-error "B:12.56" "need exactly one comma" (list))))
 > (.subtitle-item 12.56 l)
 [(tm) 12.56]
 > (.subtitle-item (Tcomment "") l)
 [(Tcomment) ""])


(def (bare->subtitle-items l)
     "Convert bare real numbers as well as '|A:1234.5| style variants
into |tm| objects"
     (.map l (lambda (v)
               (.subtitle-item (source-code v) (maybe-source-location v)))))


(def (subtitles:_list . items)
     (bare->subtitle-items items))


(defmacro (subtitles:list . items)
  "Like |list| but auto-quotes symbols that start with `A:` and then
passes the items through `bare->subtitle-items`."
  `(subtitles:_list
    ,@(map (lambda (item)
             ;; Could use .subtitle-item here, too, to report location
             ;; information while it is still available (but would
             ;; have to be careful to only apply it to literals!); or,
             ;; could quote-source strings as well. Do the latter.
             (mcase item
                    (symbol?
                     (if (=> item source-code symbol.string
                             (string.starts-with? "A:"))
                         `(quote-source ,item)
                         item))
                    (string?
                     `(quote-source ,item))
                    (else
                     item)))
           items)))

(TEST
 > (subtitles:list A:123 1255 (tm 4) (+ 1 2))
 ([(tm) 123] [(tm) 1255] [(tm) 4] [(tm) 3]))

