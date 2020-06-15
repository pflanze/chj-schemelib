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
        (methods string.delete-parentized chars.delete-parentized)

        (class delete-names-config)
        default-delete-names-config
        (methods string.delete-names chars.delete-names)

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

(defmacro (def.-string-charlist-proxy/optional
            arity
            [(source-of symbol?) toname]
            [(source-of symbol?) fromname])
  ;;(assert* fixnum-natural0? arity)
  (let-pair ((arg0 args)
             (map (lambda (i) (gensym)) (iota (-> fixnum-natural? (eval arity)))))
            `(def. (,toname ,arg0 #!optional ,@args)
               (docstring-from ,fromname)
               (=> ,arg0
                   string.list
                   (,fromname ,@args)
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
config. Nested parens are properly matched.

See also `subtitles-items.drop-parentized`."
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


;; .delete-names should probably care about parens, too, so, the
;; proper solution would be to make a single full parser for the
;; subtitle texts.

(defclass (delete-names-config [function? name-char?]
                               [boolean? no-dash-at-line-start?]
                               [string? end-of-sentence-chars]
                               [boolean? allow-space-in-names?]))

(def default-delete-names-config
     (delete-names-config char-alpha-international?
                          #f
                          ".?!\n"
                          #f))

;; how are these "names" called?
(def. (chars.delete-names cs #!optional maybe-config)
  "Delete/replace names of people who say something."
  (with.
   delete-names-config
   (or maybe-config
       default-delete-names-config)

   (let ((end-of-sentence-char? (char-one-of end-of-sentence-chars))
         (char-dash? (lambda (c) (char=? c #\-))))

     (let rec ((cs cs)
               (line-start? #t)
               (sentence-start? #t)
               (after-dash? #f))
       (if-let-pair
        ((c cs*) cs)
        (cond
         ((char-newline? c)
          (cons c (rec cs*
                       #t
                       (or sentence-start?
                           (end-of-sentence-char? c))
                       #f)))
         ((char-dash? c)
          ;; dash is ignored for start of sentence and line detection
          (cons c (rec cs*
                       line-start?
                       sentence-start?
                       #t)))
         ((end-of-sentence-char? c)
          (cons c (rec cs*
                       (char-newline? c)
                       #t
                       #f)))
         ((char-whitespace? c)
          ;; ^ XX really only space and tab; form feed and such would
          ;; count as sentence and line start?
          (cons c (rec cs*
                       line-start?
                       sentence-start?
                       after-dash?)))
         ((and sentence-start? (name-char? c))
          (let lp-namechars ((cs cs*)
                             (res (cons c '()))
                             (last-was-space? #f))
            (if-let-pair
             ((c cs*) cs)
             (cond
              ((and (char=? c #\:) (not last-was-space?))
               ;; yep, name, delete/replace it
               (let (rem (rec cs* #f #f #f))
                 (cond (after-dash?
                        (drop-while char-whitespace? rem))
                       ((or 
                         (and no-dash-at-line-start? line-start?))
                        rem)
                       (else
                        (cons #\- rem)))))
              ((name-char? c)
               (lp-namechars cs* (cons c res) #f))
              ((and allow-space-in-names? (char-whitespace? c) (not last-was-space?))
               (lp-namechars cs* (cons c res) #t))
              (else
               ;; not a name
               (append-reverse res 
                               (rec cs
                                    (char-newline? c)
                                    (end-of-sentence-char? c)
                                    (char-dash? c)))))
             (reverse res))))
         (else
          (cons c (rec cs*
                       (char-newline? c)
                       (end-of-sentence-char? c)
                       (char-dash? c)))))
        '())))))


(def.-string-charlist-proxy/optional 2 string.delete-names chars.delete-names)

(TEST
 > (.delete-names "FOO: I did this. Bar: And I did that: something.")
 "- I did this. - And I did that: something."
 > (def c (=> default-delete-names-config
              (.name-char?-set char-alpha-uc?)))
 > (.delete-names "FOO: I did this. Bar: And I did that. BAZ: And I was lazy." c)
 "- I did this. Bar: And I did that. - And I was lazy."

 > (.delete-names "- I like that.\n- FOO: Me too.")
 "- I like that.\n- Me too."
 > (.delete-names "- Foo: yes!\n- Bar:no. -Baz: sure!")
 "- yes!\n- no. -sure!" ;; OK with the "-sure"?

 > (.delete-names "FOO:All good?")
 "-All good?"
 > (.delete-names "FOO:\nAll good?")
 "-\nAll good?"
 ;; This (currently) interacts with string-newlines in a (literally?)
 ;; 'interesting' way: (but is really a bug, todo fix?)
 > (def nlo (strip-newlines-options #t #f))
 > (.subtitle-strip-newlines (.delete-names "FOO:\nAll good?") nlo)
 "All good?"
 > (.subtitle-strip-newlines (.delete-names "FOO: All good?") nlo)
 "- All good?"

 ;; Non-ascii languages:
 > (.delete-names "- Das ist fein.\n- MÄDCHEN: Nein!")
 "- Das ist fein.\n- Nein!"

 ;; Names with space: default is off
 > (.delete-names "WALLACE AND GROMMIT:\nHi!")
 "WALLACE AND GROMMIT:\nHi!"
 ;; Turn it on (you may want to restrict to uppercase only, too):
 > (def c+ (=> c (.allow-space-in-names?-set #t)))
 > (.delete-names "WALLACE AND GROMMIT:\nHi!" c+)
 "-\nHi!"
 > (.delete-names "A B C D: FOO. D E F: G!" c+)
 "- FOO. - G!"
 > (.delete-names "A B\nC D: FOO. D E F: G!" c+)
 "- FOO. - G!"
 > (.delete-names "A B\nc D: FOO. D E F  G!" c+)
 "A B\nc D: FOO. D E F  G!"
 > (.delete-names "A B\nC D: FOO. D E F  G!" c+)
 "- FOO. D E F  G!"
 )



;; === subtitles:list -- easy time syntax =========================

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

