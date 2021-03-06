;;; Copyright 2016-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         eol
         csv-defaults
         (predicates length-=)
         jclass
         stream
         oo-lib-vector
         (string-util-3 string.replace-substrings)
         error
         (spreadsheet-reference (class spreadsheet-reference-absolute))
         (cj-path FILE)
         (cj-io-util dirname))

(export (class read-csv-error)
        (class csv-cell)
        csv-cell-of
        possibly-csv-cell-of
        possibly-csv-cell.value ;; *not* a method
        x-csv-cell-of
        X-csv-cell-of ;; allows unwrapped inputs, too (if match predicate)
        (method csv-cell.xvalue-of)
        ;; The main use:
        csv-file-stream
        csv-port-stream
        ;; Heavy OO:
        (class csv-reader)
        (interface input-provider
                   (class file-input-provider)))


;; type error reporting
(defclass (csv-type-error maybe-nested-error ;; force |error?| here instead of BUG msg below?
                          [csv-cell? cell])
  implements: error-interface

  (defmethod (string s)
    (string-append
     (if maybe-nested-error
         (if (error? maybe-nested-error)
             (string-append (.string maybe-nested-error)
                            " ")
             "(BUG while reporting: invalid type of nested error) ")
         "")
     "at "
     (.error-string cell))))


;;XX this should be in some interface file, once useful methods were
;;added; also, confusion with exception/continuation. Also, implements
;;error ? Also, rename error to error-interface ?
(defclass (error/continuation [continuation? continuation]))

;; read/parse error reporting
(defclass (read-csv-error [(either string? port?) path-or-port]
                          [fixnum-natural0? line]
                          ;; error_diag values from the perl library
                          [fixnum-natural0? cde]
                          [string? message]
                          [(maybe fixnum-natural0?) column])
  extends: error/continuation
  implements: error-interface ;; <- put this into error/continuation class?

  ;; XX .string is old code, instead use .location now internally?
  ;; And/or introduce a global protocol for errors which are not just
  ;; location but the whole thing? Well, that's what the error
  ;; protocol already should be (see note above).
  (defmethod (string s)
    ($ (string.replace-substrings message "QUO character" "quote character")
       " in "
       (object->string (if (port? path-or-port)
                           (.name path-or-port)
                           path-or-port))
       " line "
       line
       (if column
           ($ " pos $column")
           "")))
  
  (defmethod (location s)
    (location (if (port? path-or-port)
                  (.name path-or-port)
                  path-or-port)
              (position line column)))

  (defmethod (csv-type-error s maybe-nested-error)
    (csv-type-error maybe-nested-error)))


;; location tracking

(defclass (csv-cell [(maybe string?) value]
                    [(either string? port?) path-or-port]
                    [fixnum-natural? rowno]
                    [fixnum-natural? colno])

  (defmethod (error-string s)
    ($ "row "
       rowno
       " col "
       colno
       " ("
       (.formula-string-fragment (spreadsheet-reference-absolute #f rowno colno))
       ") in file "
       (object->string path-or-port))))


;; evil naming to use a dot if it's not a method; sigh, but I don't
;; want to have to change more than prefix possibly- to change the
;; usage case.
(def (possibly-csv-cell.value v)
     (if (csv-cell? v)
         (@csv-cell.value v)
         v))

(def ((csv-cell-of pred) v)
     (if (csv-cell? v)
         (let ((w (pred (@csv-cell.value v))))
           (if (eq? w #t)
               #t
               (csv-type-error w v)))
         #f))

(def ((possibly-csv-cell-of pred) v)
     (if (csv-cell? v)
         (let ((w (pred (@csv-cell.value v))))
           (if (eq? w #t)
               #t
               (csv-type-error w v)))
         (pred v)))


(def (@x-csv-cell-of v pred msg)
     (let* ((val (@csv-cell.value v))
            (w (pred val)))
       (if (eq? w #t)
           val
           (error ($ (if msg ($ msg ": ") "")
                     "expecting a "
                     ;; XX oh, ()almost?) need macro for this,
                     ;; too?
                     (object->string (try-show pred))
                     " "
                     ;; XX show actual value? consistency?
                     (csv-type-error w v))))))

;; could be a method but then order of arguments would be wrong and
;; dunno?; (Should this be a macro to tell the expression like
;; cj-typed does? No, right?)
(def (x-csv-cell-of v pred #!optional msg)
     (if (csv-cell? v)
         (@x-csv-cell-of v pred msg)
         (error "not a csv-cell:" v)))

;; and then, still, too?
(def. csv-cell.xvalue-of x-csv-cell-of)

;; Variant that allows unwrapped values, too:
(def (X-csv-cell-of v pred #!optional msg)
     (if (csv-cell? v)
         (@x-csv-cell-of v pred msg)
         ;; (-> pred v) no, since it's not a macro now we have to do
         ;; runtime:
         (let* ((val v)
                (w (pred val)))
           (if (eq? w #t)
               val
               (error ($ (if msg ($ msg ": ") "")
                         "expecting a "
                         ;; XX oh, ()almost?) need macro for this,
                         ;; too?
                         (object->string (try-show pred))
                         " "
                         ;; XX show actual value? consistency?
                         (if w
                             (.string w)
                             "")))))))



(TEST
 > (def c (csv-cell "hi" "foo.csv" 1039 4))
 > (.error-string c)
 "row 1039 col 4 (D1039) in file \"foo.csv\""
 > ((csv-cell-of string?) c)
 #t
 > (show ((csv-cell-of nothing?) c))
 (csv-type-error #f (csv-cell "hi" "foo.csv" 1039 4))
 > (x-csv-cell-of c string?)
 "hi"
 > (%try (x-csv-cell-of c symbol?))
 (exception
  text:
  "expecting a symbol? at row 1039 col 4 (D1039) in file \"foo.csv\"\n")
 > (%try (x-csv-cell-of c number? "expecting the number of beers"))
 (exception
  text:
  "expecting the number of beers: expecting a number? at row 1039 col 4 (D1039) in file \"foo.csv\"\n")
 > (%try (X-csv-cell-of c number? "expecting the number of beers"))
 (exception
  text:
  "expecting the number of beers: expecting a number? at row 1039 col 4 (D1039) in file \"foo.csv\"\n")
 > (%try (X-csv-cell-of "foo" number? "expecting the number of beers"))
 (exception text: "expecting the number of beers: expecting a number? \n")
 > (%try (X-csv-cell-of "foo" number?))
 (exception text: "expecting a number? \n")
 > (X-csv-cell-of 123 number? "expecting the number of beers")
 123
 )





(def (_csv-port-stream port
                       maybe-file-or-port
                       source?
                       #!optional (tail '()))
     (let lp ((rowno 1))
       (delay
         (let ((line (read-line port)))
           (if (eof-object? line)
               (begin
                 (close-port port)
                 (assert (zero? (process-status port)))
                 tail)
               (let ((vals-or-signal
                      (xone (call-with-input-string line read-all)))
                     (rest (lp (inc rowno))))
                 (xcond
                  ((and (vector? vals-or-signal)
                        (> (vector-length vals-or-signal) 0))
                   (let ((signal vals-or-signal))
                     (xcase (vector-ref signal 0)
                            ((OK)
                             (if (null? (force rest))
                                 tail
                                 (error "read-csv bug: did get OK signal before end of output")))
                            ((ERROR)
                             (assert (= (vector-length signal) 6))
                             (continuation-capture
                              (lambda (cont)
                                (read-csv-error cont
                                                ;; path-or-port:
                                                ;;(vector-ref signal 1)
                                                ;; ^ OK re SECURITY? alternative:
                                                ;; The above is just "-", use this:
                                                (or maybe-file-or-port port)
                                                ;; lineno:
                                                (vector-ref signal 2)
                                                ;; cde:
                                                (vector-ref signal 3)
                                                ;; message:
                                                (vector-ref signal 4)
                                                ;; maybe pos:
                                                (vector-ref signal 5))))))))
                  ((ilist? vals-or-signal)
                   (let ((vals vals-or-signal))
                     (cons (if (and source? maybe-file-or-port)
                               (map/iota (lambda (val colno)
                                           (csv-cell val
                                                     maybe-file-or-port
                                                     rowno
                                                     (inc colno)))
                                         vals)
                               vals)
                           rest))))))))))


(def (csv-file-stream path
                      #!key
                      ([char? sep-char] (current-csv-input-sep-char))
                      ([eol-name? eol] (current-csv-input-eol))
                      (tail '())
                      source?)
     (_csv-port-stream (open-input-process
                        (list path: "lib/csv2sexpr"
                              arguments: (list path
                                               "-"
                                               (string sep-char)
                                               (symbol.string eol))
                              char-encoding: 'UTF-8))
                       path
                       source?
                       tail))


;;XX lib (and lostontie?)
(def (send-file-strings inport outport)
     ;; XX well.
     (let ((str (read-line inport #f)))
       (display str outport)))


(def (csv-port-stream port
                      #!key
                      ([char? sep-char] (current-csv-input-sep-char))
                      ([eol-name? eol] (current-csv-input-eol))
                      (tail '())
                      maybe-source
                      (source? #t))
     (let ((p (open-process
               (list path: (path-append (dirname (FILE))
                                        "csv2sexpr")
                     arguments: (list "-"
                                      "-"
                                      (string sep-char)
                                      (symbol.string eol))
                     ;; WOW, would really need different encoding for
                     ;; input vs. output. If I wanted to process bytes
                     ;; inbetween; breaking down so much. But yeah,
                     ;; char-encoding when opening orig file is done,
                     ;; then we have strings, we write them here as
                     ;; UTF-8 and read them back as the same, so,
                     ;; actually fine here. "send-file-strings kinda"
                     ;; does the transcoding.
                     char-encoding: 'UTF-8
                     stdout-redirection: #t
                     stdin-redirection: #t))))
       (future
        (let lp ()
          (send-file-strings port p)
          (close-output-port p)
          ;; XX btw TODO: check status, don't even do that in csv-file-stream!
          (close-port port)))
       (_csv-port-stream p
                         (or maybe-source port)
                         (and maybe-source source?)
                         tail)))



;; XX add more
(def char-encodings '(UTF-8))

(def (char-encoding? v)
     (and (memq v char-encodings) #t))


(jinterface input-provider

            (method (open) -> input-port?)

            (jclass (file-input-provider [path-string? path-string]
                                         [char-encoding? char-encoding])

                    (def-method (open s)
                      (open-input-file (list path: path-string
                                             char-encoding: char-encoding)))

                    ;; (def-method (close s port)
                    ;;   (close-port port)) unused
                    ))


;;XX lib
(def (filter/snd vals keep?s)
     (if (or (null? vals) (null? keep?s))
         '()
         (let-pair ((v vals) vals)
                   (let-pair ((k? keep?s) keep?s)
                             (let ((r (filter/snd vals keep?s)))
                               (if k?
                                   (cons v r)
                                   r))))))

(TEST
 > (filter/snd '(a b c) '(#f #t #f))
 (b)
 > (filter/snd '(a b c) '(#f #t #t))
 (b c)
 > (filter/snd '(a b c) '(#f #t))
 (b)
 > (filter/snd '(a b c) '(#f))
 ()
 > (filter/snd '(a b c) '(#t))
 (a)
 ;; XX hmm what about these, really??:
 > (filter/snd '(a b c) '(#f #t #t #f))
 (b c)
 > (filter/snd '(a b c) '(#f #t #t #t))
 (b c))


(jclass (csv-reader [input-provider? input-provider]
                    #!key
                    [char? sep-char]
                    [eol-name? eol]
                    [(maybe natural0?) maybe-head-skip]
                    [boolean? skip-last?]
                    [(maybe (list-of natural0?)) maybe-columns])

        (def-method (stream s)
          (let* ((s (csv-port-stream (.open input-provider)
                                     sep-char: sep-char
                                     eol: eol))
                 (s (if maybe-head-skip
                        (stream-drop s maybe-head-skip)
                        s))
                 (s (if skip-last?
                        (stream-butlast s)
                        s))
                 (s (if maybe-columns
                        (stream-map (lambda (row)
                                      (map (let ((row* (list.vector row)))
                                             (lambda (i)
                                               (vector.ref row* i)))
                                           maybe-columns))
                                    s)
                        s)))
            s)))

