;;; Copyright 2016-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy-2
         (cj-env-2 future)
         (cj-io-util open-process*
                     dirname+basename
                     directory-item-stream)
         (cj-path path-string?)
         (Result Ok Error)
         monad/syntax
         port-settings
         container
         test)

(export (jclass command)
        process-run
        string-writer
        string-reader
        file-contents
        glob-match
        glob
        Result:read-all-source
        Result:read-all
        Result:call-with-input-file
        Result:call-with-output-file
        (methods location.read-all-source
                 location.read-all
                 location.expr-source
                 location.expr)
        #!optional
        process-spec?)

(include "cj-standarddeclares.scm")

"Newer, better, higher-level IO routines (than the cj-io-util ~mess).

Includes Result based I/O, and methods for different source/sink
specifications than path-or-port-settings (e.g. location)."


(def (process-spec? v)
     (or (null? v)
         (and (pair? v)
              (let-pair ((a v*) v)
                        (and (keyword? a)
                             (pair? v*)
                             (let-pair ((b v**) v*)
                                       (and (or (symbol? b)
                                                (string? b))
                                            (process-spec? v**))))))))

(TEST
 > (process-spec? '())
 #t
 > (process-spec? '(a))
 #f
 > (process-spec? '(a:))
 #f
 > (process-spec? '(a: "a"))
 #t
 > (process-spec? '("a:" "a"))
 #f
 > (process-spec? '(a: "a" b: b))
 #t)


(jclass (command [path-string? path]
                 #!key
                 ([process-spec? additional-spec] '())
                 #!rest
                 [(list-of string?) arguments])

        (def-method (process-spec c . args)
          `(path: ,(.path c)
                  arguments: ,(.arguments c)
                  ;; XX do the following two need merging?
                  ,@(.additional-spec c)
                  ,@args)))

(TEST
 > (command "a" additional-spec: '(foo: "a") "b")
 #((command) "a" (foo: "a") ("b"))
 > (command "a" "b")
 #((command) "a" () ("b")))


(def (process-run cmd reader writer ok?)
     (let* ((p (open-process* (.process-spec cmd
                                             stdin-redirection: #t
                                             stdout-redirection: #t)))
            (f (future (writer p)))
            (res (reader p)))
       (close-input-port p)
       (thread-join! f) ;; should exceptions be suppressed here?
       (let ((s (process-status p)))
         (if (ok? s)
             (Ok res)
             (Error s)))))

(def (string-writer #(string? s))
     (lambda (p)
       (display s p)
       (close-output-port p)))

(def string-reader
     (lambda (p)
       (read-line p #f)))

(TEST
 > (process-run (command "tr" "a" "b")
                string-reader
                (string-writer "Hallo")
                zero?)
 #((Ok) "Hbllo")
 > (process-run (command "tr" "a" "b")
                string-reader
                (string-writer "Hallo")
                (complement zero?))
 #((Error) 0)
 ;; BTW status codes should have their own type, too, right. Then we
 ;; would see it perfectly well-informed here, too !
 )

(def (file-contents #(path-string? path))
     (call-with-input-file path string-reader))


(def (glob-match [string? pattern]) -> function?
     "shell style pattern language except only supports '*' for now"
     (let (parts (string-split pattern #\*))
       (case (length parts)
         ((1) (C string=? _ pattern))
         (else
          (let-pair ((p0 parts*) parts)
                    (let ((plast (last parts*))
                          (pbetween (butlast parts*)))
                      (if (pair? pbetween)
                          (error "only supporting 1 * for now" pattern)
                          (lambda (str)
                            (and (string-starts-with? str p0)
                                 (string-ends-with? str plast)
                                 ;; difficult complexity
                                 ;; (let lp ((parts parts*))
                                 ;;   )
                                 )))))))))

(TEST
 > ((glob-match "foo") "foo")
 #t
 > ((glob-match "foo") "foo ")
 #f
 > ((glob-match "foo*bar") "foo ")
 #f
 > ((glob-match "foo*bar") "foo bar")
 #t
 > ((glob-match "foo*bar") "foobar")
 #t
 > ((glob-match "foo*bar") "fobar")
 #f
 > ((glob-match "foo*bar") " foobar")
 #f
 > ((glob-match "foo*bar") "foobar ")
 #f)


(def (glob [string? pattern])
     "shell style pattern language except only supports '*' for now"
     (letv ((dirpath filepattern) (dirname+basename pattern))
           (if (string-contains? dirpath "*")
               (error "patterns in directories not supported yet" pattern)
               (=>> (stream-filter (glob-match filepattern)
                                   (directory-item-stream dirpath))
                    (stream-map (lambda (item)
                                  (path-append dirpath item)))))))



(def (make-Result:read-all* read-all)
     (lambda ([input-port? port]) -> Result?
        ;; Should we capture IO exceptions? That would really be unusual
        ;; though and perhaps better signalled as exceptions rather than
        ;; cancellable errors. So only do the return part.
        (return (read-all port))))

(def Result:read-all-source (make-Result:read-all* read-all-source))
(def Result:read-all (make-Result:read-all* read-all))

(def (make-Result:call-with-*-file open-*-file)
     (lambda ([path-or-port-settings? pps] [procedure? op]) -> Result?
        (mlet ((port
                (with-exception-catcher
                 Error
                 (& (Ok (open-*-file pps)))))
               (res
                (op port))
               (closeres
                (with-exception-catcher
                 Error
                 (& (Ok (close-port port))))))
              (return res))))

(def Result:call-with-input-file
     (make-Result:call-with-*-file open-input-file))
(def Result:call-with-output-file
     (make-Result:call-with-*-file open-output-file))



(def (make-location->read-all-source Result-read-all)
     (lambda ([location? loc]) -> (Result-of ilist? any?)
        "Read all exprs from the source file containing loc."
        (let (c (location-container loc))
          (if (path-string? c)
              (Result:call-with-input-file c Result-read-all)
              (Error `("location container is not a path string" ,c))))))

(def. location.read-all-source
  (make-location->read-all-source Result:read-all-source))
(def. location.read-all
  (make-location->read-all-source Result:read-all))


(def. (location.expr-source loc) -> (Result-of source? any?)
  "Retrieve expr at loc from the source file, with location
information. (Careful, slow.)"
  (mlet ((l (.read-all-source loc)))
        ;; Deep find; really deep for-each, short-cut when found.
        (call/cc
         (lambda (exit)
           (let find-in ((v l))
             (cond ((source? v)
                    (let (loc2 (source-location v))
                      ;; Need a `location.equal?` that normalizes
                      ;; paths? Yes would, in case compiled objects
                      ;; were moved. BUT, location-container is
                      ;; already guaranteed to be the same (XX really?
                      ;; Compilation units can have a mix of location
                      ;; information, but that doesn't matter; so, I
                      ;; think that yes?) Thus just check position.
                      (if ((on location-position =) loc loc2)
                          (exit (Ok v))
                          (find-in (source-code v)))))
                   ((container? v)
                    (.for-each-item v find-in))
                   (else
                    (when #f
                      (warn "ignoring:" v)))))
           (Error `("location not found in backing file" ,loc))))))

(def. (location.expr loc) -> (Result-of any? any?)
  "Retrieve expr at loc from the source file, without location
information. (Careful, slow.)"
  (>>= (.expr-source loc)
       (lambda (src)
         (return (cj-desourcify src)))))



