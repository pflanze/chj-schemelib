;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require cj-typed
         dsssl
         dot-oo
         (cj-functional-2 either)
         (cj-env-2 xcond)
         (cj-path path-string?))

(export port-setting?
        path-or-port-settings?
        gambit-encoding?
        (methods-for .encoding-set)
        (methods-for .maybe-encoding))


(define (port-settings? v)
  (and (list? v)
       (dsssl? v)
       ;; XX test for particular set of keywords etc?
       #t))

(TEST
 > (port-settings? '(path: "foo" char-encoding: iso-8859-1))
 #t)


(define path-or-port-settings?
  (either path-string? port-settings?))


(define (gambit-encoding? v)
  (case v
    ;; XX what are the values exactly?, look up in Gambit source or decide
    ((UTF-8 utf-8 utf8 UTF8) #t)
    ((iso-8859-1 ISO-8859-1) #t)
    (else #f)))


(define. (path-string.encoding-set s [gambit-encoding? encoding])
  (list path: s
        char-encoding: encoding))

(define. (port-settings.encoding-set s [gambit-encoding? encoding])
  (dsssl-set s char-encoding: encoding))


(define. (path-string.maybe-encoding s)
  #f)

(define. (port-settings.maybe-encoding s)
  (dsssl-maybe-ref s char-encoding:))


(define. (path-string.delete-encoding s)
  s)

(define. (port-settings.delete-encoding s)
  (dsssl-delete s char-encoding:))


(TEST
 > (.encoding-set "foo" 'iso-8859-1)
 (path: "foo" char-encoding: iso-8859-1)
 > (.encoding-set # 'iso-8859-1)
 (char-encoding: iso-8859-1 path: "foo")
 > (.encoding-set # 'UTF-8)
 (char-encoding: UTF-8 path: "foo")
 > (.delete-encoding #)
 (path: "foo"))

