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
        path-or-port-settings.encoding-set
        path-or-port-settings.maybe-encoding)


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


(define. (path-or-port-settings.encoding-set s [gambit-encoding? encoding])
  (xcond ((path-string? s)
          (list path: s
                char-encoding: encoding))
         ((port-settings? s)
          (dsssl-set s char-encoding: encoding))))


(define. (path-or-port-settings.maybe-encoding s)
  (xcond ((path-string? s) #f)
         ((port-settings? s)
          (dsssl-maybe-ref s char-encoding:))))


(define. (path-or-port-settings.delete-encoding s)
  (xcond ((path-string? s) s)
         ((port-settings? s)
          (dsssl-delete s char-encoding:))))


(TEST
 > (.encoding-set "foo" 'iso-8859-1)
 (path: "foo" char-encoding: iso-8859-1)
 > (.encoding-set # 'iso-8859-1)
 (char-encoding: iso-8859-1 path: "foo")
 > (.encoding-set # 'UTF-8)
 (char-encoding: UTF-8 path: "foo")
 > (.delete-encoding #)
 (path: "foo"))

