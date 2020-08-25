;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy-2
         test)

(export container?
        (generics .for-each-item))

(include "cj-standarddeclares.scm")

"A container is a Scheme value that contains other values that can be
containers (or have location information, which on Gambit amounts to
the same thing). I.e. numbers consist of bits and homogenous vectors
consist of numbers but they are not containers. cj-struct etc. objects
are not container either, unless explicitly made so. (Meant primarily
for dealing with source code where no struct literals are (currently)
possible anyway.)"


(def container?
     (either vector?
             pair?))


(def. vector.for-each-item (flip vector-for-each))

(def. (pair.for-each-item p proc)
  (let-pair ((a r) p)
            (proc a)
            (proc r)))

