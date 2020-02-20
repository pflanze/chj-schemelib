;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         posix/cj-posix
         Result)

(export open
        open-new)

"Nicer API for cj-posix."
;; Perhaps portable, too? Not possible? Try?

(include "../cj-standarddeclares.scm")


(def (open [path-string? path]
           #!key
           [uint32? flags]
           (mode #o666)
           [(maybe list?) settings])
     -> (Result-of port?
                   posix-exception?)

     (let (res (posix:_open path flags mode))
       (if (posix-exception? res)
           (Error res)
           (Ok (fd->port res
                         (posix:open-flags->direction flags)
                         settings
                         (path-normalize path))))))

(def (open-new [path-string? path]
               #!key
               (flags (bitwise-or O_CREAT O_EXCL O_WRONLY O_APPEND))
               (mode #o600)
               [(maybe list?) settings])
     -> (Result-of output-port?
                   posix-exception?)
     (open path flags: flags mode: mode settings: settings))

