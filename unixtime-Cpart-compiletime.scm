;;; Copyright 2013-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy)

(export sizeof-time_t
	bitsof-time_t)

(c-declare "
       #define _GNU_SOURCE
       #define _XOPEN_SOURCE
       #include <time.h>
       #include <stdlib.h>
")

(def sizeof-time_t (##c-code "___RESULT= ___FIX(sizeof(time_t));"))

(assert (<= sizeof-time_t 8))
;; ah wow we are on 64 bits already?!

(def bitsof-time_t (* sizeof-time_t 8))

