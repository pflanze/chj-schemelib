;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require cj-warn
	 unixtime)

(export warn-t)

(include "cj-standarddeclares.scm")


(def (warn-t msg . args)
     (let ((p (current-error-port)))
       (display (unixtime.localtime-string (current-unixtime)) p)
       (display " - " p))
     (apply warn msg args))
