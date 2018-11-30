;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 eol)


(defparameter current-csv-input-sep-char #\,)
(defparameter current-csv-input-eol 'LF)

(defparameter current-csv-output-sep-char #\,)
(defparameter current-csv-output-eol 'LF)

