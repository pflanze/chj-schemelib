;;; Copyright 2016-2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy)

;; finally?

(defenum eol-name CR LF CRLF)

(def. (eol-name.newline-string v)
  (xcase v
	 ((CR) "\r")
	 ((LF) "\n")
	 ((CRLF) "\r\n")))


