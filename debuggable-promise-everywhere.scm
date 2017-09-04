;;; Copyright 2017 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.

(require define-macro-star
	 debuggable-promise)

(export delay force promise?)


(define-macro* (delay . rest)
  `(debuggable#delay ,@rest))

(define force debuggable#force)
(define promise? debuggable#promise?)
