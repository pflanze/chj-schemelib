;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.



(require test
	 joo)

(TEST
 > (joo-class (faaa2) (defmethod (blimm s) -1))
 > (.blimm (faaa2))
 -1)

