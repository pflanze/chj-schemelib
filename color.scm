;;; Copyright 2014 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy test
	 rgb colorspaces)


(def color? rgb?) ;; change/parametrize when other solutions abound

(defstruct colored
  #(color? color)
  value)

