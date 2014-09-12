;;; Copyright 2014 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy test
	 rgb colorspaces)


(class colorstring
       (struct #(string? value))
       (method html-colorstring colorstring.value))

;; change/parametrize when other solutions abound
(def color? (either colorstring? rgb?))

(defstruct colors
  #(color? stroke)
  #(color? fill))

(defstruct colored
  #((either color? colors?) color)
  value)

;; delegates, now they're coming?..
(def. (colored.start v)
  (.start (colored.value v)))

(def. (colored.min+maxs/prev a b)
  (.min+maxs/prev (colored.value a) b))

