;;; Copyright 2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy test
	 rgb colorspaces)


;; color =================================================================

(class colorstring
       (struct #(string? value))
       (method html-colorstring colorstring.value)

       (method maybe-stroke-color identity)
       (method maybe-fill-color identity)
       (method maybe-stroke-width false/1))

;; change/parametrize when other solutions abound
(def color? (either colorstring? rgb?))

;; add methods to existing class (monkey patching)
(class rgb
       (method maybe-stroke-color identity)
       (method maybe-fill-color identity)
       (method maybe-stroke-width false/1))

;; paintoptions ==========================================================

(class colors
       (struct #(color? stroke)
	       #(color? fill))
       (method maybe-stroke-color colors.stroke)
       (method maybe-fill-color colors.fill)
       (method maybe-stroke-width false/1))

;; with stroke width, and optional keyword parameters
(class paint
       (struct #!key
	       #((maybe color?) stroke-color)
	       #((maybe color?) fill-color)
	       #((maybe positive-real?) stroke-width))
       (method maybe-stroke-color paint.stroke-color)
       (method maybe-fill-color paint.fill-color)
       (method maybe-stroke-width paint.stroke-width))

(def paintoptions?
     (either color? colors? paint?))

;; applying it ===========================================================

(defstruct painted
  #((improper-list-of paintoptions?) optionS)
  value)

;; delegates, now they're coming?..
(def. (painted.start v)
  (.start (painted.value v)))

(def. (painted.min+maxs/prev a b)
  (.min+maxs/prev (painted.value a) b))

