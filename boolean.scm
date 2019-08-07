;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;; This file is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License (LGPL)
;;; as published by the Free Software Foundation, either version 2 of
;;; the License, or (at your option) any later version.


(require easy ;; dot-oo would be enough, tough
         test)

(export (method boolean.=))

(def. (boolean.= [boolean? a] [boolean? b])
  (eq? a b))

