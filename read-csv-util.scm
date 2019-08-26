;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         ;; read-csv
         test)

(export read-csv-util:list-empty?)


(def (read-csv-util:list-empty? l)
     (or (null? l)
         (let-pair ((a l) l)
                   (and (not a)
                        (read-csv-util:list-empty? l)))))

