;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy)

(export (interface sqlite3-error
                   (class sqlite3-error-static)
                   (class sqlite3-error/message)))

(include "cj-standarddeclares.scm")


(definterface sqlite3-error
  (method (code s) -> fixnum?)
  (method (name s) -> symbol?)
  (method (description s) -> string?)
  ;; implements cj-exception-handler's error protocol (which isn't
  ;; declared as an interface there! XX?):
  (method (maybe-exception-message s) -> list?)
  
  (defclass (sqlite3-error-static [fixnum? code]
                                  [symbol? name]
                                  [string? description])
    (defmethod (maybe-exception-message e)
      (list "sqlite3-error-static" name description)))

  (defclass (sqlite3-error/message [sqlite3-error-static? error]
                                   [string? message])
    (defmethod (code s) (.code error))
    (defmethod (name s) (.name error))
    (defmethod (description s) (.description error))
    (defmethod (maybe-exception-message e)
      (list "sqlite3-error/message" (.name error) message))))

