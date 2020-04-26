;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (string-util-2 string-downcase)
         string-case
         test)

(export os-exception-codesymbol)

(use-memcmp);; oh my, for string-case


(define (os-exception-codesymbol e) ;; -> symbol? or exception
  "A representation of errors as symbols, unlike the \"OS dependent\"
codes, and unlike the strings which might change. Throws an exception
on unknown errors."
  ;; Sigh (why do I have to do this)
  (let* ((code (os-exception-code e))
         (msg (string-downcase (err-code->string code))))
    (string-case
     msg
     (("can't convert from c utf-8-string") 'cannot-convert-from-utf-8)
     (("can't convert from c char-string") 'cannot-convert-from-c-char-string)
     (else
      (error "unknown error message (unfinished) for code:" msg code)))))

