(declare (block)(standard-bindings)(extended-bindings))


; (declare (fixnum)
; 	 (not safe))

(c-declare "
#include <unistd.h>
#include <errno.h>
#include <string.h>
")

(define strerror
  (c-lambda (int)
	    ISO-8859-1-string
	    "strerror"))
;;(^- todo: use strerror_r to be pthread-safe)

