(require)

(include "../cj-standarddeclares.scm")


; (declare (fixnum)
; 	 (not safe))

(c-declare "
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

static const size_t buflen= 1024;
static char* buf = NULL;
")

(define strerror
  (c-lambda (int)
	    ISO-8859-1-string
	    "
if (!buf) {
    buf= malloc(buflen);
    /* XX let it segfault if malloc fails? */
}

___result= ___CAST(unsigned char*, strerror_r(___arg1, buf, buflen));
"))

