
(require)

(include "cj-standarddeclares.scm")

(c-declare "
static int segv(int* p) {
    return *p;
}
")

(define (segv i)
  (##c-code "___RESULT= ___INT(segv(___CAST(int*,___ARG1)));" i))
