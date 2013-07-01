
(require cj-functional
	 cj-ffi)

(declare (block)
	 (standard-bindings)
	 (extended-bindings))

(c-declare "
#include <sys/mman.h>

#define VOID2FIX(e) ___CAST(___WORD,(e))
#define FIX2VOID(e) ___CAST(void*,(e))

int mmap_errno=0;
#include <string.h> // strerror
#include <errno.h> // errno

")

(define void*? fixnum?)

(define-typed (_mmap #((maybe void*?) addr) ;; addr not yet implemented
		     #(size? length)
		     #(size0? prot) ;; int
		     #(size0? flags) ;; int
		     #(size0? fd)    ;; int
		     ;; XX what is off_t?:
		     #(size0? offset))
  ;; returns 0 instead of -1 on errors
  (##c-code "
void* addr= FIX2VOID(___ARG1);
size_t length= ___INT(___ARG2);
int prot= ___INT(___ARG3);
int flags= ___INT(___ARG4);
int fd= ___INT(___ARG5);
off_t offset= ___INT(___ARG6);

void* res= mmap(addr, length, prot, flags, fd, offset);
mmap_errno= errno;
___RESULT= (___CAST(___WORD,res) == -1) ? ___FIX(0) : VOID2FIX(res);
" (or addr 0) length prot flags fd offset))

(define mmap:error
  (c-lambda ()
	    char-string
	    "___result= strerror(mmap_errno);"))

(define (mmap maybe-addr length prot flags fd offset)
  (let ((res (_mmap maybe-addr length prot flags fd offset)))
    (if (zero? res)
	(error "mmap:" (mmap:error))
	res)))


(define-constants-from-C PROT_EXEC PROT_READ PROT_WRITE PROT_NONE)

(define-constants-from-C MAP_SHARED MAP_PRIVATE)

(define-typed (_munmap #(void*? addr)
		       #(size? length))
  (##c-code "
void* addr= FIX2VOID(___ARG1);
size_t length= ___INT(___ARG2);

___RESULT= ___FIX(munmap(addr,length));
mmap_errno= errno;"
	    addr length))

(define (munmap addr length)
  (or (zero? (_munmap addr length))
      (error "unmap:" (mmap:error))))

