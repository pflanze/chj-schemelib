;;; Copyright 2013-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 (cj-env warn)
	 cj-warn
	 ;;(interrupts interrupt-install-handler! SIGCHLD)
	 cj-c-util	  ;; {maybe-,}define-constant-from-C
	 posix/cj-c-errno ;; including define/check and define/check->integer
	 (posix/cj-c-errno throw-posix-exception
			   posix-exception ;;(or should I make cj-c-errno export those by default?)

			   ;; for re-export:
			   posix-exception?
			   posix-exception-errno
			   posix-exception-message)
	 ;; for the cj-c-types.scm include: (compile-time only)
	 cj-env
	 cj-gambit-sys
	 (test TEST)
	 cj-env-2
	 ;;cj-struct
	 dot-oo

	 (cj-symbol string->uninterned-symbol)

	 ;; it also does (include "cj-c-types.scm")
	 )

(export posix-exception?
	posix-exception-errno
	posix-exception-message
 
	getpid
	getppid

	_fork fork fork*
 
	;; status dissection:
	WIFEXITED
	WEXITSTATUS
	WIFSIGNALED
	WTERMSIG
	WIFSTOPPED
	WSTOPSIG
	WCOREDUMP

	_wait wait wait* wait**
	_waitpid waitpid waitpid* waitpid**

	filedescriptors?
	;;error-filedescriptors-wrong-type  well,interesting?
	;;check-filedescriptors macro
	filedescriptors-read
	filedescriptors-write
	filedescriptors-first
	filedescriptors-second

	_pipe pipe
	pipe-values
	_socketpair socketpair
	;;many more constants...
 
	_close close

	_dup dup
	_dup2 dup2

	_execv execv
	_execvp execvp
	exec ;; wrapper around execvp

	_exit

	fd->port
	fd-nonblock-set!
	fd-nonblock?
 
	;; constants:
	WNOHANG
	WUNTRACED

	SEEK_SET
	SEEK_CUR
	SEEK_END
	O_APPEND
	O_NONBLOCK
	O_ASYNC
					;  O_DIRECT
	O_RDONLY
	O_WRONLY
	O_RDWR
	O_CREAT
	O_EXCL
	O_NOCTTY
	O_TRUNC
	O_NDELAY
	O_SYNC
	O_NOFOLLOW  ;; may be #f -- well might if not a hack was used
	O_DIRECTORY ;; may be #f -- well might if not a hack was used
	O_ASYNC
	O_LARGEFILE ;; may be #f
	F_DUPFD
	F_GETFD
	F_SETFD
	F_GETFL
	F_SETFL
	F_GETLK
	F_SETLK
	F_SETLKW
	F_RDLCK
	F_WRLCK
	F_UNLCK
	F_GETOWN
	F_SETOWN
	F_GETSIG ;; may be #f
	F_SETSIG ;; may be #f
	F_GETOWN
	F_SETOWN
	;;  F_SETLEASE
	;;  F_GETLEASE
	;;  F_NOTIFY
	;;  DN_ACCESS
	;;  DN_MODIFY
	;;  DN_CREATE
	;;  DN_DELETE
	;;  DN_RENAME
	;;  DN_ATTRIB
	;;  DN_MULTISHOT


	_setuid setuid
	_setgid setgid
	_seteuid seteuid
	_setegid setegid
	_setreuid setreuid
	_setregid setregid
	_getuid getuid
	_getgid getgid
	_geteuid geteuid
	_getegid getegid

	body->void*
	posix:_read posix:read
	posix:_write posix:write

	_open open
	_chdir chdir
	_fchdir fchdir
	_chroot chroot
	_mkdir mkdir
	_rmdir rmdir
	_getcwd getcwd

	posix:environ

	#!optional
	status? ;; is it a s32vector of length 1?
	strerror
	)

;; (compile #t)

(include "../cj-standarddeclares.scm")

;; Syntax used in this module:
;; - from cj-c-errno:
;; define/check:  normal scheme parameters, inside use c-lambda
;; define/check->integer: combined type/scheme parameters, creates c-lambda under the hood, no manual coding necessary anymore.

;; todo:
;; - return #!void instead of 0 in the cases where the only "successful" return value is 0? (called-for-side-effect)
;; (- formatting of error messages isnt really *that* well designed)

(c-declare "
#define _GNU_SOURCE 1 /* make O_NOFOLLOW and O_DIRECTORY appear, see man 2 open. Hm BUT DOES NOT HELP. */
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
/* #include <sys/stat.h>  for constants like O_DIRECT? no doesn't help */
#include <pwd.h>
#include <grp.h>
#include <sys/stat.h>
")


(define no-value (gensym 'no-value))


;; todo, are these always correct?
(c-define-type pid_t int)
(c-define-type off_t int)

(define posix:getpid
  (c-lambda ()
	    pid_t
	    "getpid"))

; (define (getpid-cheap)
;   (##c-code "
;     ___RESULT=___FIX(getpid());//NOT CORRECT
; "))
;about 90 cycles less than getpid.

(define posix:getppid
  (c-lambda ()
	    pid_t
	    "getppid"))


;; "should I return #f in the child or should I not?" (Reading scsh manual) Yes, I should.
(define/check posix:_fork posix:fork () 
  (error-to-posix-exception
   ;; I still let c-lambda convert to an integer, since using ___FIX
   ;; might not provide a range big enough.
   (let ((pid ((c-lambda ()
			 pid_t
			 "___result= fork();
                         if(___result<0) ___result=-errno;"))))
     (if (eq? pid 0)
	 #f
	 pid))))


(c-define (thread_startup) () void "thread_startup" "static"
	  (let ()
	    (declare (not interrupts-enabled))
	    (##thread-startup!)))

(define-typed (posix:fork*
	       #!optional
	       #((maybe procedure?) thunk)
	       continue-threads?)
  (let ()
    (declare (not interrupts-enabled))
    (if thunk
	;; to make sure other Gambit threads cannot run before being
	;; shut down, do everything in C:
	(let ((pid ((c-lambda (scheme-object)
			      pid_t
			      "
pid_t pid=fork();
if (pid<0) {
    ___result= -errno;
} else {
    if (pid) {
        ___result=pid;
    } else {
        ___result=0;
        if (___arg1 != ___FAL) {
            /* stop threads */
            thread_startup();
        }
    }
}
") continue-threads?)))
	  (cond ((< pid 0)
		 (throw-posix-exception
		  (posix-exception (- pid))
		  'fork*
		  '()
		  '()))
		((= pid 0)
		 ;; Gambit's exit will set all fd's to blocking, which
		 ;; makes the parent block if threading is used. So we use
		 ;; _exit here:
		 (let ((code (thunk)))
		   (if (##fixnum? code)
		       (posix:_exit code)
		       (begin
			 (warn "non-fixnum exit-code given:" code)
			 (posix:_exit 1)))))
		(else pid)))
	(posix:fork))))

;;XX prefix?
(define (status? obj)
  (and (s32vector? obj)
       (>= (##s32vector-length obj) 1)))

(define-constant-from-C WNOHANG)
(define-constant-from-C WUNTRACED)

(define-macro (typecheck-status arg body)
  `(if (status? ,arg)
       ,body
       (error ,(string-append (symbol->string arg)
			      " argument must be s32vector of length 1:")
	      ,arg)))


;; go the way of allocating a temporary s32vector if
;; necessary. (guess: converting a bigint to that is as fast as
;; converting it into hm. ok was it dumb? but its at least simpler
;; than 2 c-lambdas. it makes more sense than the intermediate bigint,
;; since usually we do have the vector already)

(define (posix:?status->status obj fn)
  (if (status? obj)
      (fn obj)
      (if (##fixnum? obj)
	  (fn (s32vector obj))
	  (error "expecting either s32vector of length 1 or fixnum:" obj))))


;; It's tempting to add a ? to those macros which return booleans, but
;; since they already carry IF in their name, I don't. Better create
;; aliases.

;;XX posix:-prefix these as well?
(define (WIFEXITED ?status)
  (posix:?status->status
   ?status
   (c-lambda (scheme-object)
	     scheme-object
	     "___result=WIFEXITED(*(___BODY(___arg1))) ? ___TRU : ___FAL;")))


(define (posix:W-errorchecked name fn-1ary)
  (lambda (status)
    (cond ((fn-1ary status) => (lambda (v) v))
	  (else (error "status value is not suited for this operation:" status name)))))

(define (WEXITSTATUS ?status) ;; "This macro can only be evaluated if WIFEXITED returned true."
  (posix:?status->status
   ?status
   (posix:W-errorchecked
    'WEXITSTATUS
    (c-lambda (scheme-object)
	      scheme-object
	      "___result= WIFEXITED(*(___BODY(___arg1))) ?
                                          ___FIX(WEXITSTATUS(*(___BODY(___arg1))))
                                          : ___FAL;"))))

(define (WIFSIGNALED ?status)
  (posix:?status->status
   ?status
   (c-lambda (scheme-object)
	     scheme-object
	     "___result=WIFSIGNALED(*(___BODY(___arg1))) ? ___TRU : ___FAL;")))

(define (WTERMSIG ?status) ;; "This macro can only be evaluated if WIFSIGNALED returned non-zero."
  (posix:?status->status
   ?status
   (posix:W-errorchecked
    'WTERMSIG
    (c-lambda (scheme-object)
	      scheme-object
	      "___result= WIFSIGNALED(*(___BODY(___arg1))) ?
                                          ___FIX(WTERMSIG(*(___BODY(___arg1))))
                                          : ___FAL;"))))

(define (WIFSTOPPED ?status)
  (posix:?status->status
   ?status
   (c-lambda (scheme-object)
	     scheme-object
	     "___result=WIFSTOPPED(*(___BODY(___arg1))) ? ___TRU : ___FAL;")))

(define (WSTOPSIG ?status) ;; "This macro can only be evaluated if WIFSTOPPED returned non-zero."
  (posix:?status->status
   ?status
   (posix:W-errorchecked
    'WSTOPSIG
    (c-lambda (scheme-object)
	      scheme-object
	      "___result= WIFSTOPPED(*(___BODY(___arg1))) ?
                                          ___FIX(WSTOPSIG(*(___BODY(___arg1))))
                                          : ___FAL;"))))


;" Some versions of Unix (e.g. Linux, Solaris, but not AIX, SunOS) also define a macro WCORE-
;  DUMP(status) to test whether the child process dumped core.  Only  use  this  enclosed  in
;  #ifdef WCOREDUMP ... #endif. "

(define WCOREDUMP-exists? (c-lambda ()
				    scheme-object
				    "
#ifdef WCOREDUMP
___result= ___TRU;
#else
___result= ___FAL;
#endif
"));; (sadly we cannot use this in a macro 8-), so we use it at initialization time)

(define WCOREDUMP
  (if (WCOREDUMP-exists?)
      (lambda (?status)
	(posix:?status->status
	 ?status
	 (c-lambda (scheme-object)
		   scheme-object
		   "
#ifdef WCOREDUMP
___result=WCOREDUMP(*(___BODY(___arg1))) ? ___TRU : ___FAL;
#else
___result=___FIX(-1000); /*shouldn't happen*/
#endif
")))
      (lambda (v)
	(error "WCOREDUMP is not supported on your system"))))


;; Is it correct to use s32 values for int on 64bit machines?  /todo write automatic test

(define/check posix:_wait posix:wait (&status)
  (typecheck-status
   &status
   (error-to-posix-exception
    ((c-lambda (scheme-object)
	       pid_t
	       "___result= wait(___CAST(int*,___BODY(___arg1)));
                                 if(___result<0) ___result=-errno;")
     &status))))


(define/check posix:_waitpid posix:waitpid (pid &status options)
  (typecheck-status
   &status
   (error-to-posix-exception
    ((c-lambda (pid_t scheme-object int)
	       pid_t
	       "
___result= waitpid(___arg1, ___CAST(int*,___BODY(___arg2)), ___arg3);
                                 if(___result<0) ___result=-errno;")
     pid &status options))))

;; ATTENTION: set a signal handler for SIGCHLD (using the interrupts
;; module), otherwise zombies will be reaped w/o getting a chance to
;; get at them unless wait is called before the child exits.

(define (posix:wait*)
  (let* ((&status (##make-s32vector 1))
	 (pid (posix:wait &status))) ;;TODO tailposition.
    (cons pid (s32vector-ref &status 0))))

(define (posix:waitpid* pid #!optional (options 0))
  (let* ((&status (##make-s32vector 1))
	 (pid (posix:waitpid pid &status options)))
    (cons pid (s32vector-ref &status 0))))


(define (posix:wait**)
  (let* ((&status (##make-s32vector 2))
	 (pid (posix:wait &status)))
    (s32vector-set! &status 1 pid)
    &status))

(define (posix:waitpid** pid #!optional (options 0))
  (let* ((&status (##make-s32vector 2))
	 (pid (posix:waitpid pid &status options)))
    (s32vector-set! &status 1 pid)
    &status))


;; for pipe, we don't take an argument, since only one value (the pair
;; of descriptors) is being returned (and we can differentiate this
;; value from errors through type checking):

;; the fds type:
;;XX posix:-prefix as well?
(define (filedescriptors? obj)
  (and (s32vector? obj)
       (eq? (s32vector-length obj) 2)))

(define (error-filedescriptors-wrong-type obj)
  (error "not a filedescriptors type:" obj))

(define-macro (check-filedescriptors expr body)
  (let ((v (gensym)))
    `(let ((,v ,expr))
       (if (filedescriptors? ,v)
	   ,body
	   (error-filedescriptors-wrong-type ,v)))))

(define (filedescriptors-read fds)
  (check-filedescriptors fds
			 (s32vector-ref fds 0)))

(define (filedescriptors-write fds)
  (check-filedescriptors fds
			 (s32vector-ref fds 1)))

;; for socketpair:
(define filedescriptors-first filedescriptors-read)
(define filedescriptors-second filedescriptors-write)


(define/check posix:_pipe posix:pipe ()
  (error-to-posix-exception
   (let ((v (##make-s32vector 2))) ;; (same worries as above, todo)
     (let ((r ((c-lambda (scheme-object)
			 int
			 "___result= pipe(___CAST(int*,___BODY(___arg1)));
                            if(___result<0) ___result=-errno;")
	       v)))
       (if (= r 0)
	   v
	   r)))))


;; For use with srfi-11, could alternatively write a let-s32vector macro, though.
(define (posix:pipe-values)
  (let ((v (posix:pipe)))
    (values (s32vector-ref v 0)
	    (s32vector-ref v 1))))


(define/check posix:_close posix:close (fd)
  (error-to-posix-exception
   ((c-lambda (int)
	      int
	      "___result= close(___arg1);
              if(___result<0) ___result=-errno;") fd)))



(c-declare "
       #include <sys/socket.h>
")

;; communication domains:
(define-constant-from-C PF_UNIX)
(define-constant-from-C PF_INET)
(define-constant-from-C PF_INET6);;doesthiswork?...
(define-constant-from-C PF_IPX)
(define-constant-from-C PF_NETLINK)
(define-constant-from-C PF_X25)
(define-constant-from-C PF_AX25)
(define-constant-from-C PF_ATMPVC)
(define-constant-from-C PF_APPLETALK)
(define-constant-from-C PF_PACKET);; Low level packet interface

;;;huh difference from PF_UNIX to AF_UNIX ?? (which man socketpair uses, see c-lambda below)
;(define-constant-from-C AF_UNIX)
;;both are 1

;; socket types:
(define-constant-from-C SOCK_STREAM)
(define-constant-from-C SOCK_DGRAM)
(define-constant-from-C SOCK_SEQPACKET)
(define-constant-from-C SOCK_RAW)
(define-constant-from-C SOCK_RDM)

;; protocols:  hum?
; (define-constant-from-C )
;...

;;  int socketpair(int d, int type, int protocol, int sv[2]);
(define/check posix:_socketpair posix:socketpair (#!optional (type SOCK_STREAM))
  (error-to-posix-exception
   (let ((v (##make-s32vector 2))) ;; (ditto, see above)
     (let ((r ((c-lambda (int scheme-object)
			 int
			 "
___result= socketpair(AF_UNIX, ___arg1, 0, ___CAST(int*,___BODY(___arg2)));
                           if(___result<0) ___result=-errno;")
	       type v)))
       (if (= r 0)
	   v
	   r)))))


;; seeking:
(define-constant-from-C SEEK_SET)
(define-constant-from-C SEEK_CUR)
(define-constant-from-C SEEK_END)

;; file open flags:
(define-constant-from-C O_APPEND)
(define-constant-from-C O_NONBLOCK)
(define-constant-from-C O_ASYNC)
; (define-constant-from-C O_DIRECT)

(define-constant-from-C O_RDONLY)
(define-constant-from-C O_WRONLY)
(define-constant-from-C O_RDWR)
(define-constant-from-C O_CREAT)
(define-constant-from-C O_EXCL)
(define-constant-from-C O_NOCTTY)
(define-constant-from-C O_TRUNC)
(define-constant-from-C O_NDELAY)
(define-constant-from-C O_SYNC)

;;Linux-specific:
(HACK_maybe-define-constant-from-C O_NOFOLLOW)
(HACK_maybe-define-constant-from-C O_DIRECTORY)

(define-constant-from-C O_ASYNC)
(maybe-define-constant-from-C O_LARGEFILE)

;; Handling close-on-exec
(define-constant-from-C F_DUPFD)
(define-constant-from-C F_GETFD)
(define-constant-from-C F_SETFD)

;; The file status flags
(define-constant-from-C F_GETFL)
(define-constant-from-C F_SETFL)

;; Advisory locking
(define-constant-from-C F_GETLK)
(define-constant-from-C F_SETLK)
(define-constant-from-C F_SETLKW)

(define-constant-from-C F_RDLCK)
(define-constant-from-C F_WRLCK)
(define-constant-from-C F_UNLCK)

;; Managing signals
(define-constant-from-C F_GETOWN)
(define-constant-from-C F_SETOWN)
;; Linux-specific:
(maybe-define-constant-from-C F_GETSIG)
(maybe-define-constant-from-C F_SETSIG)

;; specific to BSD and Linux:
(define-constant-from-C F_GETOWN)
(define-constant-from-C F_SETOWN)


;; Leases:
; (define-constant-from-C F_SETLEASE)
; (define-constant-from-C F_GETLEASE)

;; File and directory change notification  (Linux):
; (define-constant-from-C F_NOTIFY)
; (define-constant-from-C DN_ACCESS)
; (define-constant-from-C DN_MODIFY)
; (define-constant-from-C DN_CREATE)
; (define-constant-from-C DN_DELETE)
; (define-constant-from-C DN_RENAME)
; (define-constant-from-C DN_ATTRIB)
; (define-constant-from-C DN_MULTISHOT)


(define/check posix:_fcntl posix:fcntl (fd cmd #!optional (arg-or-lock no-value))
  (cond ((eq? arg-or-lock no-value)
	 (error-to-posix-exception
	  ((c-lambda (int int)
		     int
		     "___result= fcntl(___arg1,___arg2);
                                      if(___result<0) ___result=-errno;")
	   fd cmd)))
	((number? arg-or-lock)
	 (error-to-posix-exception
	  ((c-lambda (int int long)
		     int
		     "___result= fcntl(___arg1,___arg2,___arg3);
                                      if(___result<0) ___result=-errno;")
	   fd cmd arg-or-lock)))
	(else
	 (error "struct flock *lock argument currently not supported:" arg-or-lock))))


(define posix:_exit
  (c-lambda (int)
	    void
	    "_exit"))


(define/check->integer "execv" posix:_execv posix:execv
  ((UTF-8-string path)
   (nonnull-UTF-8-string-list argv))
  int)

(define/check->integer "execvp" posix:_execvp posix:execvp
  ((UTF-8-string path)
   (nonnull-UTF-8-string-list argv))
  int)

(define (posix:exec cmd . args)
  (posix:execvp cmd (cons cmd args)))


(define/check->integer "dup" posix:_dup posix:dup ((int oldfd)) int)
(define/check->integer "dup2" posix:_dup2 posix:dup2 ((int oldfd) (int newfd)) int)

;;XX posix:-prefix these?
(define (fd-nonblock-set! fd)
  (posix:fcntl fd F_SETFL (bitwise-ior (posix:fcntl fd F_GETFL) O_NONBLOCK)))

(define (fd-nonblock? fd)
  (any-bits-set? (posix:fcntl fd F_GETFL) O_NONBLOCK))

(define gambit-port-direction-in 1)
(define gambit-port-direction-out 2)
(define gambit-port-direction-inout 3)

(define (fd->port fd direction #!optional settings name)
  (cond ((and (##fixnum? fd)
	      (symbol? direction)
	      (or (not settings)
		  (list? settings)))
	 (fd-nonblock-set! fd) ;; (otherwise, gambit will be blocked!)
	 (##open-predefined (case direction
			      ((RDONLY input) gambit-port-direction-in)
			      ((WRONLY output) gambit-port-direction-out)
			      ((RDWR input-output) gambit-port-direction-inout)
			      (else (error "invalid direction specifyer:" direction)))
			    (or name
                                (list
                                 (string->uninterned-symbol
                                  (string-append "fd-" (number->string fd)))))
			    fd
			    (or settings '())))
	(else
	 (error "fd->port: invalid type of inputs:" fd direction settings))))


;; todo, are these always correct?
(c-define-type uid_t int)
(c-define-type gid_t int)




(define/check->integer "setuid" posix:_setuid posix:setuid ((uid_t uid)) int)
(define/check->integer "setgid" posix:_setgid posix:setgid ((gid_t gid)) int)

(define/check->integer "seteuid" posix:_seteuid posix:seteuid ((uid_t euid)) int)
(define/check->integer "setegid" posix:_setegid posix:setegid ((gid_t egid)) int)

(define/check->integer "setreuid" posix:_setreuid posix:setreuid ((uid_t ruid) (uid_t euid)) int)
(define/check->integer "setregid" posix:_setregid posix:setregid ((gid_t rgid) (gid_t egid)) int)

;; "This call is nonstandard. ..was first introduced in HP-UX."
;; The problem is, we should #define_GNU_SOURCE  but how to find out whether it is supported?
; (define/check->integer _setresuid setresuid ((uid_t ruid) (uid_t euid) (uid_t suid)) int)
; (define/check->integer _setresgid setresgid ((gid_t rgid) (gid_t egid) (gid_t sgid)) int)

(define/check->integer "getuid" posix:_getuid posix:getuid () uid_t)
(define/check->integer "getgid" posix:_getgid posix:getgid () gid_t)

(define/check->integer "geteuid" posix:_geteuid posix:geteuid () uid_t)
(define/check->integer "getegid" posix:_getegid posix:getegid () gid_t)

;those would require boxes or simliar, but Linux-specific anyway:
;(define/check->int _getresuid getresuid ...)
;(define/check->int _getresuid getresgid ...)

;; ===== read/write accesses: ========================================

(define/check->integer "lseek" posix:_lseek posix:lseek
  ((int fd) (off_t offset) (int whence)) off_t)


;; Note that these are unbuffered and are not integrated into Gambit's
;; I/O (thus block!)

;; Using the typesafe but (relatively) slow way of ffi wrappers for
;; holding addresses:

;; (use prefixes to differentiate from r5rs functions read and write)

(c-define-type size_t int);;; OK? todo
(c-define-type ssize_t int);;; OK? todo
(include "../cj-c-types.scm");; for void* and const_void*

(define/check->integer "read" posix:_read posix:read
  ((int fd) (void* buf) (size_t count))
  ssize_t)

(define/check->integer "write" posix:_write posix:write
  ((int fd) (const_void* buf) (size_t count))
  ssize_t)

;; (define/check->integer "read" posix:_read-u8vector posix:read-u8vector
;;   ((int fd) ((pointer unsigned-int8) buf) (size_t count))
;;   ssize_t)
;;doesn't work, expansion hand-edited:
(begin
  (define (posix:_read-u8vector fd buf count)
    (if (u8vector? buf)
	(error-to-posix-exception
	 ((c-lambda
	   (int scheme-object size_t)
	   ssize_t
	   "___result= read(___arg1, ___BODY(___arg2), ___arg3);
 if(___result<0) ___result=-errno;")
	  fd
	  buf
	  count))
	(error "not a u8vector:" buf)))
  (define (posix:read-u8vector fd buf count)
    (check-not-posix-exception
     (posix:_read-u8vector fd buf count)
     'posix:read-u8vector
     '(fd buf count)
     (lambda () (list fd buf count)))))

;; (define/check->integer "write" posix:_write-u8vector posix:write-u8vector
;;   ((int fd) ((pointer unsigned-int8) buf) (size_t count))
;;   ssize_t)
;; ditto, aha, less expansion needed:
(define/check
  posix:_write-u8vector
  posix:write-u8vector
  (fd buf count)
  (if (u8vector? buf)
      (error-to-posix-exception
       ((c-lambda
	 (int scheme-object size_t)
	 ssize_t
	 "___result= write(___arg1, ___BODY(___arg2), ___arg3); if(___result<0) ___result=-errno;")
	fd
	buf
	count))
      (error "not a u8vector:" buf)))

;;NOTE that void* canot be replaced with wordaddress: it's not
;;necessarily bound/parallizd ehr to a word. Aligned.

(c-declare "
#define stillp(obj) (___HD_TYP(___HEADER(obj))==___STILL)
#define permp(obj) (___HD_TYP(___HEADER(obj))==___PERM)
#define is_still_or_perm(obj) (stillp(obj)||permp(obj))
")

(define (body->void* obj) ;; or object->void* or object-body->void* ..
  (or ((c-lambda (scheme-object)
		 void*
		 "___result_voidstar= ___MEM_ALLOCATED(___arg1) && is_still_or_perm(___arg1)
                      ? ___BODY(___arg1) : 0;")
       obj)
      (error "body->void*: is not a still or permanent memory allocated object:" obj)))
;;^- todo move elsewhere

;; Interface with Scheme objects as buffers.  Typesafe in the sense
;; that it checks for mem-allocated Scheme objects, and their size
;; (yes this is bound checked!). [It might also be made to only accept
;; scheme objects which can't be made inconsistent, e.g. binary ones
;; (excluding bignums?)]

;; thumb eh dumb name? "*-buffer"

(define-macro* (c-function-address name-str)
  (sourcify `(let ((adr (@make-addressbox)))
	       (##c-code ,(string-append "
void (**p) (void) = (void (**) (void)) ___BODY(___ARG1);
*p= (void (*) (void)) " (source-code name-str) ";")
			 adr)
	       adr)
	    name-str))

(define &write (c-function-address "write"))
(define &read (c-function-address "read"))
;; (of course it's a joke, right? I wanted to parametrize the stuff
;; below to save space / double compilation, but because of the
;; necessity to compile for getting the function pointer (without
;; using the dynamic linker) I have double compilation anyway..)
;; (Hm but we can hope that the code is really small. so ok.)

;;UNNND wie das error endli.oder eben aussenrum?.ech.  mit inlined gambit-sys ops.
; (dedede "

; ")

; (define/check _read-buffer read-buffer
;   #hmm scheisse,  parametrisierung geht ja gar nöd da?
;ç

; (define/check->integer _read-buffer read-buffer
;   (fd buf #!optional start end)
;   (check-mem-allocated )  
;   ssize_t)

; (define/check->integer _write write
;   ((int fd) (const_void* buf) (size_t count))
;   ssize_t)



;; ----


;; todo, are these always correct?
(c-define-type mode_t int)


(define/check posix:_open posix:open (pathname flags #!optional mode)
  (error-to-posix-exception
   (if mode
       ((c-lambda (UTF-8-string
		   int
		   mode_t)
		  int
		  "___result= open(___arg1,___arg2,___arg3);
                   if(___result<0) ___result=-errno;") pathname flags mode)
       ((c-lambda (UTF-8-string
		   int)
		  int
		  "___result= open(___arg1,___arg2);
                   if(___result<0) ___result=-errno;") pathname flags))))

;; ^- instead of duplicating c-lambdas it "would be nice" if I could
;; declare a C type maybe-mode_t which can be given #f to turn into
;; somehow hm what? (like null-string stuff). well would require a
;; separate flag...so maybe not. Or it would be nice if there were
;; easily usable converters in C space, to feed a scheme-object
;; through, check it and if it's not false convert it to a mode_t --
;; all including an error mechanism if conversion fails..

(define/check->integer "chroot" posix:_chroot posix:chroot
  ((UTF-8-string path))
  int)

(define/check->integer "chdir" posix:_chdir posix:chdir
  ((UTF-8-string path))
  int)

(define/check->integer "fchdir" posix:_fchdir posix:fchdir
  ((int fd))
  int)

(define/check->integer "mkdir" posix:_mkdir posix:mkdir
  ((UTF-8-string path)
   (mode_t mode))
  int)

(define/check->integer "rmdir" posix:_rmdir posix:rmdir
  ((UTF-8-string path))
  int)



;; man 2 sync:

;; CONFORMING TO
;;        sync(): SVr4, 4.3BSD, POSIX.1-2001.
;;        syncfs() is Linux-specific.

;;  syncfs() first appeared in Linux 2.6.39; library support was added
;;  to glibc in version 2.14.

(c-declare "
#define CJ_HAVE_SYNCFS 0 /* introduced in glibc 2.14, how to test? */
#if CJ_HAVE_SYNCFS
static int wrapped_syncfs (int fd) {
    return syncfs(fd);
}
#else
static int wrapped_syncfs (int fd) {
    errno= ENOSYS;
    return -1;
}
#endif
")

(define/check->integer "wrapped_syncfs" posix:_syncfs posix:syncfs
  ((int fd))
  int
  nowarn: #t)

(define posix:sync
  ;; never fails, heh, huh
  (c-lambda () void
	    "sync"))


;; (c-declare "
;; ")

(define/check->integer "fsync" posix:_fsync posix:fsync
  ((int fd))
  int)

(define/check->integer "fdatasync" posix:_fdatasync posix:fdatasync
  ((int fd))
  int)



(define (char*->string u8vec _charset) ;; copies "the char* in u8vec" (up to the end or to before the first \0) into a string
  ;;; todo . this is a hack only.   Why am I doing this here and not use the ffi conversion functions? because those are statically compiled. Here I'm hoping to be runtime configurable in the future.
  ;;or is there such a copy function already?
  (let* ((len (u8vector-length u8vec))
	 (len* (let lp ((i 0))
		 (if (>= i len)
		     i
		     (let ((ch (u8vector-ref u8vec i)))
		       (if (= ch 0)
			   i
			   (lp (+ i 1)))))))
	 (out (##make-string len*)))
    (let lp ((i 0))
      (if (>= i len*)
	  out
	  (begin
	    (string-set! out i (integer->char (u8vector-ref u8vec i)))
	    (lp (+ i 1)))))))
(TEST
 > (char*->string '#u8(0 ) 0)
 ""
 > (char*->string '#u8(1) 0)
 "\1"
 > (char*->string '#u8(72 97 108 108 246 99 104 101 110 10 0 97 98 99 10) 0)
 "Hall\366chen\n"
 > (char*->string '#u8(72 97 108 108 246 99 104 101 110 10 97 98 99 10) 0)
 "Hall\366chen\nabc\n"
 )

(define/check posix:_getcwd posix:getcwd ()
  (let lp ((size 128))
    (let* ((buf (##make-u8vector size))
	   (res (##c-code "
char* res= getcwd ((char*)___BODY(___ARG1), ___INT(___ARG2));
if (res) {
    ___RESULT= ___TRU;
} else {
    if (errno==ERANGE) {
        ___RESULT= ___FAL;
    } else {
        ___RESULT= ___FIX(errno);
    }
}
" buf size)))
      (if (not res)
	  (begin
	    (warn "buffer too small, doubling buffer size");;debugging only
	    (lp (* size 2)))
	  (if (eq? res #t)
	      (char*->string buf 'THE_ENCODING)
	      (posix-exception res))))))




;; (todo: move this to more appropriate place? those are [partly] linux specific!)

(c-declare "
#include <sys/prctl.h>
#include <errno.h>
")

(define posix:prctl
  (c-lambda (int ;; option
	     unsigned-long ;; arg2
	     unsigned-long ;; arg3
	     unsigned-long ;; arg4
	     unsigned-long ;; arg5
	     )
	    int
	    ;;"prctl"
	    ;; errno handling reinvention:
	    "___result= prctl (___arg1, ___arg2, ___arg3, ___arg4, ___arg5);
if (___result<0) {
    ___result= -errno;
}
"))

(define-constant-from-C PR_SET_PDEATHSIG)
(define-constant-from-C PR_GET_PDEATHSIG)
(define-constant-from-C PR_SET_DUMPABLE)
(define-constant-from-C PR_GET_DUMPABLE)
(define-constant-from-C PR_SET_KEEPCAPS)
(define-constant-from-C PR_GET_KEEPCAPS)



(define/check posix:_truncate posix:truncate (pathname length)
  (error-to-posix-exception
   ((c-lambda (UTF-8-string
	       off_t)
	      int
	      "___result= truncate(___arg1,___arg2);
               if(___result<0) ___result=-errno;") pathname length)))

(define/check posix:_ftruncate posix:ftruncate (fd length)
  (error-to-posix-exception
   ((c-lambda (int
	       off_t)
	      int
	      "___result= ftruncate(___arg1,___arg2);
               if(___result<0) ___result=-errno;") fd length)))


(define/check posix:_symlink posix:symlink (oldpath newpath)
  (error-to-posix-exception
   ((c-lambda (UTF-8-string
	       UTF-8-string)
	      int
	      "___result= symlink(___arg1,___arg2);
               if(___result<0) ___result=-errno;") oldpath newpath)))


(define/check posix:_unlink posix:unlink (path)
  (error-to-posix-exception
   ((c-lambda (UTF-8-string)
	      int
	      "___result= unlink(___arg1);
               if(___result<0) ___result=-errno;") path)))

(define/check posix:_chown posix:chown (path owner group)
  (error-to-posix-exception
   ((c-lambda (UTF-8-string
	       uid_t
	       gid_t)
	      int
	      "___result= chown(___arg1,___arg2,___arg3);
               if(___result<0) ___result=-errno;") path owner group)))


;; userspace posix stuff: XX separate section or file?

(define-struct. posix-passwd
  name
  passwd
  uid
  gid
  gecos
  dir
  shell)

(c-define (posix_passwd n p u g ge d sh)
	  (UTF-8-string
	   UTF-8-string
	   uid_t
	   gid_t
	   UTF-8-string
	   UTF-8-string
	   UTF-8-string)
	  scheme-object
	  "posix_passwd" ""
	  (posix-passwd n p u g ge d sh))


;;(define/check posix:_getpwnam posix:getpwnam (name)
;; XX how does that work again. anyway, what use would posix:_getpwnam be. thus:

(define (posix:getpwnam name)
  ;; returns maybe value
  (let* ((res ((c-lambda (UTF-8-string)
			 scheme-object
			 "
struct passwd* p;
errno=0;
p= getpwnam(___arg1);
if (p) {
    ___result= posix_passwd(
           p->pw_name,
           p->pw_passwd,
           p->pw_uid,
           p->pw_gid,
           p->pw_gecos,
           p->pw_dir,
           p->pw_shell);
    // XX how does it propagate exceptions during or entering or exiting the scheme call within the posix_passwd call?
} else {
    if (errno) {
        ___result= ___FIX(-errno);
    } else {
        ___result= ___FAL;
    }
}
")
		  name)))
    (if (fixnum? res)
	(raise (posix-exception (- res)))
	res)))


;; much of the same as above, sigh, but also many changes. XX abstract how?

(define-struct. posix-group
  name
  passwd
  gid
  mem)

(c-define (posix_group n p g m)
	  (UTF-8-string
	   UTF-8-string
	   gid_t
	   nonnull-UTF-8-string-list)
	  scheme-object
	  "posix_group" ""
	  (posix-group n p g m))

(define (posix:getgrnam name)
  ;; returns maybe value
  (let* ((res ((c-lambda (UTF-8-string)
			 scheme-object
			 "
struct group* p;
errno=0;
p= getgrnam(___arg1);
if (p) {
    ___result= posix_group(
           p->gr_name,
           p->gr_passwd,
           p->gr_gid,
           p->gr_mem);
    // XX how does it propagate exceptions during or entering or exiting the scheme call within the posix_passwd call?
} else {
    if (errno) {
        ___result= ___FIX(-errno);
    } else {
        ___result= ___FAL;
    }
}
")
		  name)))
    (if (fixnum? res)
	(raise (posix-exception (- res)))
	res)))


(define posix:environ ##os-environ)
