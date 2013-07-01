;(namespace "")
(requires gambit-interpreter-env
	  (cj-env warn)
	  ;;(interrupts interrupt-install-handler! SIGCHLD)
	  cj-c-util ;; {maybe-,}define-constant-from-c
	  cj-c-errno ;; including define/check and define/check->integer
	  (cj-c-errno throw-posix-exception
		      posix-exception);;(or should I make cj-c-errno export those by default?)
	  (cj-c-errno posix-exception?
		      posix-exception-errno
		      posix-exception-message) ;; for re-export
	  ;; for the cj-c-types.scm include: (compile-time only)
	  cj-env
	  cj-gambit-sys
	  (cj-test TEST)
	  )
;; it also does (include "cj-c-types.scm")

(exports
 posix-exception?
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
 _socketpair socketpair
 ;;;many more constants...
 
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
  O_NOFOLLOW ;; may be #f -- well might if not a hack was used
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
;  F_SETLEASE
;  F_GETLEASE
;  F_NOTIFY
;  DN_ACCESS
;  DN_MODIFY
;  DN_CREATE
;  DN_DELETE
;  DN_RENAME
;  DN_ATTRIB
;  DN_MULTISHOT


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
 )

(exports-on-request
 status? ;; is it a s32vector of length 1?
 strerror
 )

(compile #t)
