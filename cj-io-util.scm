;;; Copyright 2013-2014 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require test)


(define (read-lines #!optional (p (current-input-port)) (tail '()))
  (let rec ()
    (let ((line (read-line p)))
      (if (eof-object? line)
	  tail
	  (cons line (rec))))))


(define (writeln obj)
  (write obj)
  (newline))


(define (port.content p)
  (read-line p #f))

(define (pathspec.xcontent pathspec)
  (let* ((p (open-input-file pathspec))
	 (output (port.content p)))
    (close-port p)
    output))

(define (string.print-file str pathspec)
  (let* ((p (open-output-file pathspec)))
    (display str p)
    (close-port p)))

(define (_call-with-process open-process close-port parms proc)
  (let* ((p (open-process parms))
	 (res (proc p)))
    (close-port p)
    (let ((s (process-status p)))
      (values res s))))

(define (__xcall-with-process open-input-process
			      close-input-port)
  (lambda (ok? values err)
    (lambda (parms proc)
      (letv ((res s) (_call-with-process open-input-process
					 close-input-port
					 parms
					 proc))
	    (if (ok? s)
		(values res s)
		(err s parms))))))

(define _xcall-with-input-process
  (__xcall-with-process open-input-process close-input-port))

(define _xcall-with-process
  (__xcall-with-process open-process close-port))


(define xcall-with-input-process
  (_xcall-with-input-process
   zero?
   (lambda (res s) res)
   (lambda (s parms)
     (error "process exited with non-zero status:"
	    s
	    parms))))

;; #f on subprocess errors
(define Xcall-with-input-process
  (_xcall-with-input-process
   zero?
   (lambda (res s) res)
   (lambda (s parms)
     #f)))


(define _error-exited-with-error-status
  (lambda (s parms)
    (error "process exited with error status:"
	   s
	   parms)))



;; XX make xx* vs x* vs. odd _ x variant naming consistent
;; Well, deviate from xperlfunc by omitting one x ?

(define (_system status-ok?)
  (let ((xcall (_xcall-with-process
		status-ok?
		(lambda (res s) s)
		_error-exited-with-error-status)))
    (lambda (cmd . args)
      (xcall (list path: cmd
		   arguments: args
		   stdin-redirection: #f
		   stdout-redirection: #f)
	     void/1))))

(define xxsystem (_system zero?))

(define xsystem (_system true/1))

(define 01status? (either zero? (lambda_ (= _ 256))))

(define 01system (_system 01status?))


(TEST
 > (xxsystem "true")
 ;; well, guaranteed result value iff returns, thus useless, but for
 ;; consistency with xsystem still nice
 0
 > (%try-error (xxsystem "false"))
 #(error
   "process exited with error status:"
   256
   (path: "false" arguments: () stdin-redirection: #f stdout-redirection: #f))
 > (xsystem "true")
 0
 > (xsystem "false")
 256
 > (xsystem "bash" "-c" "exit 250")
 64000
 > (with-exception-catcher no-such-file-or-directory-exception?
			   (& (xsystem "nonexistingbinary81874")))
 #t)


(define (_backtick status-ok? cont)
  (let ((xcall (_xcall-with-input-process
		status-ok?
		(lambda (output s)
		  (cont (if (eof-object? output) ;; stupid lib
			    ""
			    (chomp output))
			s))
		_error-exited-with-error-status)))
    (lambda (cmd . args)
      (xcall (list path: cmd
		   arguments: args
		   stdout-redirection: #t
		   char-encoding: 'UTF-8)
	     (lambda_
	      (read-line _ #f))))))

(define xbacktick (_backtick zero? (lambda (out s) out)))
(define Xbacktick (_backtick true/1 values))

;; (define one? (lambda_ (= _ 1)))

;; XX stupid name, what else?
(define 01backtick (_backtick 01status? (lambda (out s) out)))

(define backtick (_backtick 01status? values))

(TEST
 > (xbacktick "true")
 ""
 > (%try-error (xbacktick "false"))
 #(error "process exited with error status:"
	 256 (path: "false" arguments: () stdout-redirection: #t char-encoding: UTF-8))
 > (01backtick "false")
 ""
 > (xbacktick "echo" "world")
 "world"
 ;; check that unicode is read as such:
 > (xbacktick "echo" "-e" "Mot\\xc3\\xb6rhead")
 "Mot\366rhead"
 ;; and check that it gets 'correctly' to the process, too:
 ;; > (xbacktick "echo" "Motörhead")
 ;; "Mot\366rhead"
 ;; XXX: Gambit passes the argument as latin1, *and* then silently cuts off the latin1 result to "Mot"
 > (values.vector (backtick "false"))
 #("" 256)
 > (/ (snd (Xbacktick "sh" "-c" "exit 23")) 256)
 23)



(define (xbacktick-bash code)
  (xbacktick "bash" "-c" code))

(define bash xbacktick-bash) ;; ok?

(define (hostname)
  (xbacktick "hostname"))

;; where should that be moved to?
(define file-info->mtime
  (compose time->seconds
	   file-info-last-modification-time))



;; -- for a different libary?  not full path lib but simple path manipul

;; a basename that behaves like the shell util, not like
;; name->basename+maybe-suffix . One-argument form only, though, for
;; now.

(define (file-basename path)
  (last (string-split path #\/)))

(TEST
 > (file-basename "/foo/bar.scm")
 "bar.scm"
 > (file-basename "bar.scm")
 "bar.scm"
 > (file-basename "/foo/")
 ""
 )

(define (basename path #!optional suffix)
  (let* ((n (file-basename (if (string-ends-with? path "/")
			       (substring path 0 (dec (string-length path)))
			       path))))
    (if (and suffix
	     (string-ends-with? n suffix))
	(substring n 0 (- (string-length n)
			  (string-length suffix)))
	n)))

(TEST
 > (basename "/foo/bar.scm")
 "bar.scm"
 > (basename "/foo/bar.scm" ".scm")
 "bar"
 > (basename "/foo/bar.scmn" ".scm")
 "bar.scmn"
 > (basename "bar.scm")
 "bar.scm"
 > (basename "bar.scm" "longerthanthething")
 "bar.scm"
 > (basename "/foo/")
 "foo"
 > (basename "\\foo\\")
 "\\foo\\" ;; just like the shell util
 > (basename "/foo/..")
 ".."
 > (basename "/foo/.")
 "."
 )

(define dirname-slow
  (cut xbacktick "dirname" <>))

;; (define dirname-fast
;;   ;; (strings-join (reverse (cdr (reverse (string-split path #\/)))))
;;   ;; well no.
;;   ;;..sigh
;;   )

(define dirname dirname-slow)


(TEST
 ;; it seems to be s|/+[^/]+/*$|| except for using / when it gets "" ?
 ;; or the first char rather?
 > (dirname "/foo/bar")
 "/foo"
 > (dirname "/foo/bar/")
 "/foo"
 > (dirname "/foo//bar/fi.scm")
 "/foo//bar"
 > (dirname "/foo//bar/fi.scm/..")
 "/foo//bar/fi.scm"
 > (dirname "/foo//bar/fi.scm/../..")
 "/foo//bar/fi.scm/.."
 > (dirname "/")
 "/"
 > (dirname "/foo")
 "/"
 > (dirname "//")
 "/"
 > (dirname "///")
 "/"
 > (dirname ".//")
 "."
 > (dirname ".//a")
 "."
 > (dirname "foo//a")
 "foo"
 > (dirname "foo")
 "."
 )



(define (port->stream p read close-port)
  (let rec ()
    (delay
      (let ((item (read p)))
	(if (eof-object? item)
	    (begin
	      (close-port p)
	      '())
	    (cons item (rec)))))))

(define (directory-item-stream dir)
  (port->stream (open-directory dir)
		read
		close-port))


(define (_-name-or-id->id get access msg)
  (lambda (v)
    (cond ((string? v)
	   (cond ((get v)
		  => (lambda (p)
		       (access p)))
		 (else
		  (error msg v))))
	  ((natural0? v) v)
	  (else (error "invalid type:" v)))))

(define (user-name-or-id->id v)
  ((_-name-or-id->id posix:getpwnam .uid "unknown user name:") v))

(define (group-name-or-id->id v)
  ((_-name-or-id->id posix:getgrnam .gid "unknown group name:") v))

(define (chown path maybe-username-or-id maybe-groupname-or-id)
  ;; XX is this different from other cases (which?) where in case of
  ;; #f it might keep what owner/group the file has?
  (let ((uid (if maybe-username-or-id
		 (user-name-or-id->id maybe-username-or-id)
		 (posix:getuid)))
	(gid (if maybe-groupname-or-id
		 (group-name-or-id->id maybe-groupname-or-id)
		 (posix:getgid))))
    (posix:chown path uid gid)))

