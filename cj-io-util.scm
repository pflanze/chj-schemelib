;;; Copyright 2013-2014 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


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


(define (_call-with-input-process parms proc)
  (let* ((p (open-input-process parms))
	 (res (proc p)))
    (close-input-port p)
    (let ((s (process-status p)))
      (values res s))))

(define (xcall-with-input-process parms proc)
  (letv ((res s) (_call-with-input-process parms proc))
	(if (zero? s)
	    res
	    (error "process exited with non-zero status:"
		   s
		   parms))))

;; XX: rewrite xcall-* and xxsystem to use it?
(define (xxsystem cmd . args)
  (let* ((p (open-process (list path: cmd
				arguments: args
				stdin-redirection: #f
				stdout-redirection: #f))))
    (close-input-port p)
    (assert (zero? (process-status p)))))

(define (backtick cmd . args)
  (let* ((output (xcall-with-input-process (list path: cmd
						 arguments: args
						 stdout-redirection: #t
						 char-encoding: 'UTF-8)
					   (lambda (p)
					     (read-line p #f)))))
    (if (eof-object? output) ;; stupid lib
	""
	(chomp output))))

(TEST
 > (backtick "true")
 ""
 > (%try-error (backtick "false"))
 #(error "process exited with non-zero status:"
	 256 (path: "false" arguments: () stdout-redirection: #t char-encoding: UTF-8))
 > (backtick "echo" "world")
 "world"
 ;; check that unicode is read as such:
 > (backtick "echo" "-e" "Mot\\xc3\\xb6rhead")
 "Mot\366rhead"
 ;; and check that it gets 'correctly' to the process, too:
 ;; > (backtick "echo" "Motörhead")
 ;; "Mot\366rhead"
 ;; XXX: Gambit passes the argument as latin1, *and* then silently cuts off the latin1 result to "Mot"
 )



(define (backtick-bash code)
  (backtick "bash" "-c" code))

(define bash backtick-bash) ;; ok?

(define (hostname)
  (backtick "hostname"))

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

(define basename
  (compose* file-basename
	    list->string
	    (cut list-trim-right <> (cut char=? <> #\/))
	    string->list))

(TEST
 > (basename "/foo/bar.scm")
 "bar.scm"
 > (basename "bar.scm")
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
  (cut backtick "dirname" <>))

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

