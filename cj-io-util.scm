;;; Copyright 2013-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         (cj-port port-name)
         test
         (cj-path path-string?)
         (keyword-alist keyword-alist:Maybe-ref
                        keyword-alist:delete
                        keyword-alist:set)
         (alist <alist>)
         srfi-11
         srfi-1
         posix/cj-posix ;; posix:environ open close read write seek etc.
         (cj-functional list-of values-of)
         string-bag
         (cj-source source location position))

(export open-process*
        open-input-process*
        open-output-process*
        port-name?
        (method port.name
                port.content)
        eexist-exception?
        eperm-exception?
        read-line/location
        read-lines
        maybe-read-line
        xread-line
        writeln
        pathspec.xcontent
        string.print-file
        xcall-with-input-process
        Xcall-with-input-process
        xxsystem
        xsystem
        01status?
        01system
        xbacktick
        Xbacktick
        01backtick
        backtick
        xbacktick-bash
        backtick-bash
        run-bash
        bash-command ;; for (xbacktick (bash-command ....))--ehr no, neither
        hostname
        xforwardtick
        Xforwardtick
        01forwardtick
        file-info->mtime
        file-basename
        file-mtime
        file-exists-and-newer?
        basepath
        basename
        dirname
        dirname+basename
        port->stream
        directory-item-stream
        file-line-stream
        file-line/location-stream
        file-char/location-stream
        process-line-stream
        process-read-stream
        user-name-or-id->id
        group-name-or-id->id
        chown
        possibly-create-directory
        putfile
        getfile
        partial-copy-file)


;; handle setenv:-enriched process specs:

(def (env-alist:key+val.key #(string? s))
     (letv ((k maybe-v)
            (string-split-once s #\= #t))
           k))

(def (environ-key? v)
     (and (string? v)
          (not (string-empty? v))
          (not (string-contains? v "="))))

(modimport/prefix env-alist:
                  (<alist> string?
                           env-alist:key+val.key
                           string=?))

(def (process*-alist-expand spec *environ)
     (Maybe:cond
      ((keyword-alist:Maybe-ref spec setenv:)
       => (lambda (entry)
            (let ((k+v-s (-> (list-of (values-of environ-key? string?))
                             (cdr entry))))
              (keyword-alist:set
               (keyword-alist:delete spec setenv:)
               (cons environment:
                     (fold-right
                      (lambda (k+v env)
                        (letv ((k v) k+v)
                              (env-alist:set env
                                             (string-append k "=" v))))
                      (*environ)
                      k+v-s))))))
      (else
       spec)))

(TEST
 > (process*-alist-expand '((foo: . "bar") (baz: . "bum"))
                          (C error "bug"))
 ((foo: . "bar") (baz: . "bum")))


(def (process-list.alist l)
     (if (null? l)
         l
         (let-pair ((k l) l)
                   (let-pair ((v l) l)
                             (cons (cons k v)
                                   (process-list.alist l))))))

(def (process-alist.list l)
     (if (null? l)
         l
         (let-pair ((k+v l) l)
                   (let-pair ((k v) k+v)
                             (cons* k v
                                    (process-alist.list l))))))


(def (process*-spec-expand spec *environ)
     (if (string? spec)
         spec
         (process-alist.list
          (process*-alist-expand (process-list.alist spec)
                                 *environ))))

(TEST
 > (process*-spec-expand "foo" (C error "bug"))
 "foo"
 > (def (env)
        '("PATH=a:b:c" "BAR=2" "CWD=/x/y"))
 > (process*-spec-expand '(foo: "bar" baz: "bum") env)
 (foo: "bar" baz: "bum")
 > (process*-spec-expand
    (list setenv: (list (values "FOO" "bar")
                        (values "BAR" "baz"))
          baz: "bum")
    env)
 (environment: ("FOO=bar" "PATH=a:b:c" "BAR=baz" "CWD=/x/y")
               baz: "bum"))


(def (open-process* spec)
     (open-process (process*-spec-expand spec posix:environ)))

(def (open-input-process* spec)
     (open-input-process (process*-spec-expand spec posix:environ)))

(def (open-output-process* spec)
     (open-output-process (process*-spec-expand spec posix:environ)))


(def port-name? (either path-string? pair?))

(def. (port.name #(port? p)) -> port-name?
  "Return the name associated with `p`; if `p` was opened
from a file, this is the path string. In other cases it is a list with
some informal structure describing what the port was opened from."
  (##port-name p))

(def. (port.content p)
  (read-line p #f))

;; XX still that hack of hard-coding constants
(def (eexist-exception? v)
     (and (os-exception? v)
          (= (os-exception-code v) -515899375)))
(def (eperm-exception? v)
     (and (os-exception? v)
          (= (os-exception-code v) -515899379)))


(define (read-lines #!optional (p (current-input-port)) (tail '()))
  (let rec ()
    (let ((line (read-line p)))
      (if (eof-object? line)
          tail
          (cons line (rec))))))

(define (maybe-read-line . args)
  (let ((v (apply read-line args)))
    (if (eof-object? v)
        #f
        v)))

(define (read-line/location port)
  (let ((line (read-line port)))
    (if (eof-object? line)
        line
        (source line
                (location (port-name port)
                          (position (input-port-line port)
                                    1))))))


;; would preferably use read-line as the name, but, better don't
;; confuse Scheme users.
(define (xread-line p . args)
  (let ((v (apply read-line p args)))
    (if (eof-object? v)
        (error "xread-line: got EOF reading from " p)
        v)))


(define (writeln obj #!optional maybe-port)
  (let ((port (or maybe-port (current-output-port))))
    (write obj port)
    (newline port)))

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
  (__xcall-with-process open-input-process* close-input-port))

(define _xcall-with-output-process
  (__xcall-with-process open-output-process* close-output-port))

(define _xcall-with-process
  (__xcall-with-process open-process* close-port))


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

;; XX: mis-nomer too? As C's system does invoke a shell. (This is
;; Perl's multi-argument "system".) (Hmm to be fair, "system" as "run
;; from the system" does kind of sound fair, though. Call it
;; "call-system" or something? Not really "run-system" as that implies
;; some 'monadic' input format, like a code string (see run-bash); a
;; single command is not running through a sequence of steps.)
(define xxsystem (_system zero?))

;; XX: and more stupid naming; this is really for compatibility with
;; my Chj Perl stuff. Looking for better names.
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


(define (bash-command str . args)
  (list path: "bash"
        arguments: (cons* "-c" str args)))

(TEST
 > (bash-command "true" "foo" "bar")
 (path: "bash" arguments: ("-c" "true" "foo" "bar")))


(define (xbacktick-bash code)
  (xbacktick "bash" "-c" code))

;; 'x' for only 1 return value, no 'x' for both. That seems sensible
;; to me. I don't want to say 'X' and it's even still a different
;; return protocol. (XX: change Xbacktick vs. backtick naming?)
(define (backtick-bash code)
  (Xbacktick "bash" "-c" code))

(define (run-bash code)
  (xsystem "bash" "-c" code))

(define (hostname)
  (xbacktick "hostname"))


;; the somewhat inverse of backtick: feed a string to a process.
;; adapted COPY-PASTE from _backtick
(define (_forwardtick status-ok? cont)
  (let ((xcall (_xcall-with-output-process
                status-ok?
                (lambda (value s)
                  s)
                _error-exited-with-error-status)))
    (lambda (cmd . args)
      ;; nice usage for explicit "currying"
      (lambda (str)
        (xcall (list path: cmd
                     arguments: args
                     stdin-redirection: #t
                     stdout-redirection: #f
                     char-encoding: 'UTF-8)
               (lambda_
                (display str _)))))))



(define xforwardtick (_forwardtick zero? (lambda (out s) out)))
(define Xforwardtick (_forwardtick true/1 (lambda (out s) out)))
(define 01forwardtick (_forwardtick 01status? (lambda (out s) out)))


(TEST
 > ((xforwardtick "tr" "a" "e") "Hallo\n")
 0 ;; and prints "Hello" to stdout
 > (%try ((xforwardtick "false") ""))
 (exception
  text:
  "process exited with error status:\n256\n(path: \"false\" arguments: () stdin-redirection: #t stdout-redirection: #f ch...\n")
 > ((Xforwardtick "false") "")
 256
 > ((01forwardtick "false") "")
 256
 > (%try ((Xforwardtick "falsewefwefef") ""))
 (exception
  text:
  "No such file or directory\n(open-output-process\n '(path: \"falsewefwefef\" arguments: () stdin-redirection: #t stdout-redirect...\n)\n"))






;; where should that be moved to?
(define file-info->mtime
  (compose-function time->seconds
           file-info-last-modification-time))
;;^ also now see file-info.mtime in oo-gambit.scm

(define (file-mtime path)
  (time->seconds
   (file-info-last-modification-time
    (file-info path))))

(define (file-exists-and-newer? existingfile newfile)
  (and (file-exists? newfile)
       (> (file-mtime newfile)
          (file-mtime existingfile))))


;; -- for a different libary?  not full path lib but simple path manipul

;; a basename that behaves like the shell util, not like
;; name->basename+maybe-suffix . One-argument form only, though, for
;; now.

(def (file-basename path) -> path-string?
     (last (string-split path #\/)))

(TEST
 > (file-basename "/foo/bar.scm")
 "bar.scm"
 > (file-basename "bar.scm")
 "bar.scm"
 ;; > (file-basename "/foo/")
 ;; ""
 > (%try-error (file-basename "/foo/"))
 #(error "value fails to meet predicate:" (path-string? ""))
 ;; XX ok? sigh.
 )

(def (basepath #(path-string? n)
               suffixS
               #!optional insensitive?)
     -> path-string?
     (if suffixS
         (cond ((improper-find (C string-ends-with? n _ insensitive?)
                               suffixS)
                => (lambda (suffix)
                     (substring n 0 (- (string-length n)
                                       (string-length suffix)))))
               (else n))
         n))

(TEST
 > (basepath "/.foo" ".foo")
 "/" ;; XX problematic already?
 > (%try-error (basepath ".foo" ".foo"))
 #(error "value fails to meet predicate:" (path-string? ""))
 > (%try-error (basepath "..foo" ".foo"))
 "." ;; XX problematic, too? Should really work on structured paths?
 )

(def (basename path #!optional suffixS insensitive?) -> path-string?
     (basepath (file-basename
                (if (string-ends-with? path "/")
                    (substring path 0 (dec (string-length path)))
                    path))
               suffixS
               insensitive?))

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
 ;; improper list feature:
 > (basename "/foo/bar.scm" '(".scm" ".txt"))
 "bar"
 > (basename "/foo/bar.txt" '(".scm" ".txt"))
 "bar"
 > (basename "/foo/bar.txt" '(".scm" ".TXT"))
 "bar.txt"
 > (basename "/foo/bar.txt" '(".scm" ".TXT") #t)
 "bar")


(define dirname-slow
  (cut xbacktick "dirname" <>))

;; (define dirname-fast
;;   ;; (strings-join (reverse (cdr (reverse (string-split path #\/)))))
;;   ;; well no.
;;   ;;..sigh
;;   )

(define dirname dirname-slow)


(define (dirname+basename path)
  ;; XX optimize
  (values (dirname path)
          (basename path)))


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



(define (port->stream p read close-port #!optional strip-BOM?)
  ;; BOM=Byte order mark (U+FEFF), a Unicode character
  (let rec ((no 0))
    (delay
      (let ((item (read p)))
        (if (eof-object? item)
            (begin
              (close-port p)
              '())
            (cons (if (zero? no)
                      (let* ((str (source-code item))
                             (len (string-length str)))
                        (if (> len 0)
                            (if (char=? (string-ref str 0) #\xFEFF)
                                (possibly-sourcify (substring str 1 len)
                                                   item)
                                item)
                            item))
                      item) (rec (inc no))))))))

(define (directory-item-stream dir)
  (port->stream (open-directory dir)
                read
                close-port))

(define (directory-path-stream dir)
  (port->stream (open-directory dir)
                (lambda (port)
                  (let ((item (read port)))
                    (if (eof-object? item)
                        item
                        (path-append dir item))))
                close-port))

(define (file-line-stream file)
  (port->stream (open-input-file file)
                read-line
                close-port
                #t))

(define (file-line/location-stream file)
  (port->stream (open-input-file file)
                read-line/location
                close-port
                #t))

(define (file-char/location-stream file)
  (let ((p (open-input-file file)))
    (let rec ((line 1)
              (col 1))
      (delay
        ;; XX is this the one without mutex locking?
        (let ((c (##read-char p)))
          (if (eof-object? c)
              (begin
                (close-port p)
                '())
              (cons (source c (location file (position line col)))
                    (if (or (eq? c #\return)
                            (eq? c #\newline))
                        (rec (fx+ line 1)
                             1)
                        (rec line
                             (fx+ col 1)))))))))) 

(define (process-status-assert-zero process)
  (lambda (status)
    (when (not (zero? status))
          (error "process exited with non-zero status:"
                 process))))

(define (make-close-and-assert status-handler)
  (lambda (p)
    (close-port p)
    (status-handler (process-status p))))

(define (process-line-stream
         process
         #!key
         (status-handler (process-status-assert-zero process)))
  (port->stream (open-input-process* process)
                read-line
                (make-close-and-assert status-handler)))

(define (process-read-stream
         process
         #!key
         (status-handler (process-status-assert-zero process)))
  (port->stream (open-input-process* process)
                read
                (make-close-and-assert status-handler)))


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



(define (possibly-create-directory path)
  (with-exception-catcher
   (lambda (e)
     (if (eexist-exception? e)
         #f
         (raise e)))
   (lambda ()
     (create-directory path)
     #t)))

(TEST
 > (def cj-io-util:testbase ".cj-io-util:testbase")
 > (possibly-create-directory cj-io-util:testbase)
 #t
 > (possibly-create-directory cj-io-util:testbase)
 #f
 > (possibly-create-directory cj-io-util:testbase)
 #f
 > (delete-directory cj-io-util:testbase))


;; first bag then path, so that it works nicely with =>
(def (putfile bag path)
     (call-with-output-file path
       (lambda (port)
         (string-bag-display bag port))))


(def (getfile path)
     (call-with-input-file path
       (lambda (port)
         (read-line port #f))))


;; unlike Gambit's copy-file this copies a part of the file, also, it
;; doesn't request any data outside the given range (which may be
;; important if there are read errors). For now only use one buffer,
;; assume we can deal with that?
;; XX Oh, should it use Result.scm instead of exceptions?
(def (partial-copy-file #(path-string? from-path)
                        #(path-string? to-path)
                        #(natural0? from-byte)
                        #(natural0? to-byte))
     (let* ((len (-> natural0? (- to-byte from-byte)))
            (buf (make-u8vector len))
            (in (posix:open from-path (bitwise-or O_RDONLY)))
            ;; XX option for permissions?
            (out (posix:open to-path (bitwise-or O_CREAT O_WRONLY) #o666)))
       ;; XX error condition file too short? wait would EXTEND it? or
       ;; RDONLY prevents this?
       (posix:lseek in from-byte SEEK_SET)
       (let ((nread (posix:read-u8vector in buf len)))
         (posix:close in)
         ;; XX another error condition, file too short or (forever!) EINTR
         (assert (= nread len))
         ;; XX another EINTR case?
         (assert (= (posix:write-u8vector out buf len) len))
         (posix:close out))))

