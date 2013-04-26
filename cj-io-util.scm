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


(define (backtick cmd . args)
  (let* ((p (open-process (list path: cmd
				arguments: args
				stdout-redirection: #t)))
	 (output (read-line p #f)))
    (close-port p)
    (assert (zero? (process-status p)))
    (chomp output)))

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

