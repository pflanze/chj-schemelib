
;; for macro-write-char
;;(include "~~/lib/_io#.scm")
;; no, instead:


;; _gambit#.scm
(##define-macro (macro-slot index struct . val)
  (if (null? val)
    `(##vector-ref ,struct ,index)
    `(##vector-set! ,struct ,index ,@val)))

;; _thread#.scm
(##define-macro (macro-btq-owner node)           `(macro-slot 7 ,node))

(##define-macro (macro-mutex-unlocked-not-abandoned-and-not-multiprocessor? mutex)
  `(##not (macro-btq-owner ,mutex)))

;; _io#.scm
(define-type port
  id: 2babe060-9af6-456f-a26e-40b592f690ec
  type-exhibitor: macro-type-port
  constructor: macro-make-port
  implementer: implement-type-port
  macros:
  prefix: macro-
  opaque:
  unprintable:

  extender: define-type-of-port

  mutex              ;; access to the port is controlled with this mutex

  rkind              ;; port kind for reading (none-port if can't read)
  wkind              ;; port kind for writing (none-port if can't write)

  name               ;; procedure which returns the name of the port
  read-datum         ;; procedure to read a datum
  write-datum        ;; procedure to write a datum
  newline            ;; procedure to write a datum separator
  force-output       ;; procedure to force output to occur on target device
  close              ;; procedure to close the port
  roptions           ;; options for reading (buffering type, encoding, etc)
  rtimeout           ;; time at which a read that would block times out
  rtimeout-thunk     ;; thunk called when a read timeout occurs
  set-rtimeout       ;; procedure to set rtimeout and rtimeout-thunk
  woptions           ;; options for writing (buffering type, encoding, etc)
  wtimeout           ;; time at which a write that would block times out
  wtimeout-thunk     ;; thunk called when a write timeout occurs
  set-wtimeout       ;; procedure to set wtimeout and wtimeout-thunk
)

(define-type-of-port character-port
  id: 85099702-35ec-4cb8-ae55-13c4b9b05d10
  type-exhibitor: macro-type-character-port
  constructor: macro-make-character-port
  implementer: implement-type-character-port
  macros:
  prefix: macro-
  opaque:
  unprintable:

  extender: define-type-of-character-port

  rbuf               ;; character read buffer (a string)
  rlo                ;; low pointer (start of unread characters)
  rhi                ;; high pointer (end of unread characters)
  rchars             ;; number of characters read at start of read buffer
  rlines             ;; number of lines read up to low pointer
  rcurline           ;; absolute character position where current line starts
  rbuf-fill          ;; procedure to read characters into the read buffer
  peek-eof?          ;; peeking the next character should return end-of-file?

  wbuf               ;; character write buffer (a string)
  wlo                ;; low pointer (start of unwritten characters)
  whi                ;; high pointer (end of unwritten characters)
  wchars             ;; number of characters written at start of write buffer
  wlines             ;; number of lines written up to high pointer
  wcurline           ;; absolute character position where current line starts
  wbuf-drain         ;; procedure to write characters from the write buffer

  input-readtable    ;; readtable for reading
  output-readtable   ;; readtable for writing
  output-width       ;; procedure to get the output width in characters
)

(##define-macro (macro-port-mutex-unlocked-not-abandoned-and-not-multiprocessor? port)
  `(macro-mutex-unlocked-not-abandoned-and-not-multiprocessor? (macro-port-mutex ,port)))

(##define-macro
 (macro-write-char c port)
 `(let ((c ,c)
	(port ,port))

    (##declare (not interrupts-enabled)
	       (not safe) (fixnum)
	       (block)
	       (standard-bindings)
	       (extended-bindings))

    ;; try to get exclusive access to port and if successful perform
    ;; operation inline

    (if (and (##not (##char=? c #\newline))
	     (macro-port-mutex-unlocked-not-abandoned-and-not-multiprocessor? port))

	(let ((char-wbuf (macro-character-port-wbuf port))
	      (char-whi+1 (##fixnum.+ (macro-character-port-whi port) 1)))
	  (if (##fixnum.< char-whi+1 (##string-length char-wbuf))

	      ;; adding this character would not make the character write
	      ;; buffer full, so add character and increment whi

	      (begin
		(##string-set! char-wbuf (##fixnum.- char-whi+1 1) c)
		(macro-character-port-whi-set! port char-whi+1)
		(##void))

	      ;; the character write buffer would become full, so handle
	      ;; this out-of-line

	      (let ()
		(##declare (interrupts-enabled))
		(##write-char c port))))

	;; end-of-line processing is needed or exclusive access to port
	;; cannot be obtained easily, so handle this out-of-line

	(let ()
	  (##declare (interrupts-enabled))
	  (##write-char c port)))))

;; by CJ
(##define-macro
 (macro-write-char-neverlock c port)
 `(let ((c ,c)
	(port ,port))

    (##declare (not interrupts-enabled)
	       (not safe) (fixnum)
	       (block)
	       (standard-bindings)
	       (extended-bindings))

    ;; try to get exclusive access to port and if successful perform
    ;; operation inline

    (let ((char-wbuf (macro-character-port-wbuf port))
	  (char-whi+1 (##fixnum.+ (macro-character-port-whi port) 1)))
      (if (##fixnum.< char-whi+1 (##string-length char-wbuf))

	  ;; adding this character would not make the character write
	  ;; buffer full, so add character and increment whi

	  (begin
	    (##string-set! char-wbuf (##fixnum.- char-whi+1 1) c)
	    (macro-character-port-whi-set! port char-whi+1)
	    (##void))

	  ;; the character write buffer would become full, so handle
	  ;; this out-of-line

	  (let ()
	    (##declare (interrupts-enabled))
	    (##write-char c port))))))
