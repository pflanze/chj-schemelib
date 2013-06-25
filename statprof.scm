;; statprof.scm -- A statistical profiler for Gambit-C 4.0

;; See the README file for license and usage information.

;; $Id: statprof.scm,v 1.9 2005/03/14 07:35:49 guillaume Exp $

;; public:
;;  (profile-start!)
;;  (profile-stop!)
;;  (write-profile-report string:subdirectory)
;;  (run-with-profile thunk #!optional (profile-name "profile"))

;; ----------------------------------------------------------------------------
;; Profiling & interruption handling

(define statprof:*buckets* #f)
(define statprof:*total* 0)

(define (profile-start!)
  (set! statprof:*buckets* '())
  (set! statprof:*total* 0)
  (##interrupt-vector-set! 1 statprof:profile-heartbeat!))

(define (profile-stop!)
  (##interrupt-vector-set! 1 ##thread-heartbeat!))

;; (define (identify-continuation cont) ; first version
;;   (or (##continuation-creator cont)
;;       'unknown))

(define (statprof:identify-continuation cont) ; second version
  (let ((locat (##continuation-locat cont)))
    (if locat
        (let* ((container (##locat-container locat))
               (file (##container->path container)))
          (if file
              (let* ((filepos (##position->filepos (##locat-position locat)))
                     (line (##fixnum.+ (##filepos-line filepos) 1))
                     (col (##fixnum.+ (##filepos-col filepos) 1)))
                (list file line col))
              'unknown))
        'unknown)))

(define (statprof:profile-heartbeat!)
  (##continuation-capture
   (lambda (cont)
     (##thread-heartbeat!)
     (let ((id (statprof:identify-continuation cont)))
       (if (not (eq? id 'unknown))
           (let ((bucket (assoc (car id) statprof:*buckets*)))
             (set! statprof:*total* (+ statprof:*total* 1))
             (if (not bucket)
                 (begin
                   (set! statprof:*buckets*
			 (cons
			  (cons (car id)
				;; fixme: arbitrary hard limit
				;; on the length of source
				;; files
				(make-vector 5000 0))
			  statprof:*buckets*))
                   (set! bucket (car statprof:*buckets*))))

             (vector-set! (cdr bucket)
                          (cadr id)
                          (+ (vector-ref (cdr bucket)
                                         (cadr id))
                             1))))))))


;; ----------------------------------------------------------------------------
;; Text formatting

(define (statprof:pad-left s l c)
  (let loop ((s (string->list s)))
    (if (< (length s) l)
        (loop (cons c s))
        (list->string s))))


;; ----------------------------------------------------------------------------
;; Palette generation & color formatting

(define (statprof:gradient from to step)
  (let ((inc (map (lambda (x) (/ x step))
                  (map - to from))))

    (let loop ((i 0)
               (acc '()))
      (if (= i step)
          (reverse acc)
          (loop (+ i 1)
                (cons (map
                       (lambda (x o)
                         (round (+ x (* i o))))
                       from
                       inc)
                      acc))))))

(define (statprof:as-rgb col)
  (apply string-append
         (map
          (lambda (x)
            (statprof:pad-left (number->string x 16) 2 #\0))
          col)))

(define statprof:palette
  (list->vector
   (cons '(255 255 255)
         (statprof:gradient '(127 127 255)
                   '(255 127 127)
                   16))))


;; ----------------------------------------------------------------------------
;; Functions to generate the report

(define (write-profile-report profile-name)

  (define (iota1 n)
    (let loop ((n n)
               (l '()))
      (if (>= n 1)
          (loop (- n 1) (cons n l))
          l)))

  (define directory-name (string-append (current-directory)
                                        profile-name
                                        "/"))
  (with-exception-catcher
   (lambda (e)
     ;; ignore the exception, it probably means that the directory
     ;; already existed.  If there's another problem it will be
     ;; signaled later.
     #f)
   (lambda ()
     (create-directory (list path: directory-name
                             permissions: #o755))))

  (let ((max-intensity
         (apply max
                (map
                 (lambda (data)
                   (apply max
                          (vector->list data)))
                 (map cdr statprof:*buckets*)))))

    (for-each
     (lambda (bucket)
       (let ((file (car bucket))
             (data (cdr bucket)))

         (define (get-color n)
           (let ((i (vector-ref data n)))
             (if (= i 0)
                 (statprof:as-rgb (vector-ref statprof:palette 0))
                 (let ((x (* (/ (log (+ 1. i))
                                (ceiling (log max-intensity)))
                             (- (vector-length statprof:palette) 1))))
                   (statprof:as-rgb (vector-ref statprof:palette
                                       (inexact->exact (ceiling x))))))))

         (with-output-to-file (string-append
                               directory-name
                               (path-strip-directory file)
                               ".html")
           (let ((lines (call-with-input-file file
                          (lambda (p) (read-all p read-line)))))
             (lambda ()
               (print
                (statprof:sexp->html
                 `(html
                   (body
                    (table
                     cellspacing: 0
                     cellpadding: 0
                     border: 0
                     style: "font-size: 12px;"
                     ,@(map
                        (lambda (line line#)
                          `(tr
                            (td ,(string-append
                                  (number->string line#)
                                  ": "))
                            ;; (td
                            ;;  align: center
                            ;;  ,(let ((n (vector-ref data line#)))
                            ;;     (if (= n 0)
                            ;;         ""
                            ;;         (string-append "["
                            ;;                        (number->string n)
                            ;;                        "/"
                            ;;                        (number->string statprof:*total*)
                            ;;                        "]"))))

                            (td
                             align: center
                             ,(let ((n (vector-ref data line#)))
                                (if (= n 0)
                                    ""
                                    (string-append
                                     (number->string
                                      (statprof:round% (/ n statprof:*total*)))
                                     "% "))))

                            (td (pre style: ,(string-append
                                              "background-color:#"
                                              (get-color line#))
                                     ,line))))
                        lines
                        (iota1 (length lines)))))))))))))

     statprof:*buckets*))

  (with-output-to-file (string-append directory-name "index.html")
    (lambda ()
      (print
       (statprof:sexp->html
        `(html
          (body
           ,@(map (lambda (bucket)
                    (let ((file-path (string-append
                                      directory-name
                                      (path-strip-directory (car bucket))
                                      ".html")))
                      `(p (a href: ,file-path ,file-path)
                          " ["
                          ,(statprof:round%
                            (/ (apply + (vector->list (cdr bucket)))
                               statprof:*total*))
                          " %]")))
                  statprof:*buckets*))))))))

(define (statprof:round% n)
  (/ (round
      (* 10000 n))
     100.))


(define (run-with-profile thunk #!optional (profile-name "profile"))
  (profile-start!)
  (thunk)
  (profile-stop!)
  (write-profile-report profile-name))


;; ----------------------------------------------------------------------------
;; Included file "html.scm"
;; ----------------------------------------------------------------------------

;; html.scm -- A simple html generator for Gambit-C 4.0

;; Written by Guillaume Germain (germaing@iro.umontreal.ca)
;; This code is released in the public domain.


(define (statprof:stringify x)
  (with-output-to-string ""
    (lambda ()
      (print x))))

(define (statprof:to-escaped-string x)
  (statprof:stringify
   (map (lambda (c)
          (case c
            ((#\<) "&lt;")
            ((#\>) "&gt;")
            ((#\&) "&amp;")
            (else c)))
        (string->list
         (statprof:stringify x)))))

;; Quick and dirty conversion of s-expressions to html
(define (statprof:sexp->html exp)

  ;; write the opening tag
  (define (open-tag exp)
    (cond
     ;; null tag isn't valid
     ((null? exp)
      (error "null tag"))

     ;; a tag must be a list beginning with a symbol
     ((and (pair? exp)
           (symbol? (car exp)))
      (list "<"
            (car exp)
            " "
            (maybe-args (car exp) (cdr exp))))

     (else
      (error "invalid tag" exp))))

  ;; take care of the keywords / arguments
  (define (maybe-args tag exp)

    (cond
     ;; does the rest of the list begins with a keyword
     ((and (pair? exp)
           (keyword? (car exp)))

      ;; does the keyword has an associated value?
      (if (or (null? (cdr exp))
              (keyword? (cadr exp)))
          ;; no, we don't put an associated value
          (list (keyword->string (car exp))
                " "
                (maybe-args tag (cdr exp)))
          ;; yes, we take the next element in the list as the value
          (list (keyword->string (car exp))
                "=\""
                (cadr exp)
                "\" "
                (maybe-args tag (cddr exp)))))

     ;; must just be some content
     (else
      (content tag exp))))

  ;; handle the content of the tag and closing it
  (define (content tag exp)
    (cond
     ;; no content...
     ((null? exp)
      ;;(list "></" tag ">"))           ; close as "<br></br>"
      (list "/>"))                      ; close as "<br/>"

     ;; write the content, handle tags inside
     ((pair? exp)
      (list ">"
            (map (lambda (e)
                   (if (pair? e)
                       (open-tag e)
                       (statprof:to-escaped-string e)))
                 exp)
            "</"
            tag
            ">"))

     ;; non-null terminated list?
     (else
      (error "strange content..."))))

  ;; we rely on Gambit's flattening of list when printed with DISPLAY
  (with-output-to-string ""
                         (lambda ()
                           (print (open-tag exp)))))
