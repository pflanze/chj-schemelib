;;; Copyright 2017-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; included in dot-oo, hence no require form. pseudo export form:
(export dot-oo:method-key-maybe-ref-i
        dot-oo:method-table-maybe-ref-method
        dot-oo:method-table-maybe-ref-columnS
        dot-oo:method-table-maybe-ref-prefix-columnS
        dot-oo:method-table-set!
        dot-oo:new-method-table
        dot-oo:show-method-table-entry?
        dot-oo:show-method-table
        methodtablevector?
        methodtable?
        #!optional
        dot-oo:method-table-set)


;; Method tables consist of a vector in a box (so that replaceable) in
;; the format (unused fields so that the number of entries is
;; derivable cheaply from the vector-length via shifting):

;; (vector prefix1 prefix2 prefix3
;;         pred1 pred2 pred3
;;         method1 method2 method3
;;         stats1 stats2 stats3
;;         location1 location2 location3
;;         unused1 unused2 unused3
;;         unused1 unused2 unused3
;;         unused1 unused2 unused3)


(define-macro (col:prefix) 0) ;; type name
(define-macro (col:pred) 1) ;; type predicate
(define-macro (col:method) 2)

(define-macro (col:stats) 3)
;; ^ only updated if *dot-oo:method-stats* is true. See
;;   dot-oo:show-method-table-entry?.

(define-macro (col:location) 4)
;; ^ the location of the def. or similar definition.


(define-inline (@methodtablevector-nentries->length nentries)
  (declare (fixnum) (not safe))
  (arithmetic-shift nentries 3))

(define-inline (@methodtablevector-length->nentries len)
  (declare (fixnum) (not safe))
  (arithmetic-shift len -3))

(define-inline (@methodtablevector-nentries t)
  (declare (fixnum) (not safe))
  (@methodtablevector-length->nentries (vector-length t)))

(define-typed (methodtablevector-nentries [vector? t])
  (@methodtablevector-nentries t))

(define (methodtablevector? v)
  (and (vector? v)
       (let ((len (vector-length v)))
         (=> (@methodtablevector-length->nentries len)
             @methodtablevector-nentries->length
             (= len)))))

(define methodtable? (box-of methodtablevector?))

(TEST
 > (def l '([] [1] [a b c d]
            [a b c d e f g h]
            (a b c d e f g h)
            [a b c d e f g h 1 2 3 4 5 6 7 8]
            [a b c d e f g h 1 2 3 4 5 6 7 8 9]
            [a b c d e f g h 1 2 3 4 5 6 7]))
 > (map methodtablevector? l)
 (#t #f #f #t #f #t #f #f)
 > (def l* (map box l))
 > (map methodtablevector? l*)
 (#f #f #f #f #f #f #f #f)
 > (map methodtable? l*)
 (#t #f #f #t #f #t #f #f))


(define (vector-copy! fromvec from to tovec tofrom)
  (let* ((offset (fx- tofrom from)))
    (for..< (i from to)
            (vector-set! tovec (fx+ i offset)
                         (vector-ref fromvec i)))))

(TEST
 > (define v0 (vector 'a 'b 'c 'd))
 > (define v1 (vector 'x 'y 'z))
 > (vector-copy! v1 1 3 v0 1)
 > v0
 #(a y z d))

;; /move


(define (dot-oo:method-key-maybe-ref-i vec n key)
  (let ((end n))
    (let lp ((i 0))
      (if (< i end)
          (if (eq? (vector-ref vec i) key)
              i
              (lp (inc i)))
          #f))))


(define *dot-oo:method-trace* #f)
(set! *dot-oo:method-trace* #f)

;; #t = collect call count (cheap), location = table with location ->
;; call count (expensive)
(define *dot-oo:method-stats* #f)
(set! *dot-oo:method-stats* #f)

;; How many levels of stack frames to retrieve (and map to locations)
;; if *dot-oo:method-stats* is set to 'location
(define *dot-oo:method-stats-locations-depth* #f)
(set! *dot-oo:method-stats-locations-depth* 2)


(define (@dot-oo:method-type-maybe-ref-method vec nentries obj)
  (declare (block)
           (standard-bindings)
           (extended-bindings)
           (fixnum) (not safe))
  ;; remember, columns in the 'matrix' are laid out "horizontally";
  ;; start iterating over the vector at the first entry of the pred
  ;; column.
  (let ((end (+ nentries nentries)))
    (let lp ((i nentries))
      (if (< i end)
          (if
           ( ;; XX should function call be safe here?
            (vector-ref vec i)
            obj)
           (let ((m (vector-ref vec (+ i nentries))))
             (when *dot-oo:method-trace*
                   (warn "method call for:"
                         ;; prefix:
                         (vector-ref vec (- i nentries))
                         ;; Don't have access to the genericname, can
                         ;; only do:
                         m))
             (let ((mode *dot-oo:method-stats*))
               (case mode
                 ((#f) m)
                 ((#t)
                  (let ((j (+ i (* 2 nentries))))
                    ;; ^ not 3 as i is already in second row
                    (vector-set! vec j
                                 (+ (vector-ref vec j) 1)))
                  m)
                 ((location continuation)
                  (continuation-capture
                   (lambda (cont)
                     (let ((j (+ i (* 2 nentries)))) ;; COPY-PASTE
                       ;; ^ not 3 as i is already in second row
                       (let ((t (let ((v (vector-ref vec j)))
                                  (if (table? v)
                                      v
                                      ;; simply throw away previous count. :/
                                      (let ((t (make-table)))
                                        (vector-set! vec j t)
                                        t))))
                             (key (continuation-carp:locations-outside-filename
                                   cont "dot-oo.scm"
                                   *dot-oo:method-stats-locations-depth*))
                             (maybe-cont (if (eq? mode 'continuation)
                                             cont
                                             #f)))
                         (cond
                          ((table-ref t key #f)
                           => (lambda (n+cont)
                                (set-car! n+cont (inc (car n+cont)))
                                (set-cdr! n+cont maybe-cont)))
                          (else
                           (table-set! t key (cons 1 maybe-cont))))))
                     m)))
                 (else
                  (error "invalid value of *dot-oo:method-stats*"
                         *dot-oo:method-stats*)
                  m))))
           (lp (inc i)))
          #f))))

(define (dot-oo:method-table-maybe-ref-method tbl obj)
  (let* ((vec (unbox tbl))
         (nentries (methodtablevector-nentries vec)))
    (@dot-oo:method-type-maybe-ref-method vec nentries obj)))


;; These two don't need to be fast, just used for introspection

(define (dot-oo:method-table-maybe-ref-col-columnS tbl col match? colnumS)
  "scan `col` in `tbl` until `match?` returns true of its value,
return values at `colnumS`. Returns #f if there's no match."
  (let* ((vec (unbox tbl))
         (nentries (methodtablevector-nentries vec)))
    ;; adapted partial copy-paste of
    ;; @dot-oo:method-type-maybe-ref-method, see docs there; now
    ;; further modified to use `col` and `match?`
    (let* ((start (fx* nentries col))
           (end (+ start nentries)))
      (let lp ((i start))
        (if (< i end)
            (if (match? (vector-ref vec i))
                (improper-map (lambda (colnum)
                                (vector-ref
                                 vec
                                 (+ (- i start) (* nentries colnum))))
                              colnumS)
                (lp (inc i)))
            #f)))))

(define (dot-oo:method-table-maybe-ref-columnS tbl obj colnumS)
  (dot-oo:method-table-maybe-ref-col-columnS
   tbl
   (col:pred)
   (lambda (pred) (pred obj))
   colnumS))

(define (dot-oo:method-table-maybe-ref-prefix-columnS tbl prefix colnumS)
  (dot-oo:method-table-maybe-ref-col-columnS
   tbl
   (col:prefix)
   (lambda (p) (eq? p prefix))
   colnumS))



;; Disable all assertments for production mode? Can we have full
;; testing instead?
(define-typed (dot-oo:copy-rail! raili
                                 old #(natural0? oldn)
                                 new #(natural0? newn)
                                 ;; and these are indexes within an
                                 ;; individual rail:
                                 #(natural0? oldfrom)
                                 #(natural0? oldto)
                                 #(natural0? newfrom))
  (assert (<= oldto oldn))
  (assert (<= (fx+ newfrom (fx- oldto oldfrom)) newn))
  (assert (<= oldfrom oldto)) ;; XX could relax?
  (let ((oldbase (fx* raili oldn)))
    (vector-copy! old
                  (fx+ oldbase oldfrom)
                  (fx+ oldbase oldto)
                  new
                  (fx+ (fx* raili newn) newfrom))))

(define (dot-oo:copy-rail raili old oldn new newn
                          oldfrom oldto newfrom)
  (let ((new (vector-copy new)))
    (dot-oo:copy-rail! raili old oldn new newn
                       oldfrom oldto newfrom)
    new))


(TEST
 > (define orig '#(baz 0 0 0 0 0 0 0 0 0 0 0))
 > (dot-oo:copy-rail
    0
    '#(a foo baz a? foo? baz? a.bar foo.bar baz.bar 0 0 0)
    3
    orig
    3
    0
    1
    1)
 ;; rail 0 is [a foo baz]. 0..1 is a, out 1 
 #(baz a 0 0 0 0 0 0 0 0 0 0)
 > (dot-oo:copy-rail
    0
    '#(a foo baz a? foo? baz? a.bar foo.bar baz.bar 0 0 0)
    3
    orig
    3
    0
    2
    1)
 #(baz a foo 0 0 0 0 0 0 0 0 0)
 > (%try (dot-oo:copy-rail
          0
          '#(a foo baz a? foo? baz? a.bar foo.bar baz.bar 0 0 0)
          3
          orig
          3
          0
          2
          2))
 (exception text: "assertment failure: (<= (fx+ newfrom (fx- oldto oldfrom)) newn) (<= (fx+ 2 (fx- 2 0)) 3)\n")
 > (dot-oo:copy-rail
    1
    '#(a foo baz a? foo? baz? a.bar foo.bar baz.bar 0 0 0)
    3
    orig
    3
    0
    2
    1)
 #(baz 0 0 0 a? foo? 0 0 0 0 0 0)
 > (dot-oo:copy-rail
    2
    '#(a foo baz a? foo? baz? a.bar foo.bar baz.bar 0 0 0)
    3
    orig
    3
    0
    2
    1)
 #(baz 0 0 0 0 0 0 a.bar foo.bar 0 0 0)
 > (dot-oo:copy-rail
    3
    '#(a foo baz a? foo? baz? a.bar foo.bar baz.bar 0 0 0)
    3
    orig
    3
    0
    2
    1)
 #(baz 0 0 0 0 0 0 0 0 0 0 0)
 > (%try (dot-oo:copy-rail
          4
          '#(a foo baz a? foo? baz? a.bar foo.bar baz.bar 0 0 0)
          3
          orig
          3
          0
          2
          1))
 (exception
  text:
  "(Argument 2) Out of range\n(vector-ref '[a foo baz a? foo? baz? a.bar foo.bar baz.bar 0 0 0] 12)\n"))



(both-times
 (define (nonempty-symbol? v)
   (and (symbol? v)
        (not (zero? (string-length (symbol->string v)))))))

(define-typed (dot-oo:method-table-set old
                                       [nonempty-symbol? prefix]
                                       pred
                                       meth
                                       location)

  (define (finish new n)
    (vector-set! new 0 prefix)
    (vector-set! new n pred)
    (vector-set! new (fx* n 2) meth)
    (vector-set! new (fx* n 4) location)
    new)

  (let* ((oldlen (vector-length old))
         (oldn (@methodtablevector-length->nentries oldlen)))
    (cond ((dot-oo:method-key-maybe-ref-i old oldn prefix)
           => (lambda (oldi)
                (let* ((n oldn)
                       (new (make-vector oldlen)))

                  (define (do-rail raili)
                    (let ((oldi* (inc oldi)))
                      ;; copy part after found position (unshifted):
                      (dot-oo:copy-rail! raili old oldn new n
                                         oldi* oldn oldi*)
                      ;; copy part before found position towards the end
                      ;; to make space for new row at the beginning:
                      (dot-oo:copy-rail! raili old oldn new n 0 oldi 1)))

                   ;; these are used when setting an entry again that
                   ;; was already set
                  (do-rail 0)
                  (do-rail 1)
                  (do-rail 2)
                  (do-rail 3)
                  (do-rail 4)
                  (finish new n))))
          (else
           ;; prepend to the top
           (let* ((n (inc oldn))
                  (new (make-vector (@methodtablevector-nentries->length n))))
             (dot-oo:copy-rail! 0 old oldn new n 0 oldn 1)
             (dot-oo:copy-rail! 1 old oldn new n 0 oldn 1)
             (dot-oo:copy-rail! 2 old oldn new n 0 oldn 1)
             (dot-oo:copy-rail! 3 old oldn new n 0 oldn 1)
             (dot-oo:copy-rail! 4 old oldn new n 0 oldn 1)
             (finish new n))))))

(TEST
 > (dot-oo:method-table-set (vector) 'foo 'foo? 'foo.bar 'fooloc)
 [foo foo? foo.bar 0 fooloc 0 0 0]
 > (dot-oo:method-table-set # 'baz 'baz? 'baz.bar 'bazloc)
 [
  baz foo
  baz? foo?
  baz.bar foo.bar
  0 0
  bazloc fooloc
  0 0
  0 0
  0 0]
 > (dot-oo:method-table-set # 'foo 'foo? 'foo.bar 'fooloc2)
 [
  foo baz
  foo? baz?
  foo.bar baz.bar
  0 0
  fooloc2 bazloc
  0 0
  0 0
  0 0]
 > (define a
     (dot-oo:method-table-set # 'a 'a? 'a.bar 'aloc))
 > a
 [
  a foo baz
  a? foo? baz?
  a.bar foo.bar baz.bar
  0 0 0
  aloc fooloc2 bazloc
  0 0 0
  0 0 0
  0 0 0]
 > (dot-oo:method-table-set a 'foo 'foo? 'foo.bar 'fooloc-a)
 [
  foo a baz
  foo? a? baz?
  foo.bar a.bar baz.bar
  0 0 0
  fooloc-a aloc bazloc
  0 0 0
  0 0 0
  0 0 0]
 > (dot-oo:method-table-set a 'baz 'baz? 'baz.bar 'bazloc)
 [
  baz a foo
  baz? a? foo?
  baz.bar a.bar foo.bar
  0 0 0
  bazloc aloc fooloc2
  0 0 0
  0 0 0
  0 0 0])


(define (dot-oo:method-table-set! tbl prefix pred meth loc)
  (set-box! tbl (dot-oo:method-table-set (unbox tbl) prefix pred meth loc))
  ;; still return it to allow for easier 'define-once' approach:
  tbl)


(define (dot-oo:new-method-table)
  (box '#()))


(define dot-oo:show-method-table-entry?
  (inhomogenous-list-of symbol? ;; the type name (predicate w/o "?")
                        procedure? ;; predicate
                        procedure? ;; method implementor
                        (either
                         ;; call count if collection was on: It is
                         ;; either the call count (a fixnum) if
                         ;; *dot-oo:method-stats* is #t, or if
                         ;; *dot-oo:method-stats* is 'location or
                         ;; 'continuation, a table mapping a caller
                         ;; location list (of up to depth
                         ;; *dot-oo:method-stats-locations-depth*) to
                         ;; a pair of the call count and maybe (in the
                         ;; 'location mode) the last continuation
                         ;; encountered in that case.
                         exact-natural0?
                         table?)))

(define-typed (dot-oo:show-method-table t)
  -> (list-of dot-oo:show-method-table-entry?)
  (let* ((v (unbox t))
         (n (methodtablevector-nentries v)))
    (unfold (C >= _ n)
            (lambda (i)
              (list (vector-ref v i)
                    (vector-ref v (+ i n))
                    (vector-ref v (+ i (* 2 n)))
                    (vector-ref v (+ i (* 3 n)))))
            inc-function
            0)))
