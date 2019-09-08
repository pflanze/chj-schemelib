;;; Copyright 2017-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; included in dot-oo, hence no require form. pseudo export form:
(export dot-oo:method-key-maybe-ref-i
        dot-oo:method-table-maybe-ref-method
        dot-oo:method-table-maybe-ref-columnS
        dot-oo:method-table-set!
        dot-oo:new-method-table
        dot-oo:show-method-table-entry?
        dot-oo:show-method-table
        #!optional
        dot-oo:method-table-set)


;; Method tables consist of a vector in a box (so that replaceable) in
;; the format:

;; (vector prefix1 prefix2 prefix3
;;         pred1 pred2 pred3
;;         method1 method2 method3
;;         stats1 stats2 stats3)

;; stats field is only updated if *dot-oo:method-stats* is true. See
;; dot-oo:show-method-table-entry?.


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
             (case *dot-oo:method-stats*
               ((#f) m)
               ((#t)
                (let ((j (+ i (* 2 nentries))))
                  ;; ^ not 3 as i is already in second row
                  (vector-set! vec j
                               (+ (vector-ref vec j) 1)))
                m)
               ((location)
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
                                 *dot-oo:method-stats-locations-depth*)))
                       (table-set! t key
                                   (inc (table-ref t key 0)))))
                   m)))
               (else
                (error "invalid value of *dot-oo:method-stats*"
                       *dot-oo:method-stats*)
                m)))
           (lp (inc i)))
          #f))))

(define (dot-oo:method-table-maybe-ref-method tbl obj)
  (let* ((vec (unbox tbl))
         (nentries (arithmetic-shift (vector-length vec) -2)))
    (@dot-oo:method-type-maybe-ref-method vec nentries obj)))


;; This one doesn't need to be fast, just used for introspection:
;; still find the entry first, then get whatever column valueS is/are
;; asked for
(define (dot-oo:method-table-maybe-ref-columnS tbl obj colnumS)
  (let* ((vec (unbox tbl))
         (nentries (arithmetic-shift (vector-length vec) -2)))
    ;; adapted partial copy-paste of
    ;; @dot-oo:method-type-maybe-ref-method, see docs there
    (let ((end (+ nentries nentries)))
      (let lp ((i nentries))
        (if (< i end)
            (if ((vector-ref vec i) obj)
                (improper-map (lambda (colnum)
                                (vector-ref
                                 vec (+ i (* nentries (dec colnum)))))
                              colnumS)
                (lp (inc i)))
            #f)))))



;; Disable all assertments for production mode? Can we have full testing instead?
(define-typed (dot-oo:copy-rail! raili
                                 old #(natural0? oldn)
                                 new #(natural0? newn)
                                 ;; and these are indexes within an individual rail:
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

(define-typed (dot-oo:method-table-set old #(nonempty-symbol? prefix) pred meth)

  (define (finish new n)
    (vector-set! new 0 prefix)
    (vector-set! new n pred)
    (vector-set! new (fx+ n n) meth)
    new)

  (let* ((oldlen (vector-length old))
         (oldn (arithmetic-shift oldlen -2)))
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

                  (do-rail 0)
                  (do-rail 1)
                  (do-rail 2)
                  (do-rail 3)
                  (finish new n))))
          (else
           ;; prepend to the top
           (let* ((n (inc oldn))
                  (new (make-vector (arithmetic-shift n 2))))
             (dot-oo:copy-rail! 0 old oldn new n 0 oldn 1)
             (dot-oo:copy-rail! 1 old oldn new n 0 oldn 1)
             (dot-oo:copy-rail! 2 old oldn new n 0 oldn 1)
             (dot-oo:copy-rail! 3 old oldn new n 0 oldn 1)
             (finish new n))))))

(TEST
 > (dot-oo:method-table-set (vector) 'foo 'foo? 'foo.bar)
 #(
   foo
   foo?
   foo.bar
   0)
 > (dot-oo:method-table-set # 'baz 'baz? 'baz.bar)
 #(
   baz foo
   baz? foo?
   baz.bar foo.bar
   0 0)
 > (dot-oo:method-table-set # 'foo 'foo? 'foo.bar)
 #(
   foo baz
   foo? baz?
   foo.bar baz.bar
   0 0)
 > (define a
     (dot-oo:method-table-set # 'a 'a? 'a.bar))
 > a
 #(
   a foo baz
   a? foo? baz?
   a.bar foo.bar baz.bar
   0 0 0)
 > (dot-oo:method-table-set a 'foo 'foo? 'foo.bar)
 #(
   foo a baz
   foo? a? baz?
   foo.bar a.bar baz.bar
   0 0 0)
 > (dot-oo:method-table-set a 'baz 'baz? 'baz.bar)
 #(
   baz a foo
   baz? a? foo?
   baz.bar a.bar foo.bar
   0 0 0))


(define (dot-oo:method-table-set! tbl prefix pred meth)
  (set-box! tbl (dot-oo:method-table-set (unbox tbl) prefix pred meth))
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
                         ;; *dot-oo:method-stats* is 'location, a
                         ;; table mapping a caller location list (of
                         ;; up to depth
                         ;; *dot-oo:method-stats-locations-depth*) to
                         ;; the call count.
                         exact-natural0?
                         table?)))

(define-typed (dot-oo:show-method-table t)
  -> (list-of dot-oo:show-method-table-entry?)
  (let* ((v (unbox t))
         (n (arithmetic-shift (vector-length v) -2)))
    (unfold (C >= _ n)
            (lambda (i)
              (list (vector-ref v i)
                    (vector-ref v (+ i n))
                    (vector-ref v (+ i (* 2 n)))
                    (vector-ref v (+ i (* 3 n)))))
            inc-function
            0)))
