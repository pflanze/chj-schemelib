;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.

;; Some parts are copied from Clojure "1.10.0" and are:

;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(require easy
         table
         clojure-base)

(export (macro use-clojure))

(defmacro (scheme . body)
  `(##let ()
          (##namespace (""))
          ,@body))


(defmacro (use-clojure)
  `(begin
     (use-clojure-base)
     (##namespace ("clojure#" hash-map conj apply reduce concat
                   take drop filter map every?
                   any?
                   zipmap
                   vec vector-of
                   keys vals
                   symbol symbol? keyword keyword?
                   last))))

(use-clojure)

(def (hash-map . keys+vals)
     (list->table (sequential-pairs keys+vals list)))

(def (conj seq . vals)
     (cond (((either null? pair?) seq)
            (fold cons seq vals))
           ((vector? seq)
            (let ((seqlen (vector-length seq))
                  (valslen (length vals)))
              (let* ((totlen (+ seqlen valslen))
                     (v (make-vector totlen)))
                (for..< (i 0 seqlen)
                        (vector-set! v i (vector-ref seq i)))
                (let lp ((i seqlen)
                         (vals vals))
                  (when (pair? vals)
                        (let-pair ((a r) vals)
                                  (vector-set! v i a)
                                  (lp (inc i) r))))
                v)))
           (else
            (error "conj: can't handle:" seq))))

(TEST
 > (use-clojure)
 > (conj '(a b) 'c 'd)
 (d c a b)
 > (conj '(a b))
 (a b)
 > (conj '[a b])
 [a b]
 > (conj '[a b] 'c 'd)
 [a b c d])

(def (apply fn fst . rest)
     (##namespace ("" apply))
     (let ((->list (lambda (v)
                     (if ((either pair? null?) v)
                         v
                         (.list v)))))
       (if (null? rest)
           (apply fn (->list fst))
           (apply fn fst (append (butlast rest) (->list ((scheme last) rest)))))))

(TEST
 > (%try (apply / 10))
 ;; Execution error (IllegalArgumentException) at do4clojure.core/eval1852 ...
 ;; Don't know how to create ISeq from: java.lang.Long
 (exception text: "no method found for generic .list for value: 10\n")
 > (apply / 10 '())
 1/10
 > (apply / 10 '[])
 1/10
 > (apply / 10 11 '())
 10/11
 > (apply / 10 11 '(12 13))
 5/858
 > (apply / 10 11 '[])
 10/11
 > (apply / 10 11 '[12 13])
 5/858)


(defn reduce
  "f should be a function of 2 arguments. If val is not supplied,
  returns the result of applying f to the first 2 items in coll, then
  applying f to that result and the 3rd item, etc. If coll contains no
  items, f must accept no arguments as well, and reduce returns the
  result of calling f with no arguments.  If coll has only 1 item, it
  is returned and f is not called.  If val is supplied, returns the
  result of applying f to val and the first item in coll, then
  applying f to that result and the 2nd item, etc. If coll contains no
  items, returns val and f is not called."
  ;; {:added "1.0"}

  ;; oh well, Clojure relies on implementation in Java, and
  ;; Clojurescript on JavaScript (?, Can't find definition of
  ;; -reduce).

  ([f coll]
   (if (.null? coll)
       (f) ;; really meant for (+) right?. all. e.
       (let. ((first rest) coll)
             (if (.null? rest)
                 first
                 (.fold rest (flip f) first)))))
  ([f val coll]
   (.fold coll (flip f) val)))


(TEST
 ;; These are test results from Clojure "1.10.0"
 > (use-clojure)
 > (defn red
     ([op coll]
      (let [coll2 (apply vector coll)]
        (assert (and (list? coll) (vector? coll2)))
        (let [v1 (reduce op coll) v2 (reduce op coll2)]
          (assert (= v1 v2))
          v1)))
     ([op init coll]
      (let [coll2 (apply vector coll)]
        (assert (and (list? coll) (vector? coll2)))
        (let [v1 (reduce op init coll) v2 (reduce op init coll2)]
          (assert (= v1 v2))
          v1))))
 > (red + '(10 11))
 21
 > (red + '(10))
 10
 > (red + '())
 0
 > (%try (red / '()))
 ;; Execution error (ArityException) at do4clojure.core/eval1786 ..
 ;; Wrong number of args (0) passed to: clojure.core//
 (exception text: "Wrong number of arguments passed to procedure\n(/)\n")
 > (red / '(2))
 2
 ;; heh, not equivalent to apply!
 > (apply / '(2))
 1/2
 > (red / '(2 3))
 2/3
 > (red / '(2 3 4))
 1/6
 > (apply / '(2 3 4))
 1/6
 > (red + 100 '(10 11))
 121
 > (red + 100 '(10))
 110
 > (red + 100 '())
 100
 > (red / 100 '())
 100
 > (red / 100 '(2))
 50
 > (red / 100 '(2 3))
 50/3)


(defn concat
  "Returns a lazy seq representing the concatenation of the elements in the supplied colls."
  ;; {:added "1.0"
  ;;         :static true}
  ([]
   ;; XX Clojure defines it as: (lazy-seq nil), how can this work? nil
   ;; is not '() in Clojure.
   (lazy-seq '()))
  ([x] (lazy-seq x))
  ([x y]
   (lazy-seq
    (let [s (seq x)]
      (if s
          (if (chunked-seq? s)
              (chunk-cons (chunk-first s) (concat (chunk-rest s) y))
              (cons (first s) (concat (rest s) y)))
          y))))
  ([x y & zs]
   (let [cat (fn cat [xys zs]
                 (lazy-seq
                  (let [xys (seq xys)]
                    (if xys
                        (if (chunked-seq? xys)
                            (chunk-cons (chunk-first xys)
                                        (cat (chunk-rest xys) zs))
                            (cons (first xys) (cat (rest xys) zs)))
                        ;; (when zs
                        ;;       (cat (first zs) (next zs)))
                        ;; Clojure's |when| returns nil in the else case. Sigh. XX
                        (if zs
                            (cat (first zs) (next zs))
                            '())
                        ))))]
     (cat (concat x y) zs))))

(TEST
 > (F (concat '[a b c] '[d e f]))
 (a b c d e f) ;; *not* (a b c . [d e f]) anymore
 )

(TEST
 > (define TEST:equal? =)
 
 > (concat)
 ()
 > (concat '(a))
 (a)
 > (concat '[a])
 (a)

 > (concat '() '[a b])
 (a b)
 > (concat nil '[a b])
 (a b)

 > (concat '[a] '[b])
 (a b)
 > (concat '[a] '(b))
 (a b)
 > (concat '(a) '(b))
 (a b)
 > (concat '(a) '(b) '[c d])
 (a b c d)
 > (concat '(a x) '(b e) '(c d))
 (a x b e c d))



(def (clojure#take n s) (.take s n))
(def (clojure#drop n s) (.drop s n))
(def (clojure#filter fn s) (.filter s fn))
(def (clojure#map fn s . s*)
     ;; OH XX it needs weak (non-complaining) variants
     (apply .map s fn s*))

;; (TEST
;;  > (use-clojure)
;;  > (map vector '(a) '(1))
;;  ([a 1])
;;  > (map vector '(a) '(1 2))
;;  ([a 1])
;;  > (map vector '(a b c) '(1 2))
;;  ([a 1] [b 2]))


(def (clojure#every? fn s) (.every s fn))
(def clojure#any? true/1)

(TEST
 > (use-clojure)
 > (every? even? '[1 2 3])
 #f
 > (every? even? '[0 2 4])
 #t
 )

(def (clojure#zipmap keys vals)
     (list->table (zip-cons keys vals)))

(def pair->vector
     (lambda-pair ((a r))
             (vector a r)))

(def. table.stream
  (=>* table->list
       (cmp-sort (on first generic-cmp))
       (.map pair->vector)))


(def (clojure#vec s)
     (.vector s))

(def. table.vector
  (=>* table.stream
       list.vector))

(def (clojure#vector-of type-keyword)
     '[])


(TEST
 > (use-clojure)
 > (seq (zipmap '(a b c) '(1 2 3)))
 ([a 1] [b 2] [c 3])
 > (seq (zipmap '(a b c d e) '(1 2 3 4 5)))
 ([a 1] [b 2] [c 3] [d 4] [e 5])
 > (vec (zipmap '(a b c d e) '(1 2 3 4 5 )))
 [[a 1] [b 2] [c 3] [d 4] [e 5]]
 )


(def clojure#keys table-sorted-keys)

(def clojure#vals table-sorted-values)

(TEST
 > (use-clojure)
 > (keys (zipmap '(a b c d e) '(1 2 3 4 5 )))
 (a b c d e)
 > (vals (zipmap '(a b c d e) '(1 2 3 4 5 )))
 (1 2 3 4 5))


;; Hacky keyword and symbol handling (since we're not using a custom
;; parser but want to have them look the same printed via Scheme's
;; printer as when printed via Clojure's printer)

(defn keyword
  ;; XX Clojure silently returns nil if the argument is not a string;
  ;; sigh.
  ([nam]
   (string->symbol ($ ":$nam")))
  ([namesp nam]
   (string->symbol ($ ":$namesp/$nam"))))

(def (keyword? v)
     (or ((scheme keyword?) v) ;; ?
         (and ((scheme symbol?) v)
              (let* ((s (symbol.string v))
                     (len (string-length s)))
                (and (>= len 1)
                     (eq? (string-ref s 0) #\:))))))

(def (clojure:assert-non-keyword-looking-string str)
     (let (len (string-length str))
       (assert (or (zero? len)
                   (not (eq? (string-ref str 0) #\:))))
       str))

(defn symbol
  ;; ditto
  ([nam]
   (clojure:assert-non-keyword-looking-string nam)
   (string->symbol nam))
  ([namesp nam]
   (clojure:assert-non-keyword-looking-string namesp)
   (string->symbol ($ "${namesp}/$nam"))))


(def (symbol? v)
     (and ((scheme symbol?) v)
          (let* ((s (symbol.string v))
                 (len (string-length s)))
            (if (>= len 1)
                (not (eq? (string-ref s 0) #\:))
                #t))))

(TEST
 ;; Clojure seems crazy with this:
 ;; > (= 'true true)
 ;; #t ;; true
 > (symbol "true")
 true
 ;; > (= 'true (symbol "true"))
 ;; #f ;; false
 > (true? (symbol "true"))
 #f
 > (= 'true2 (symbol "true2"))
 #t ;; true
 > (nil? (symbol "nil"))
 #f
 
 > (keyword "3")
 :3
 ;; > (keyword 3)
 ;; nil
 ;; > (symbol ":3")
 ;; :3
 > (def v (keyword "foo" "bar"))
 > v
 :foo/bar
 > (keyword? v)
 #t
 > (symbol? v)
 #f
 > (def v (symbol "foo" "bar"))
 > v
 foo/bar
 > (keyword? v)
 #f
 > (symbol? v)
 #t)



(defn last
  ([v]
   (if (seq v)
       (if-not (.null? v) ;; Scheme's .empty?
               (.last v)))))


(TEST
 > (use-clojure)
 > (last "foob")
 #\b
 > (last '[1 2 3])
 3
 > (last '[])
 clojure#nil
 > (last '"")
 clojure#nil)




;; try: uses classname, not predicates, of course. Also, multiple
;;  |catch| clauses with 3+ arguments each instead of one with
;;  sub-lists with 2+ arguments each. Also, |finally| clauses,
;;  meh. Much work for nothing?

;; type: OK, if I get this right then classname stuff above will
;; actually work, kinda. Or step towards.

;; How does Clojurescript handle these?


;; unchecked-add: 

;; do4clojure.core=> (unchecked-add -100000000000000000000000000 -1)
;; -100000000000000000000000001N
;; do4clojure.core=> (type -100000000000000000000000000)
;; clojure.lang.BigInt
;; do4clojure.core=> (type -100000000000)
;; java.lang.Long
;; do4clojure.core=> (type -1)
;; java.lang.Long
;; do4clojure.core=> -100000000000000000000000000
;; -100000000000000000000000000N
;; do4clojure.core=> -1N
;; -1N

;; unchecked-byte:

;; do4clojure.core=> (unchecked-byte -100000000000000000000000000)
;; 0
;; do4clojure.core=> (unchecked-byte -1)
;; -1
;; do4clojure.core=> (unchecked-byte -127)
;; -127
;; do4clojure.core=> (unchecked-byte -128)
;; -128
;; do4clojure.core=> (unchecked-byte -129)
;; 127

;; OH my, blissfully give dangerous or even useless results. I guess
;; unchecked simply means unsafe really.

;; update
;; update-in
;; update-proxy
;; uri?
;; use
;; uuid?
;; val   (val e)   Returns the value in the map entry. -- ?


;; var
;; special form

;; Usage: (var symbol)

;; The symbol must resolve to a var, and the Var object
;; itself (not its value) is returned. The reader macro #'x
;; expands to (var x).

;; Please see https://clojure.org/reference/special_forms#var

