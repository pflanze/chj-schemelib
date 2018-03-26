;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Display Scheme data structures as directed graphs using the "dot"
;; tool from graphviz via "xdot", or "display" from imagemagick.

;; apt install graphviz
;; apt install xdot # or imagemagick

(require easy
	 bag
	 (cj-io-util xbacktick xsystem))

(export display-dot)


;; Process random Scheme data in two steps:

;; 1. convert to a bag of dot objects via .dot-bag (which contain the
;;    original objects)

;; 2. convert that to a bag of strings via dot-bag.string-bag (which
;;    calls dot's .string-bag methods, which use .dot-name to turn the
;;    objects into a string representation), print that to a file and
;;    run "display" on it.


(def string-bag? (bag-of string?))

;; (method (dot-id&label s) -> (values string? string-bag?))

;;    node id string (used for connection endpoint names), and label
;;    shown in the graph.


;; XX are there any differences from Scheme to C strings? Well,
;; unicode, right?
(def string.c-string object->string)


(defparameter dot:current-immediate-serial 0)

(def object->serial-number-string
     (comp number.string object->serial-number))


(def. (any.dot-id&label v)
  (values (if (mem-allocated? v)
	      (string-append
	       "A" (object->serial-number-string v))
	      (string-append
	       "I" (.string (parameter-inc! dot:current-immediate-serial))))
	  (string.c-string (object->string v))))


(def (make-vectorlike-dot-id&label obj-kind&typename)
     ;; kind is 1-character string; typename is the type name as
     ;; string
     (lambda (v)
       (let ((idn (object->serial-number-string v)))
	 (letv ((kind typename) (obj-kind&typename v))
	       (values (string-append kind idn)
		       (list "\"" typename " #" idn "\""))))))

(def. pair.dot-id&label
  (make-vectorlike-dot-id&label (lambda (v) (values "P" "pair"))))

(def. vector.dot-id&label
  (make-vectorlike-dot-id&label (lambda (v) (values "V" "vector"))))

(def. values.dot-id&label
  (make-vectorlike-dot-id&label (lambda (v) (values "T" "values"))))

(def. struct.dot-id&label
  (make-vectorlike-dot-id&label
   (lambda (v) (values "S"
		  (object->string (struct-type-name v))))))



;; Wrap Scheme objects, so that the serial number can be re-retrieved
;; multiple times (non-mem-allocated Scheme objects can't be passed
;; through object->serial-number-string because their entries in that
;; table are never freed):

(defclass ((dot-wrap _dot-wrap) id&label-cache object)

  (def (dot-wrap v) (_dot-wrap #f v))

  ;; caching delegate
  (defmethod (dot-id&label s)
    (or id&label-cache
	(let ((il (.dot-id&label object)))
	  (vector-set! s 1 il)
	  il))))

(def dot-wrap-list? (list-of dot-wrap?))



;; https://graphviz.gitlab.io/_pages/doc/info/lang.html

(definterface dot
  (method (string-bag s) -> string-bag?)


  (def dot-bag? (bag-of dot?))

  (def. (dot-bag.string-bag b)
    ;; XX digraph needed for -> but only allows that?
    (list "digraph foo {\n"
	  ;; "\tordering=out;\n"
	  ;; "\trankdir=;\n"
	  ;; "\trank=same;\n"
	  "\tranksep=0.4;\n" ;; makes it a bit less tall
	  ;; "\tmindist=3.4;\n" no effect
	  ;; "\tepsilon=0.0001;\n" only for neato
	  (map .string-bag (bag->list b))
	  "}\n"))


  (defclass (dot-leaf [dot-wrap? object])
    
    (defmethod (string-bag s) -> string-bag?
      (letv ((id label) (.dot-id&label object))
	    (list "\t" id " [ label=" label "];\n"))))

  
  (defclass (dot-> [dot-wrap? object] [dot-wrap-list? links])

    (defmethod (string-bag s) -> string-bag?
      (letv ((id label) (.dot-id&label object))
	    (list
	     ;; formatting for the object itself
	     (list "\t"
		   id
		   " [ label="
		   label
		   ", fontsize=7, shape=record ];\n")
	     ;; and the pointers to the next
	     (map/iota (lambda (w i)
			 (list "\t"
			       id
			       " -> "
			       (fst (.dot-id&label w))
			       " [ label="
			       (.string i)
			       ", fontsize=6, color=red ];\n"))
		       links))))))


(def. (dot-bag.string l)
  (=> l dot-bag.string-bag bag->string))


;; XX tmpfile?
(defparameter dot:tmp-path ".dot-scm.dot")

(defparameter dot:display-cmd
  (letv ((p c) (Xbacktick "which" "xdot"))
	(if (zero? c)
	    '("xdot")
	    '("display"))))

(def. (dot-bag.display l)
  (let ((path (dot:tmp-path)))
    (=> l
	dot-bag.string-bag
	(putfile path))
    (future (apply xsystem (append (dot:display-cmd) (list path))))))


;; (method (dot-bag v [dot-wrap? v*]) -> dot-bag?)

;;  (assert (eq? v (dot-wrap.object)))

;;  have to pass down the wrapped variant (so that sharing of the
;;  cache works), but still dispatch on the un-wrapped object so that
;;  object dispatch works easily (ok, could dispatch on a (dot-wrap-of
;;  ..) predicate, though, but..), hence take both arguments.

(def. (any.dot-bag v v*)
  (dot-leaf v*))

(def. (pair.dot-bag v v*)
  (let ((a* (dot-wrap (car v)))
	(r* (dot-wrap (cdr v))))
    (list (dot-> v* (list a* r*))
	  (*dot-bag a*)
	  (*dot-bag r*))))

(def. (null.dot-bag v v*)
  v*)

(def. (vector.dot-bag v v*)
  (let ((ws* (vector.map-list v dot-wrap)))
    (cons (dot-> v* ws*)
	  (ilist.map ws* *dot-bag))))

(def. (values.dot-bag v v*)
  (let ((ws* (vector.map-list (values->vector v) dot-wrap)))
    (cons (dot-> v* ws*)
	  (ilist.map ws* *dot-bag))))

(def. (struct.dot-bag v v*)
  (let ((ws* (ilist.map (struct-values v) dot-wrap)))
    (cons (dot-> v* ws*)
	  (ilist.map ws* *dot-bag))))


(def *dot-bag
     (lambda (w*)
       (.dot-bag (dot-wrap.object w*) w*)))

(def (display-dot v)
     (.display (.dot-bag v (dot-wrap v))))

