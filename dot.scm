;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Display Scheme data structures as directed graphs using the "dot"
;; tool via "display" from imagemagick.

;; apt install graphviz imagemagick

(require easy
	 bag
	 (cj-io-util xsystem))

(export display-dot)


;; Process random Scheme data in two steps:

;; 1. convert to a bag of dot objects via .dot-bag (which contain the
;;    original objects)

;; 2. convert that to a bag of strings via dot-bag.string-bag (which
;;    calls dot's .string-bag methods, which use .dot-name to turn the
;;    objects into a string representation), print that to a file and
;;    run "display" on it.


(def string-bag? (bag-of string?))


(def. (any.dot-name v)
  (object->string (object->string v)))

(def. (pair.dot-name v)
  (list "\"pair #" (number.string (object->serial-number v)) "\""))

(def. (struct.dot-name v)
  (list "\"" (object->string (struct-type-name v))
	" #" (number.string (object->serial-number v)) "\""))


(definterface dot
  (method (string-bag s) -> string-bag?)

  (def dot-bag? (bag-of dot?))

  (def. (dot-bag.string-bag b)
    ;; XX digraph needed for -> but only allows that?
    (list "digraph foo {\n"
	  (map .string-bag (bag->list b))
	  "}\n"))


  (defclass (dot-leaf v)

    (defmethod (string-bag s) -> string-bag?
      ;; (list "  " (.dot-name v) ";\n")
      ;; or, omit?
      ""))
  
  (defclass (dot-> a b)

    (defmethod (string-bag s) -> string-bag?
      (list "\t" (.dot-name a) " -> " (.dot-name b) ";\n"))))


(def. (dot-bag.string l)
  (=> l dot-bag.string-bag bag->string))


;; XX tmpfile?
(defparameter dot:tmp-path ".dot-scm.dot")

(def. (dot-bag.display l)
  (let ((path (dot:tmp-path)))
    (=> l
	dot-bag.string-bag
	(putfile path))
    (xsystem "display" path)))


(def. (any.dot-bag v)
  (dot-leaf v))

(def. (pair.dot-bag v)
  (list (dot-> v (car v))
	(dot-> v (cdr v))
	(.dot-bag (car v))
	(.dot-bag (cdr v))))

(def. (null.dot-bag v)
  '())

(def. (vector.dot-bag v)
  (vector.map-list v
		   (lambda (w)
		     (cons (dot-> v w)
			   (.dot-bag w)))))

(def. (struct.dot-bag v)
  (ilist.map (struct-values v)
	     (lambda (w)
	       (cons (dot-> v w)
		     (.dot-bag w)))))


(def (display-dot v)
     (future (.display (.dot-bag v))))

