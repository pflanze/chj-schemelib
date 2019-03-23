;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy ;; for now, change if need earlier?
         (joo joo:class-name.joo-type)
         test)

(export
 ;; joo.scm already has (re-export?):
 joo:class-name.joo-type
 joo-object? ;; should this be renamed to joo-instance? ?

 class-name.joo-type ;; alias for joo:class-name.joo-type
 (method joo-object.joo-type)
 (methods joo-type.subclasses
          ;; joo.scm already has these (re-export?):
          ;;    fields:
          joo-type.class-name
          joo-type.maybe-location
          joo-type.maybe-constructor-name
          joo-type.maybe-struct-tag
          joo-type.interface?
          joo-type.maybe-parent       ;; (maybe joo-type?)
          joo-type.implements         ;; (list-of joo-interface-type?)
          joo-type.field-decls        ;; list?
          joo-type.members            ;; symboltable?
          ;;    other methods:
          joo-type.members-set!
          joo-type.all-immediate-parents
          joo-type.all-parents
          joo-type.struct-tag
          joo-type.members-perhaps-add!
          joo-type.update-parents!
          joo-type.all-field-decls
          joo-type.all-field-names
          joo-type.is-a? ;; (is-a? s [joo-type? t])
          joo-type.covers-instance?
          ;; ^ (covers-instance? s v) ;; flip of joo:instance.member-of? 
          ))

"Introspection facilities for joo"

;; Didn't I write some of those already, I'm sure I did, where, sigh.


;; wanna avoid the joo: prefix, okay?
(def class-name.joo-type joo:class-name.joo-type)


(def. (joo-object.joo-type o)
  ;; implied in method dispatch but still currently not included on
  ;; direct calls, and @vector-ref is unsafe and we can't use
  ;; |vector-ref| as it's overridden, so:
  (assert (and (##vector? o) (fx>= (##vector-length o) 1)))
  (let* ((tag (@vector-ref o 0))
         (class-name (@maybe-struct-tag-name tag)))
    (assert class-name)
    ;; and currently the slowest part, probably:
    (joo:class-name.joo-type class-name)))


(def. joo-type.all-subclasses*
  ;;"including itself" XX change def. to allow docstrings like def
  (=>*/1 joo-type.members
         .list
         (.filter (comp just? cdr)) ;; XXX or does that for existance of constructor?
         (.map car)))

(def. (joo-type.all-subclasses t)
  "excluding itself"
  (let ((n (joo-type.class-name t)))
    (=> t
        joo-type.all-subclasses*
        (.filter (lambda (n*) (not (eq? n* n)))))))

(TEST
 > (definterface foob-interface)
 > (defclass (foob) implements: foob-interface)
 > (def it (joo:class-name.joo-type 'foob-interface)) ;; "class"? hmm
 > (def t (.joo-type (foob)))
 > (.class-name it) ;; "class"? hmm
 foob-interface
 > (.class-name t)
 foob
 > (.all-subclasses* t)
 (foob)
 > (.all-subclasses t)
 ()
 > (.all-subclasses it) ;; name? .all-implementations ?
 (foob)
 > (definterface foob2-interface)
 > (definterface foob3-interface extends: foob-interface)
 > (def it2 (joo:class-name.joo-type 'foob2-interface))
 > (.all-subclasses it)
 (foob) ;; foob2-interface is filtered out
 > (.all-subclasses it2)
 ()
 > (defclass (foob2)
     extends: foob
     implements: (foob2-interface))
 > (.all-subclasses* t)
 (foob foob2)
 > (.all-subclasses t)
 (foob2)
 > (.all-subclasses it2)
 (foob2)
 )
