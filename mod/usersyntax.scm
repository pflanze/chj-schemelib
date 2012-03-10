
;; --- |require| runtime macro  ----

(##top-cte-add-macro!
 ##interaction-cte
 'require
 (##make-macro-descr
  #t
  -1
  (lambda (stx)
    ;; do not eval at expansion time, because Gambit crashes when
    ;; doing nested compilation; instead usually require forms are
    ;; translated separately
    (location-warn
     (source-location stx)
     "fall back to macro definition of require form, no compile-time definitions are supported")
    (mod:require-expand stx))
  #f))


;;; |RQ|
;; a require for user interaction that first clears what has been loaded

(##top-cte-add-macro!
 ##interaction-cte
 'RQ
 (##make-macro-descr
  #t
  -1
  (lambda (stx)
    (init-mod-loaded!)
    (mod:require-expand stx))
  #f))
