(require easy
	 dot-oo ;; included in easy now?
	 )

(def (file-info-time-accessor accessor)
     (lambda (v)
       ;; cut off sub-second precision to make it |unixtime?|
       ;; compatible (which in fact is not represented in the version
       ;; of Gambit I'm using anyway)
       (integer (time->seconds (accessor v)))))

(def. file-info.type file-info-type)
(def. file-info.device file-info-device)
(def. file-info.inode file-info-inode)
(def. file-info.mode file-info-mode)
(def. file-info.nlinks file-info-number-of-links)
(def. file-info.owner file-info-owner)
(def. file-info.group file-info-group)
(def. file-info.size file-info-size)
(def. file-info.atime
  (file-info-time-accessor file-info-last-access-time))
(def. file-info.mtime
  (file-info-time-accessor file-info-last-modification-time))
(def. file-info.ctime
  (file-info-time-accessor file-info-last-change-time))
;; what are these? :
(def. file-info.attributes file-info-attributes)
(def. file-info.creation-time file-info-creation-time)
(def. file-info.creation-time*
  ;; seems to return garbage!
  (file-info-time-accessor file-info-creation-time))

