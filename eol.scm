
(require easy)

;; finally?

(defenum eol-name CR LF CRLF)

(def. (eol-name.string v)
  (xcase v
	 ((CR) "\r")
	 ((LF) "\n")
	 ((CRLF) "\r\n")))


