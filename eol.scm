
(require easy)

;; finally?

(defenum eol-name CR LF CRLF)

(def. (eol-name.newline-string v)
  (xcase v
	 ((CR) "\r")
	 ((LF) "\n")
	 ((CRLF) "\r\n")))


