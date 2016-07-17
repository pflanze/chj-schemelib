;;; Copyright 2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy test cj-io-util)

(TEST
 ;; XX heh, eliminate |suffix| from lib?
 ;; > (suffix "foo.bar")
 ;; "bar"
 ;; > (%try-error (suffix "foo"))
 ;; #(error "reached end of lis before finding pred")
 > (path-maybe-suffix "foo.bar")
 "bar"
 > (path-maybe-suffix "foo")
 #f)

(def suffix->mime-type
     (let ((t (make-table)))
       (lambda (#(string? suff) #(string? default))
	 (let ((v (table-ref t suff 'unset)))
	   (case v
	     ((unknown) default)
	     ((unset)
	      ;; (warn "first time querying of:" suff)
	      (let ((v (xbacktick "perl"
				  "-MFile::MimeInfo"
				  "-we"
				  (string-append
				   "my $v= mimetype("
				   (perl-quote
				    (string-append
				     "/nonexisting/foo." suff))
				   "); defined $v and do{ print $v or die $!}"))))
		(letv ((store ret) (if (string-empty? v)
				       (values 'unknown default)
				       (values v v)))
		      (table-set! t suff store)
		      ret)))
	     (else
	      v))))))

;; XX correct mime type for binary
(def mime-type:generic-binary "appication/binary")

(def (path.mime-type path)
     (cond ((path-maybe-suffix path)
	    => (C suffix->mime-type _ mime-type:generic-binary))
	   (else mime-type:generic-binary)))

(TEST
 > (suffix->mime-type "wefweq" "a")
 ;; first time querying of: "wefweq"
 "a"
 > (suffix->mime-type "wefweq" "b")
 "b"
 > (path.mime-type "foo.png")
 "image/png"
 > (path.mime-type "foo.txt")
 "text/plain"
 > (path.mime-type "foo.wef")
 "appication/binary"
 > (path.mime-type "foo.wefawiopo")
 "appication/binary"
 > (path.mime-type "foo")
 "appication/binary")

