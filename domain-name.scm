;; http://en.wikipedia.org/wiki/Domain_name
;; "Domain names may be formed from the set of alphanumeric ASCII
;; characters (a-z, A-Z, 0-9), but characters are case-insensitive. In
;; addition the hyphen is permitted if it is surrounded by a
;; characters or digits, i.e. it is not the start or end of a label."

(define dnchar?
  ;; excluding hyphen since already split on it
  (either char-digit? char-alpha?))

(define (nonnulldnlabel? s)
  (and (string? s)
       (<= (string-length s) 63) ;; "octets" hm.
       (let ((parts (string-split s #\-)))
	 (and
	  ;; (>= (length parts) 1)  "" splits into ("") thus never happens hm.
	  (not (string-empty? (car parts)))
	  (not (string-empty? (last parts)))
	  (every (lambda (part)
		   (every dnchar? (string->list part)))
		 parts)))))

(TEST
 > (nonnulldnlabel? "foo")
 #t
 > (nonnulldnlabel? "")
 #f
 > (nonnulldnlabel? "f-a")
 #t
 > (nonnulldnlabel? "f-a-bb")
 #t
 > (nonnulldnlabel? "f-")
 #f
 > (nonnulldnlabel? "-")
 #f
 > (nonnulldnlabel? "f--a")
 #t ;; ok? (Internationalized domain names use this, even)
 > (nonnulldnlabel? "f/a")
 #f
 )

(define (fqdn-string? x)
  (and (string? x)
       (let ((ss (string-split x #\.)))
	 (and (<= 2 (length ss) 127)
	      ;; XX allow "" at the end? But *only* at the end.
	      (every nonnulldnlabel? ss)))))

(TEST
 > (fqdn-string? "")
 #f
 > (fqdn-string? "a")
 #f
 > (fqdn-string? "a.b")
 #t
 > (fqdn-string? "a.b.")
 #f ;; currently.
 > (fqdn-string? ".b.c")
 #f
 > (fqdn-string? "a.b.c")
 #t
 )


(define. (fqdn-string.sortkey x)
  (let ((ss (string-split x #\.)))
    (strings-join (reverse (map string-lc ss))
		  ".")))

(TEST
 > (.sortkey "www.foo.com")
 "com.foo.www"
 )


(define (u8-string? str)
  (and (string? str) ;; stupid? since I already call it -string? But, exactly why?
       (cond ((string->number str) => (all-of exact?
					      natural0?
					      (cut < <> 256)))
	     (else #f))))

(TEST
 > (u8-string? "")
 #f
 > (u8-string? "1")
 #t
 > (u8-string? "-1")
 #f
 > (u8-string? "255")
 #t
 > (u8-string? "256")
 #f
 > (u8-string? "25.")
 #f
 )

(define (ipv4-string? x)
  (and (string? x)
       (let ((ss (string-split x #\.)))
	 (and (= (length ss) 4)
	      (every u8-string? ss)))))


(define (ipv6-segment-string? x)
  (and (string? x)
       (or (string-empty? x)
	   (cond ((string->number x 16)
		  => (lambda (n)
		       (<= 0 n (dec (arithmetic-shift 1 16)))))
		 (else #f)))))

(TEST
 > (ipv6-segment-string? "")
 #t
 > (ipv6-segment-string? "1")
 #t
 > (ipv6-segment-string? "1354899")
 #f
 > (ipv6-segment-string? "2432")
 #t
 > (ipv6-segment-string? "12432")
 #f
 > (ipv6-segment-string? "-2432")
 #f
 > (ipv6-segment-string? "2abf")
 #t
 > (ipv6-segment-string? "2abg")
 #f
 )


(define (ipv6-hex-string? x)
  (and (string? x)
       (= (string-length x) 32)
       (string-every char-hexdigit? x)))

(TEST
 > (ipv6-hex-string? "20010db885a3000000008a2e03707334")
 #t
 > (ipv6-hex-string? "20010db885a3000000008a2e037073341234")
 #f
 > (ipv6-hex-string? "20010db885a3000000008a2e0370")
 #f
 > (ipv6-hex-string? "2001:0db8:85a3:0000:0000:8a2e:0370:7334")
 #f
 )

(define (bare-ipv6-string? x)
  (and (string? x)
       (let ((ss (string-split x #\:)))
	 (and (<= (length ss) 8)
	      (every ipv6-segment-string? ss)
	      (<= (fold (lambda (s c)
			  (if (string-empty? s)
			      (inc c)
			      c))
			0
			ss)
		  1)))))

(TEST
 > (bare-ipv6-string? "2001:0db8:85a3:0000:0000:8a2e:0370:7334")
 #t
 > (bare-ipv6-string? "2001:0db8:85a3:0000:0000:8a2e:0370:7334:")
 #f
 > (bare-ipv6-string? "2001:0db8:85a3:0000:0000:8a2e:0370:7334:1222")
 #f
 > (bare-ipv6-string? "2001:0db8:85a3::8a2e:0370:7334")
 #t
 > (bare-ipv6-string? "2001::85a3::8a2e:0370:7334")
 #f
 > (bare-ipv6-string? "[2001:0db8:85a3::8a2e:0370:7334]")
 #f
 )

;; "[2605:2700:0:5::4713:9563]"
(define (ipv6-string? x)
  (and (string? x)
       (let ((len (string-length x)))
	 (and (> len 2)
	      (char=? (string-ref x 0) #\[)
	      (char=? (string-ref x (dec len)) #\])
	      (bare-ipv6-string? (substring x 1 (dec len)))))))

(TEST
 > (ipv6-string? "2001:0db8:85a3::8a2e:0370:7334")
 #f
 > (ipv6-string? "[2001:0db8:85a3::8a2e:0370:7334]")
 #t
 )


;;(define ip-string? (either ipv4-string? ipv6-string? ipv6-hex-string?))


;; conversion/canonical format

(define. (bare-ipv6-string.ipv6-hex-string s)
  (let* ((ss (string-split s #\:))
	 ;;^ splitting again (after bare-ipv6-string type check)? stupid? hh
	 (len (length ss))
	 (missing (- 8 len)))
    (assert (<= 0 missing 8))
    (strings-join (fold-right (lambda (item rest)
				(if (string-empty? item)
				    ;; inc since we replace item
				    (make-list/tail (inc missing) "0000" rest)
				    (cons (string-pad-left
					   (string-downcase item)
					   #\0 4)
					  rest)))
			      '()
			      ss)
		  "")))

(TEST
 > (.ipv6-hex-string "2001:0db8:85a3::8a2e:0370:7334")
 "20010db885a3000000008a2e03707334"
 > (ipv6-hex-string? #)
 #t
 > (.ipv6-hex-string "1:0db8:85a3::8a2e:0370:7334")
 "00010db885a3000000008a2e03707334"
 > (.ipv6-hex-string "2001:DB8::1")
 "20010db8000000000000000000000001"
 ;; todo: (.ipv6-hex-string "::") "1::" "::2"
 )

(define. (ipv6-string.bare-ipv6-string x)
  (let ((len (string-length x)))
    (assert (> len 2))
    (assert (char=? (string-ref x 0) #\[))
    (assert (char=? (string-ref x (dec len)) #\]))
    (substring x 1 (dec len))))

(define. ipv6-string.ipv6-hex-string
  (compose bare-ipv6-string.ipv6-hex-string
	   ipv6-string.bare-ipv6-string))


