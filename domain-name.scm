;;; Copyright 2014 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.

(require test
	 (srfi-1 every fold-right)
	 (string-util-1 string-split strings-join)
	 (string-util-2 string-pad-left string-downcase)
	 ;; ^ sigh that mess of scattered libs
	 (list-util make-list/tail)
	 (cj-functional compose both)
	 (cj-env natural0?)
	 dot-oo
	 (cj-typed ->)
	 (cj-source-util-2 assert) ;; HUH, what odd place this is in
	 )


;; http://en.wikipedia.org/wiki/Domain_name
;; "Domain names may be formed from the set of alphanumeric ASCII
;; characters (a-z, A-Z, 0-9), but characters are case-insensitive. In
;; addition the hyphen is permitted if it is surrounded by a
;; characters or digits, i.e. it is not the start or end of a label."

(define dnchar?
  ;; excluding hyphen since already split on it
  char-alphanumeric?)

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
 > (nonnulldnlabel? "_domainkey")
 #t
 )

;; XX: "Hostnames impose restrictions on the characters allowed in the
;; corresponding domain name. A valid hostname is also a valid domain
;; name, but a valid domain name may not necessarily be valid as a
;; hostname."

;; Note (https://en.wikipedia.org/wiki/Hostname): "While a hostname
;; may not contain other characters, such as the underscore character
;; (_), other DNS names may contain the underscore.[3] Systems such as
;; DomainKeys and service records use the underscore as a means to
;; assure that their special character is not confused with
;; hostnames."


;; XX NOTE: name should perhaps be changed, as:
;; - this is using restrictions for host names, not domain names.
;; - this excludes IPv4 addresses (perhaps those should be accepted as
;;   valid domains or host names?)
(define (fqdn-string? x)
  (and (string? x)
       ;; "The full domain name may not exceed a total length of 253
       ;; ASCII characters in its textual representation."
       (<= (string-length x) 253)
       ;; ^ check for ASCII happens through dnchar? later.
       ;; XX: vs. https://en.wikipedia.org/wiki/Hostname : "the entire
       ;; hostname (including the delimiting dots) has a maximum of
       ;; 255 characters"
       (let ((ss (string-split x #\.)))
	 (and (<= 2 (length ss) 127)
	      ;; XX allow "" at the end? But *only* at the end.
	      ;; "The empty label is reserved for the root node."
	      (every nonnulldnlabel? ss)
	      ;; Don't treat IP addresses as fqdn-string?, ok ?
	      (not (every char-digit?
			  (string.list (last ss))))))))

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
 > (fqdn-string? "127.0.0.1")
 #f ;; ok?
 > (fqdn-string? "brisbane._domainkey.example.net")
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

;; XX merge with uint8 in cj-env*
(define. (u8-string.u8-number str)
  ;;(assert (u8-string? str)) ;; XX sgh, should really make this part of define. ?
  (-> uint8? (string.number str)))

(TEST
 ;; > (.u8-number "")
 ;; *** ERROR IN (console)@4.1 -- no method found for generic .u8-number for value: ""
 > (.u8-number "1")
 1
 > (.u8-number "255")
 255
 ;; > (.u8-number "256")
 ;; *** ERROR IN (console)@8.1 -- no method found for generic .u8-number for value: "256"
 )


(define (ipv4-string? x)
  (and (string? x)
       (let ((ss (string-split x #\.)))
	 (and (= (length ss) 4)
	      (every u8-string? ss)))))


(define (ipv4-number? x)
  (and (natural0? x)
       (< x (expt 2 32))))


(define. (ipv4-string.ipv4-number x)
  (-> natural0?
      (let ((ss (string-split x #\.)))
	(and (= (length ss) 4)
	     (fold (lambda (x res)
		     (+ (u8-string.u8-number x)
			(* res 256)))
		   0
		   ss)))))

(TEST
 > (number->string (.ipv4-number "127.0.0.1") 16)
 "7f000001"
 > (ipv4-number? (.ipv4-number "255.255.255.255"))
 #t
 > (ipv4-number? (inc (.ipv4-number "255.255.255.255")))
 #f
 > (ipv4-number? (.ipv4-number "0.0.0.0"))
 #t
 > (ipv4-number? (dec (.ipv4-number "0.0.0.0")))
 #f
 )


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
 > (ipv6-segment-string? "-24321")
 #f
 > (ipv6-segment-string? "2abf")
 #t
 > (ipv6-segment-string? "2ABF")
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


;; don't use the .number method name, as that would be ambiguous
;; e.g. for strings that only contain decimal characters; well
;; actually the Scheme string.number method requires a second argument
;; for this reason; we've got a different api here.
(define. (ipv6-hex-string.ipv6-number x)
  (-> natural0? (string->number x 16)))


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


(define ip-string? (either ipv4-string? ipv6-string?))
;; but not ipv6-hex-string?, ok?


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


;; .ipv6-number

(define. bare-ipv6-string.ipv6-number
  (compose ipv6-hex-string.ipv6-number
	   bare-ipv6-string.ipv6-hex-string))


(define. ipv6-string.ipv6-number
  (compose bare-ipv6-string.ipv6-number
	   ipv6-string.bare-ipv6-string))


(TEST
 > (.ipv6-number "20010db885a3000000008a2e03707334")
 42540766452641154071740215577757643572
 > (.ipv6-number "2001:0db8:85a3::8a2e:0370:7334")
 42540766452641154071740215577757643572
 > (.ipv6-number "[2001:0db8:85a3:0000::8a2e:0370:7334]")
 42540766452641154071740215577757643572
 > (number->string 42540766452641154071740215577757643572 16)
 "20010db885a3000000008a2e03707334"
 )

(define (ipv6-number? x)
  (and (natural0? x)
       (< x (expt 2 128))))
;; tests?


;; --- Networks -----

;; XX hmmm, should these be named differently? These are for
;; network-*masks*.

(define (network-number? x bits)
  ;; number of zero-bits on the right being equal to the number of
  ;; bits used by the inversion means that the bits to the left are
  ;; all ones
  (= (first-bit-set x)
     (integer-length (bitwise-xor x (dec (arithmetic-shift 2 (dec bits)))))))

(TEST
 > (network-number? 1 8)
 #f
 > (network-number? 127 8)
 #f
 > (network-number? 128 8)
 #t
 > (network-number? #b11000000 8)
 #t
 > (network-number? #b01000000 8)
 #f
 > (network-number? #b11000001 8)
 #f
 > (network-number? #b11010000 8)
 #f
 ;; possibly useful detail:
 > (network-number? #b111100000 8)
 #f)



(define (ipv4-network-number? x)
  (and (ipv4-number? x)
       (network-number? x 32)))

(define (ipv6-network-number? x)
  (and (ipv6-number? x)
       (network-number? x 128)))


(define ipv4-network-string?
  (both ipv4-string?
	(compose ipv4-network-number? ipv4-string.ipv4-number)))

(TEST
 > (ipv4-network-string? "1")
 #f
 > (ipv4-network-string? "1.0.0.0")
 #f
 > (ipv4-network-string? "128.0.0.0")
 #t
 > (ipv4-network-string? "255.0.0.0")
 #t
 > (ipv4-network-string? "255.255.255.128")
 #t
 > (ipv4-network-string? "255.255.255.64")
 #f
 > (ipv4-network-string? "255.255.255.255")
 #t ;; XX ok?
 )


;; really stupid that all the methods need to be tripled; XX use a
;; module?

(define ipv6-network-string?
  (both ipv6-string?
	(compose ipv6-network-number? ipv6-string.ipv6-number)))

(define bare-ipv6-network-string?
  (both bare-ipv6-string?
	(compose ipv6-network-number? bare-ipv6-string.ipv6-number)))

;; XX and should I move the 'hex' to the front as with the 'bare'?
(define ipv6-network-hex-string?
  (both ipv6-hex-string?
	(compose ipv6-network-number? ipv6-hex-string.ipv6-number)))

(TEST
 > (ipv6-network-hex-string? "F0000000000000000000000000000000")
 #t
 > (bare-ipv6-network-string? "2001:0db8:85a3::8a2e:0370:7334")
 #f
 > (bare-ipv6-network-string? "FF00::00")
 #t
 > (bare-ipv6-network-string? "FF00::0")
 #t
 > (bare-ipv6-network-string? "FF00::1")
 #f
 > (bare-ipv6-network-string? "FF::00")
 #f ;; XX is this correct?
 ;; > (bare-ipv6-network-string? "FF00::")
 ;; #f  XXX really should parse this now, right?
 )


;; Networks with prefixes:


(define ipv4-bits 32)
(define ipv6-bits 128)

(define (string-of-integer/range lo hi)
     ;; ^ require lo hi to be integers or not? heh, leave it open.
     (lambda (s)
       (and (string? s)
	    (cond ((string->number s)
		   => (lambda (x)
			(and (integer? x)
			     (exact? x)
			     ;; XXX hmmm, does make naming
			     ;; inconsistent? Why does Scheme accept
			     ;; 2.0 as integer?
			     (<= lo x hi))))
		  (else #f)))))

(TEST
 > (map (string-of-integer/range -2 2) '("-2" "2.1" "2.0" "2 " "2" "3" "-3"))
 (#t #f #f #f #t #f #f))



(define (ip_-network/prefix-string? ip_-string? ip_-network-string? ip_-bits)
 (lambda (v)
   (and (string? v)
	(let ((ss (string-split v #\/)))
	  (and (= (length ss) 2)
	       ;; forever use exceptions instead? then I can tell
	       ;; why 'it fails' (or, make this part of the
	       ;; language? automatic?)
	       (ip_-string? (car ss))
	       (or (ip_-network-string? (cadr ss))
		   ;; XX incl or excl. 0 ?:
		   ((string-of-integer/range 1 ip_-bits) (cadr ss))))))))

(define ipv4-network/prefix-string?
     (ip_-network/prefix-string? ipv4-string?
				 ipv4-network-string?
				 ipv4-bits))

(TEST
 > (ipv4-network/prefix-string? "127.0.0.1")
 #f
 > (ipv4-network/prefix-string? "127.0.0.1/0")
 #f ;; ok?
 > (ipv4-network/prefix-string? "127.0.0.1/8")
 #t
 > (ipv4-network/prefix-string? "127.0.0.1/8.0")
 #f ;; be strict, ok? Makes sense here, but is stricter than Scheme
    ;; integer number predicate.
 > (ipv4-network/prefix-string? "127.0.0.1/8 ")
 #f
 > (ipv4-network/prefix-string? "127.0.0.1/9")
 #t
 > (ipv4-network/prefix-string? "127.0.0.1/32")
 #t ;; ok?
 > (ipv4-network/prefix-string? "127.0.0.1/33")
 #f
 > (ipv4-network/prefix-string? "127.0.0.1/255.255.0.0")
 #t
 )

(define bare-ipv6-network/prefix-string?
     (ip_-network/prefix-string? bare-ipv6-string?
				 bare-ipv6-network-string?
				 ipv6-bits))

(define ipv6-network/prefix-string?
     ;; ok? or expect [ ] around the whole thing?
     bare-ipv6-network/prefix-string?)

(TEST
 > (ipv6-network-string? "1080::8:800:200C:417A/96")
 #f
 > (ipv6-network/prefix-string? "1080::8:800:200C:417A/96")
 #t
 > (ipv6-network/prefix-string? "1080::8:800:200C:417A/FFFF:FFFF::0")
 #t
 ;; > (ipv6-network/prefix-string? "1080::8:800:200C:417A/FFFF:FFFF::")
 ;; #t -- XX enable once :: parsing is finished
 )

