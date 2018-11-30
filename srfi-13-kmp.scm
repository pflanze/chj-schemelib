;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (cj-source-util-2 assert))

(export string-contains
	string-contains-ci
	string-contains?
	string-contains-ci?
	string-kmp-partial-search)

(include "cj-standarddeclares.scm")


;; This is part of

;;; SRFI 13 string library reference implementation		-*- Scheme -*-
;;; Olin Shivers 7/2000
;;;
;;; Copyright (c) 1988-1994 Massachusetts Institute of Technology.
;;; Copyright (c) 1998, 1999, 2000 Olin Shivers. All rights reserved.
;;;   The details of the copyrights appear at the end of the file. Short
;;;   summary: BSD-style open source.



;;; Searching for an occurrence of a substring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-contains text pattern
			 #!optional
			 (t-start 0)
			 (t-end (string-length text))
			 (p-start 0)
			 (p-end (string-length pattern)))
  (%kmp-search pattern text char=? p-start p-end t-start t-end))

(define (string-contains-ci text pattern
			    #!optional
			    (t-start 0)
			    (t-end (string-length text))
			    (p-start 0)
			    (p-end (string-length pattern)))
  (%kmp-search pattern text char-ci=? p-start p-end t-start t-end))

(define (string-contains? text pattern
			  #!optional
			  (t-start 0)
			  (t-end (string-length text))
			  (p-start 0)
			  (p-end (string-length pattern)))
  (and (%kmp-search pattern text char=? p-start p-end t-start t-end) #t))

(define (string-contains-ci? text pattern
			     #!optional
			     (t-start 0)
			     (t-end (string-length text))
			     (p-start 0)
			     (p-end (string-length pattern)))
  (and (%kmp-search pattern text char-ci=? p-start p-end t-start t-end) #t))


;;; Knuth-Morris-Pratt string searching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; See
;;;     "Fast pattern matching in strings"
;;;     SIAM J. Computing 6(2):323-350 1977
;;;     D. E. Knuth, J. H. Morris and V. R. Pratt
;;; also described in
;;;     "Pattern matching in strings"
;;;     Alfred V. Aho
;;;     Formal Language Theory - Perspectives and Open Problems
;;;     Ronald V. Brook (editor)
;;; This algorithm is O(m + n) where m and n are the 
;;; lengths of the pattern and string respectively

;;; KMP search source[start,end) for PATTERN. Return starting index of
;;; leftmost match or #f.

(define (%kmp-search pattern text c= p-start p-end t-start t-end)
  (declare (fixnum))
  (let ((plen (- p-end p-start))
	(rv (make-kmp-restart-vector pattern c= p-start p-end)))

    ;; The search loop. TJ & PJ are redundant state.
    (let lp ((ti t-start) (pi 0)
	     (tj (- t-end t-start)) ; (- tlen ti) -- how many chars left.
	     (pj plen))		 ; (- plen pi) -- how many chars left.

      (if (= pi plen)
	  (- ti plen)			; Win.
	  (and (<= pj tj)		; Lose.
	       (if (c= (string-ref text ti) ; Search.
		       (string-ref pattern (+ p-start pi)))
		   (lp (+ 1 ti) (+ 1 pi) (- tj 1) (- pj 1)) ; Advance.
		   
		   (let ((pi (vector-ref rv pi))) ; Retreat.
		     (if (= pi -1)
			 (lp (+ ti 1) 0  (- tj 1) plen) ; Punt.
			 (lp ti       pi tj       (- plen pi))))))))))

;;; (make-kmp-restart-vector pattern [c= start end]) -> integer-vector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compute the KMP restart vector RV for string PATTERN.  If
;;; we have matched chars 0..i-1 of PATTERN against a search string S, and
;;; PATTERN[i] doesn't match S[k], then reset i := RV[i], and try again to
;;; match S[k].  If RV[i] = -1, then punt S[k] completely, and move on to
;;; S[k+1] and PATTERN[0] -- no possible match of PAT[0..i] contains S[k].
;;;
;;; In other words, if you have matched the first i chars of PATTERN, but
;;; the i+1'th char doesn't match, RV[i] tells you what the next-longest
;;; prefix of PATTERN is that you have matched.
;;;
;;; - C= (default CHAR=?) is used to compare characters for equality.
;;;   Pass in CHAR-CI=? for case-folded string search.
;;;
;;; - START & END restrict the pattern to the indicated substring; the
;;;   returned vector will be of length END - START. The numbers stored
;;;   in the vector will be values in the range [0,END-START) -- that is,
;;;   they are valid indices into the restart vector; you have to add START
;;;   to them to use them as indices into PATTERN.
;;;
;;; I've split this out as a separate function in case other constant-string
;;; searchers might want to use it.
;;;
;;; E.g.:
;;;    a b d  a b x
;;; #(-1 0 0 -1 1 2)

(define (make-kmp-restart-vector pattern c= start end)
  (declare (fixnum))
  (let* ((rvlen (- end start))
	 (rv (make-vector rvlen -1)))
    (if (> rvlen 0)
	(let ((rvlen-1 (- rvlen 1))
	      (c0 (string-ref pattern start)))

	  ;; Here's the main loop. We have set rv[0] ... rv[i].
	  ;; K = I + START -- it is the corresponding index into PATTERN.
	  (let lp1 ((i 0) (j -1) (k start))	
	    (if (< i rvlen-1)
		;; lp2 invariant:
		;;   pat[(k-j) .. k-1] matches pat[start .. start+j-1]
		;;   or j = -1.
		(let lp2 ((j j))
		  (cond ((= j -1)
			 (let ((i1 (+ 1 i)))
			   (if (not (c= (string-ref pattern (+ k 1)) c0))
			       (vector-set! rv i1 0))
			   (lp1 i1 0 (+ k 1))))
			;; pat[(k-j) .. k] matches pat[start..start+j].
			((c= (string-ref pattern k) (string-ref pattern (+ j start)))
			 (let* ((i1 (+ 1 i))
				(j1 (+ 1 j)))
			   (vector-set! rv i1 j1)
			   (lp1 i1 j1 (+ k 1))))

			(else (lp2 (vector-ref rv j)))))))))
    rv))


;;; We've matched I chars from PAT. C is the next char from the search string.
;;; Return the new I after handling C. 
;;;
;;; The pattern is (VECTOR-LENGTH RV) chars long, beginning at index PAT-START
;;; in PAT (PAT-START is usually 0). The I chars of the pattern we've matched
;;; are 
;;;     PAT[PAT-START .. PAT-START + I].
;;;
;;; It's *not* an oversight that there is no friendly error checking or
;;; defaulting of arguments. This is a low-level, inner-loop procedure
;;; that we want integrated/inlined into the point of call.

(define (kmp-step pat rv c i c= p-start)
  (let lp ((i i))
    (if (c= c (string-ref pat (+ i p-start)))	; Match =>
	(+ i 1)					;   Done.
	(let ((i (vector-ref rv i)))		; Back up in PAT.
	  (if (= i -1) 0			; Can't back up further.
	      (lp i))))))			; Keep trying for match.

;;; Zip through S[start,end), looking for a match of PAT. Assume we've
;;; already matched the first I chars of PAT when we commence at S[start].
;;; - <0:  If we find a match *ending* at index J, return -J.
;;; - >=0: If we get to the end of the S[start,end) span without finding
;;;   a complete match, return the number of chars from PAT we'd matched
;;;   when we ran off the end.
;;;
;;; This is useful for searching *across* buffers -- that is, when your
;;; input comes in chunks of text. We hand-integrate the KMP-STEP loop
;;; for speed.

(define (string-kmp-partial-search pat rv s i
				   #!optional
				   (c= char=?)
				   (p-start 0)
				   (s-start 0)
				   (s-end (string-length s)))
  (let ((patlen (vector-length rv)))
    (assert ((lambda (i) (and (integer? i) (exact? i) (<= 0 i) (< i patlen)))
	     i))

    ;; Enough prelude. Here's the actual code.
    (let lp ((si s-start)		; An index into S.
	     (vi i))			; An index into RV.
      (cond ((= vi patlen) (- si))	; Win.
	    ((= si s-end) vi)		; Ran off the end.
	    (else			; Match s[si] & loop.
	     (let ((c (string-ref s si)))
	       (lp (+ si 1)	
		   (let lp2 ((vi vi))	; This is just KMP-STEP.
		     (if (c= c (string-ref pat (+ vi p-start)))
			 (+ vi 1)
			 (let ((vi (vector-ref rv vi)))
			   (if (= vi -1) 0
			       (lp2 vi))))))))))))

