
For individual chars, see char-util.scm

e.g.

    > (=> (.list "Agent 007 in 2 lives") (.filter char-digit?) (.string))
    "0072"


string-util-1:

    (string-split str char-or-pred #!optional retain-matches?)

string-util-2:

    suffix-list
	suffix
	list-dropstop
	strip-suffix
	list-trim-left
	string-trim-left
	list-trim-right
	string-trim-right
	string-ref*
	chomp
	trim
	trim-maybe
	trim-both
	char-newline?
	trimlines
	trimlines-maybe
	string-trimlines-right
	nonempty? ;; nonempty-string? ? no, different.
	string-multiply
	number->padded-string
	inexact.round-at ;; XX move
	inexact.number-format
	string-starts?
	string-starts-ci?
	string-contains
	string-contains-ci
	string-contains?
	string-contains-ci?
	string-split-1 ;; vs. string-split-once ?
	if-string-split-once
	string-split-once
	string-reverse
	dirname* ;; vs. cj-io-util !
	string-map
	;; string-every see string-util-4.scm
	string-any
	string-downcase string-lc
	string-upcase string-uc
	string-upcase-first string-ucfirst
	string-pad-left
	string-ends-with?
	string-starts-with? ;;XXX vs string-starts? ?

string-util-3:

    string-contains-char?
	make-replace-substring
	replace-substring-error
	string.replace-substring
	string.replace-substring-ci
	string.replace-substrings
	string.replace-substrings-ci
	string.maybe-replace-substring
	string.maybe-replace-substring-ci
	string.maybe-replace-substrings
	string.maybe-replace-substrings-ci
	string.drop
	string.take
	string.any
	char-list.string-reverse
	substring*
	string.natural0
	string.natural
    
string-util-4:

    string-empty?
	string-every
	string-first-line
    

