;; Copyright © 2026 Barry Schwartz
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions: The above
;; copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-library (gafoundry-lib prec)

  (import (scheme base)
          (scheme char)
          (scheme write)
          (scheme lazy)
          (scheme cxr)
          (srfi 1))

  (export tok:char-group
          tok:decimal-digits
          tok:upper-case
          tok:lower-case
          tok:whitespaces
          prec:make-nud
          prec:make-led
          prec:delim
          prec:nofix
          prec:prefix
          prec:infix
          prec:nary
          prec:postfix
          prec:prestfix
          prec:commentfix
          prec:matchfix
          prec:inmatchfix)

  (begin

    (define (software-type) 'unix)

(define slib:tab #\x09)
(define slib:form-feed #\x0C)

(define char-code-limit (+ 1 #x10ffff))

(define (identity x) x)

(define slib:error
  (let ((error error))
    (lambda args
      (let ((cep (current-error-port)))
        ;;(if (provided? 'trace) (print-call-stack cep))
        (display "Error: " cep)
        (for-each (lambda (x) (display #\space cep) (write x cep)) args)
        (error "")))))

(define slib:warn
  (lambda args
    (let ((cep (current-error-port)))
      ;;(if (provided? 'trace) (print-call-stack cep))
      (display "Warn: " cep)
      (for-each (lambda (x) (display #\space cep) (write x cep)) args))))

(define (call-with-input-string str proc)
  (let* ((port (open-input-string str))
         (result (proc port)))
    (close-input-port port)
    result))

(define (call-with-output-string proc)
  (let ((port (open-output-string))
        (str #f))
    (proc port)
    (set! str (get-output-string port))
    (close-output-port port)
    str))

;;---------------------------------------------------------------------
;;"sc2.scm" Implementation of rev2 procedures eliminated in subsequent versions.
;; Copyright (C) 1991, 1993 Aubrey Jaffer
;;
;;Permission to copy this software, to modify it, to redistribute it,
;;to distribute modified versions, and to use it for any purpose is
;;granted, subject to the following restrictions and understandings.
;;
;;1.  Any copy made of this software must include this copyright notice
;;in full.
;;
;;2.  I have made no warranty or representation that the operation of
;;this software will be error-free, and I am under no obligation to
;;provide any services, by way of maintenance, update, or otherwise.
;;
;;3.  In conjunction with products arising from the use of this
;;material, there shall be no use of my name in any advertising,
;;promotional, or sales literature without prior written consent in
;;each case.
(define (substring-move-left! string1 start1 end1 string2 start2)
  (do ((i start1 (+ i 1))
       (j start2 (+ j 1))
       (l (- end1 start1) (- l 1)))
      ((<= l 0))
    (string-set! string2 j (string-ref string1 i))))
(define (string-null? str)
  (= 0 (string-length str)))
;;---------------------------------------------------------------------



    ;;;"alist.scm", alist functions for Scheme.
;;;Copyright (C) 1992, 1993, 2003 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;;@code{(require 'alist)}
;;@ftindex alist
;;
;;Alist functions provide utilities for treating a list of key-value pairs
;;as an associative database.  These functions take an equality predicate,
;;@var{pred}, as an argument.  This predicate should be repeatable,
;;symmetric, and transitive.
;;
;;Alist functions can be used with a secondary index method such as hash
;;tables for improved performance.

;;@body
;;Returns an @dfn{association function} (like @code{assq}, @code{assv}, or
;;@code{assoc}) corresponding to @var{pred}.  The returned function
;;returns a key-value pair whose key is @code{pred}-equal to its first
;;argument or @code{#f} if no key in the alist is @var{pred}-equal to the
;;first argument.
(define (predicate->asso pred)
  (cond ((eq? eq? pred) assq)
	((eq? = pred) assv)
	((eq? eqv? pred) assv)
	((eq? char=? pred) assv)
	((eq? equal? pred) assoc)
	((eq? string=? pred) assoc)
	(else (lambda (key alist)
		(let l ((al alist))
		  (cond ((null? al) #f)
			((pred key (caar al)) (car al))
			(else (l (cdr al)))))))))

;;@body
;;Returns a procedure of 2 arguments, @var{alist} and @var{key}, which
;;returns the value associated with @var{key} in @var{alist} or @code{#f} if
;;@var{key} does not appear in @var{alist}.
(define (alist-inquirer pred)
  (let ((assofun (predicate->asso pred)))
    (lambda (alist key)
      (let ((pair (assofun key alist)))
	(and pair (cdr pair))))))

;;@body
;;Returns a procedure of 3 arguments, @var{alist}, @var{key}, and
;;@var{value}, which returns an alist with @var{key} and @var{value}
;;associated.  Any previous value associated with @var{key} will be
;;lost.  This returned procedure may or may not have side effects on its
;;@var{alist} argument.  An example of correct usage is:
;;
;;@lisp
;;(define put (alist-associator string-ci=?))
;;(define alist '())
;;(set! alist (put alist "Foo" 9))
;;@end lisp
(define (alist-associator pred)
  (let ((assofun (predicate->asso pred)))
    (lambda (alist key val)
      (let* ((pair (assofun key alist)))
	(cond (pair (set-cdr! pair val)
		    alist)
	      (else (cons (cons key val) alist)))))))

;;@body
;;Returns a procedure of 2 arguments, @var{alist} and @var{key}, which
;;returns an alist with an association whose @var{key} is key removed.
;;This returned procedure may or may not have side effects on its
;;@var{alist} argument.  An example of correct usage is:
;;
;;@lisp
;;(define rem (alist-remover string-ci=?))
;;(set! alist (rem alist "foo"))
;;@end lisp
(define (alist-remover pred)
  (lambda (alist key)
    (cond ((null? alist) alist)
	  ((pred key (caar alist)) (cdr alist))
	  ((null? (cdr alist)) alist)
	  ((pred key (caadr alist))
	   (set-cdr! alist (cddr alist)) alist)
	  (else
	   (let l ((al (cdr alist)))
	     (cond ((null? (cdr al)) alist)
		   ((pred key (caadr al))
		    (set-cdr! al (cddr al)) alist)
		   (else (l (cdr al)))))))))

;;@body
;;Returns a new association list formed by mapping @var{proc} over the
;;keys and values of @var{alist}.   @var{proc} must be a function of 2
;;arguments which returns the new value part.
(define (alist-map proc alist)
  (map (lambda (pair) (cons (car pair) (proc (car pair) (cdr pair))))
       alist))

;;@body
;;Applies @var{proc} to each pair of keys and values of @var{alist}.
;;@var{proc} must be a function of 2 arguments.  The returned value is
;;unspecified.
(define (alist-for-each proc alist)
  (for-each (lambda (pair) (proc (car pair) (cdr pair))) alist))
;;; "strsrch.scm" Search for string from port.
; Written 1995, 1996 by Oleg Kiselyov (oleg@acm.org)
; Modified 1996, 1997, 1998, 2001 by A. Jaffer (agj@alum.mit.edu)
; Modified 2003 by Steve VanDevender (stevev@hexadecimal.uoregon.edu)
; 2013-01 A. Jaffer replaced the skip-vector with an alist

; This code is in the public domain.

;;;(require 'multiarg-apply)		; used in string-subst
;;;(require 'alist)

;;;@ Return the index of the first occurence of chr in str, or #f
(define (string-index str chr)
  (define len (string-length str))
  (do ((pos 0 (+ 1 pos)))
      ((or (>= pos len) (char=? chr (string-ref str pos)))
       (and (< pos len) pos))))
;@
(define (string-index-ci str chr)
  (define len (string-length str))
  (do ((pos 0 (+ 1 pos)))
      ((or (>= pos len) (char-ci=? chr (string-ref str pos)))
       (and (< pos len) pos))))
;@
(define (string-reverse-index str chr)
  (do ((pos (+ -1 (string-length str)) (+ -1 pos)))
      ((or (negative? pos) (char=? (string-ref str pos) chr))
       (and (not (negative? pos)) pos))))
;@
(define (string-reverse-index-ci str chr)
  (do ((pos (+ -1 (string-length str)) (+ -1 pos)))
      ((or (negative? pos) (char-ci=? (string-ref str pos) chr))
       (and (not (negative? pos)) pos))))
;@
(define (substring? pat str)
  (define patlen (string-length pat))
  (define strlen (string-length str))
  (cond ((zero? patlen) 0)		; trivial match
	((>= patlen strlen) (and (= patlen strlen) (string=? pat str) 0))
	;; use faster string-index to match a single-character pattern
	((= 1 patlen) (string-index str (string-ref pat 0)))
	((or (<= strlen (* 2 patlen))
	     (<= patlen 2))
	 (subloop pat patlen str strlen char=?))
	(else
	 ;; compute skip values for search pattern characters
	 ;; for all c not in pat, skip[c] = patlen + 1
	 ;; for c in pat, skip[c] is distance of rightmost occurrence
	 ;;  of c from end of str
	 (let ((skip '()))
	   (define setprop (alist-associator char=?))
	   (do ((i 0 (+ i 1)))
	       ((= i patlen))
	     (set! skip (setprop skip (string-ref pat i) (- patlen i))))
	   (subskip skip pat patlen str strlen char=?)))))
;@
(define (substring-ci? pat str)
  (define patlen (string-length pat))
  (define strlen (string-length str))
  (cond ((zero? patlen) 0)		; trivial match
	((>= patlen strlen) (and (= patlen strlen) (string-ci=? pat str) 0))
	((= 1 patlen) (string-index-ci str (string-ref pat 0)))
	((or (<= strlen (* 2 patlen))
	     (<= patlen 2))
	 (subloop pat patlen str strlen char-ci=?))
	(else
	 (let ((skip '()))
	   (define setprop (alist-associator char-ci=?))
	   (do ((i 0 (+ i 1)))
	       ((= i patlen))
	     (set! skip (setprop skip (string-ref pat i) (- patlen i))))
	   (subskip skip pat patlen str strlen char-ci=?)))))

(define (subskip skip pat patlen str strlen char=)
  (define getprop (alist-inquirer char=?))
  (do ((k patlen (if (< k strlen)
		     (+ k (or (getprop skip (string-ref str k)) (+ patlen 1)))
		     (+ strlen 1))))
      ((or (> k strlen)
	   (do ((i 0 (+ i 1))
		(j (- k patlen) (+ j 1)))
	       ((or (= i patlen)
		    (not (char= (string-ref pat i) (string-ref str j))))
		(= i patlen))))
       (and (<= k strlen) (- k patlen)))))

;;; Assumes that PATLEN > 1
(define (subloop pat patlen str strlen char=)
  (define span (- strlen patlen))
  (define c1 (string-ref pat 0))
  (define c2 (string-ref pat 1))
  (let outer ((pos 0))
    (cond
     ((> pos span) #f)		; nothing was found thru the whole str
     ((not (char= c1 (string-ref str pos)))
      (outer (+ 1 pos)))	; keep looking for the right beginning
     ((not (char= c2 (string-ref str (+ 1 pos))))
      (outer (+ 1 pos)))	 ; could've done pos+2 if c1 == c2....
     (else			  ; two char matched: high probability
					; the rest will match too
      (let inner ((pdx 2) (sdx (+ 2 pos)))
	(if (>= pdx patlen) pos	; the whole pat matched
	    (if (char= (string-ref pat pdx)
		       (string-ref str sdx))
		(inner (+ 1 pdx) (+ 1 sdx))
		;; mismatch after partial match
		(outer (+ 1 pos)))))))))
;@
(define (find-string-from-port? str <input-port> . max-no-char)
  (set! max-no-char (if (null? max-no-char) #f (car max-no-char)))
  (letrec
      ((no-chars-read 0)
       (peeked? #f)
       (my-peek-char			; Return a peeked char or #f
	(lambda () (and (or (not (number? max-no-char))
			    (< no-chars-read max-no-char))
			(let ((c (peek-char <input-port>)))
			  (cond (peeked? c)
				((eof-object? c) #f)
				((procedure? max-no-char)
				 (set! peeked? #t)
				 (if (max-no-char c) #f c))
				((eqv? max-no-char c) #f)
				(else c))))))
       (next-char (lambda () (set! peeked? #f) (read-char <input-port>)
			  (set! no-chars-read  (+ 1 no-chars-read))))
       (match-1st-char			; of the string str
	(lambda ()
	  (let ((c (my-peek-char)))
	    (and c
		 (begin (next-char)
			(if (char=? c (string-ref str 0))
			    (match-other-chars 1)
			    (match-1st-char)))))))
       ;; There has been a partial match, up to the point pos-to-match
       ;; (for example, str[0] has been found in the stream)
       ;; Now look to see if str[pos-to-match] for would be found, too
       (match-other-chars
	(lambda (pos-to-match)
	  (if (>= pos-to-match (string-length str))
	      no-chars-read	       ; the entire string has matched
	      (let ((c (my-peek-char)))
		(and c
		     (if (not (char=? c (string-ref str pos-to-match)))
			 (backtrack 1 pos-to-match)
			 (begin (next-char)
				(match-other-chars (+ 1 pos-to-match)))))))))

       ;; There had been a partial match, but then a wrong char showed up.
       ;; Before discarding previously read (and matched) characters, we check
       ;; to see if there was some smaller partial match. Note, characters read
       ;; so far (which matter) are those of str[0..matched-substr-len - 1]
       ;; In other words, we will check to see if there is such i>0 that
       ;; substr(str,0,j) = substr(str,i,matched-substr-len)
       ;; where j=matched-substr-len - i
       (backtrack
	(lambda (i matched-substr-len)
	  (let ((j (- matched-substr-len i)))
	    (if (<= j 0)
		;; backed off completely to the begining of str
		(match-1st-char)
		(let loop ((k 0))
		  (if (>= k j)
		      (match-other-chars j) ; there was indeed a shorter match
		      (if (char=? (string-ref str k)
				  (string-ref str (+ i k)))
			  (loop (+ 1 k))
			  (backtrack (+ 1 i) matched-substr-len))))))))
       )
    (match-1st-char)))
;@
(define (string-subst text old new . rest)
  (define sub
    (lambda (text)
      (set! text
	    (cond ((equal? "" text) text)
		  ((substring? old text)
		   => (lambda (idx)
			(string-append
			 (substring text 0 idx)
			 new
			 (sub (substring
			       text (+ idx (string-length old))
			       (string-length text))))))
		  (else text)))
      (if (null? rest)
	  text
	  (apply string-subst text rest))))
  (sub text))
;@
(define (count-newlines str)
  (do ((idx (+ -1 (string-length str)) (+ -1 idx))
       (cnt 0 (+ (if (eqv? #\newline (string-ref str idx)) 1 0) cnt)))
      ((<= idx 0) cnt)))
; "prec.scm", dynamically extensible parser/tokenizer	-*-scheme-*-
; Copyright 1989, 1990, 1991, 1992, 1993, 1995, 1997 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

; This file implements:
; * a Pratt style parser.
; * a tokenizer which congeals tokens according to assigned classes of
;   constituent characters.
;
; This module is a significant improvement because grammar can be
; changed dynamically from rulesets which don't need compilation.
; Theoretically, all possibilities of bad input are handled and return
; as much structure as was parsed when the error occured; The symbol
; `?' is substituted for missing input.

; References for the parser are:

; Pratt, V. R.
; Top Down Operator Precendence.
; SIGACT/SIGPLAN
; Symposium on Principles of Programming Languages,
; Boston, 1973, 41-51

; WORKING PAPER 121
; CGOL - an Alternative External Representation For LISP users
; Vaughan R. Pratt
; MIT Artificial Intelligence Lab.
; March 1976

; Mathlab Group,
; MACSYMA Reference Manual, Version Ten,
; Laboratory for Computer Science, MIT, 1983

;;;(require 'string-search)
;;;(require 'string-port)
;;;(require 'delay)
;;;(require 'multiarg-apply)

;@
(define *syn-defs* #f)

(define (tok:peek-char dyn) (peek-char (cadr dyn)))
(define (tok:read-char dyn)
  (let ((c (read-char (cadr dyn))))
    (set-car! (cddddr dyn)
	      (if (or (eqv? c #\newline) (eof-object? c))
		  0
		  (+ 1 (car (cddddr dyn)))))
    c))

(define (prec:warn dyn . msgs)
  (do ((j (+ -1 (car (cddddr dyn))) (+ -8 j)))
      ((> 8 j)
       (do ((i j (+ -1 i)))
	   ((>= 0 i)
	    (display "^ ")
	    (newline)
	    (for-each (lambda (x) (write x) (display #\space)) msgs)
	    (newline))
	 (display #\space)))
    (display slib:tab)))

;; Structure of lexical records.
(define tok:make-rec cons)
(define tok:cc car)
(define tok:sfp cdr)

(define (tok:lookup alist char)
  (if (eof-object? char)
      #f
      (let ((pair (assv char alist)))
	(and pair (cdr pair)))))
;@
(define (tok:char-group group chars chars-proc)
  (map (lambda (token)
;;;      (let ((oldlexrec (tok:lookup *syn-defs* token)))
;;;	(cond ((or (not oldlexrec) (eqv? (tok:cc oldlexrec) group)))
;;;	      (else (math:warn 'cc-of token 'redefined-to- group))))
	 (cons token (tok:make-rec group chars-proc)))
       (cond ((string? chars) (string->list chars))
	     ((char? chars) (list chars))
	     (else chars))))

(define (tokenize dyn)
  (let* ((char (tok:read-char dyn))
	 (rec (tok:lookup (car dyn) char))
	 (proc (and rec (tok:cc rec)))
	 (clist (list char)))
    (cond
     ((not proc) char)
     ((procedure? proc)
      (do ((cl clist (begin (set-cdr! cl (list (tok:read-char dyn))) (cdr cl))))
	  ((proc (tok:peek-char dyn))
	   ((or (tok:sfp rec) (lambda (dyn l) (list->string l)))
	    dyn
	    clist))))
     ((eqv? 0 proc) (tokenize dyn))
     (else
      (do ((cl clist (begin (set-cdr! cl (list (tok:read-char dyn))) (cdr cl))))
	  ((not (let* ((prec (tok:lookup (car dyn) (tok:peek-char dyn)))
		       (cclass (and prec (tok:cc prec))))
		  (or (eqv? cclass proc)
		      (eqv? cclass (+ -1 proc)))))
	   ((tok:sfp rec) dyn clist)))))))

;;; PREC:NUD is the null denotation (function and arguments to call when no
;;;     unclaimed tokens).
;;; PREC:LED is the left denotation (function and arguments to call when
;;;     unclaimed token is on left).
;;; PREC:LBP is the left binding power of this LED.  It is the first
;;; argument position of PREC:LED

(define (prec:nudf alist self)
  (let ((pair (assoc (cons 'nud self) alist)))
    (and pair (cdr pair))))
(define (prec:ledf alist self)
  (let ((pair (assoc (cons 'led self) alist)))
    (and pair (cdr pair))))
(define (prec:lbp alist self)
  (let ((pair (assoc (cons 'led self) alist)))
    (and pair (cadr pair))))

(define (prec:call-or-list proc . args)
  (prec:apply-or-cons proc args))
(define (prec:apply-or-cons proc args)
  (if (procedure? proc) (apply proc args) (cons (or proc '?) args)))

;;; PREC:SYMBOLFY and PREC:DE-SYMBOLFY are not exact inverses.
(define (prec:symbolfy obj)
  (cond ((symbol? obj) obj)
	((string? obj) (string->symbol obj))
	((char? obj) (string->symbol (string obj)))
	(else obj)))

(define (prec:de-symbolfy obj)
  (cond ((symbol? obj) (symbol->string obj))
	(else obj)))

;;;Calls to set up tables.
;@
(define (prec:define-grammar . synlsts)
  (set! *syn-defs* (append (apply append synlsts) *syn-defs*)))
;@
(define (prec:make-led toks . args)
  (map (lambda (tok)
	 (cons (cons 'led (prec:de-symbolfy tok))
	       args))
       (if (pair? toks) toks (list toks))))
(define (prec:make-nud toks . args)
  (map (lambda (tok)
	 (cons (cons 'nud (prec:de-symbolfy tok))
	       args))
       (if (pair? toks) toks (list toks))))

;;; Produce dynamically augmented grammars.
(define prec:process-binds append)

;;(define (prec:replace-rules) some-sort-of-magic-cookie)

;;; Here are the procedures to define high-level grammar, along with
;;; utility functions called during parsing.  The utility functions
;;; (prec:parse-*) could be incorportated into the defining commands,
;;; but tracing these functions is useful for debugging.
;@
(define (prec:delim tk)
  (prec:make-led tk 0 #f))
;@
(define (prec:nofix tk sop . binds)
  (prec:make-nud tk prec:parse-nofix sop (apply append binds)))

(define (prec:parse-nofix dyn self sop binds)
  (let ((dyn (cons (prec:process-binds binds (car dyn)) (cdr dyn))))
    (prec:call-or-list (or sop (prec:symbolfy self)))))
;@
(define (prec:prefix tk sop bp . binds)
  (prec:make-nud tk prec:parse-prefix sop bp (apply append binds)))

(define (prec:parse-prefix dyn self sop bp binds)
  (let ((dyn (cons (prec:process-binds binds (car dyn)) (cdr dyn))))
    (prec:call-or-list (or sop (prec:symbolfy self)) (prec:parse1 dyn bp))))
;@
(define (prec:infix tk sop lbp bp . binds)
  (prec:make-led tk lbp prec:parse-infix sop bp (apply append binds)))

(define (prec:parse-infix dyn left self lbp sop bp binds)
  (let ((dyn (cons (prec:process-binds binds (car dyn)) (cdr dyn))))
    (prec:call-or-list (or sop (prec:symbolfy self)) left (prec:parse1 dyn bp))))
;@
(define (prec:nary tk sop bp)
  (prec:make-led tk bp prec:parse-nary sop bp))

(define (prec:parse-nary dyn left self lbp sop bp)
  (prec:apply-or-cons (or sop (prec:symbolfy self))
		      (cons left (prec:parse-list dyn self bp))))
;@
(define (prec:postfix tk sop lbp . binds)
  (prec:make-led tk lbp prec:parse-postfix sop (apply append binds)))

(define (prec:parse-postfix dyn left self lbp sop binds)
  (let ((dyn (cons (prec:process-binds binds (car dyn)) (cdr dyn))))
    (prec:call-or-list (or sop (prec:symbolfy self)) left)))
;@
(define (prec:prestfix tk sop bp . binds)
  (prec:make-nud tk prec:parse-rest sop bp (apply append binds)))

(define (prec:parse-rest dyn self sop bp binds)
  (let ((dyn (cons (prec:process-binds binds (car dyn)) (cdr dyn))))
    (prec:apply-or-cons (or sop (prec:symbolfy self)) (prec:parse-list dyn #f bp))))
;@
(define (prec:commentfix tk stp match . binds)
  (append
   (prec:make-nud tk prec:parse-nudcomment stp match (apply append binds))
   (prec:make-led tk 220 prec:parse-ledcomment stp match (apply append binds))))

(define (prec:parse-nudcomment dyn self stp match binds)
  (let ((dyn (cons (prec:process-binds binds (car dyn)) (cdr dyn))))
    (tok:read-through-comment dyn stp match)
    (prec:advance dyn)
    (cond ((prec:delim? dyn (force (caddr dyn))) #f)
	  (else (prec:parse1 dyn (cadddr dyn))))))

(define (prec:parse-ledcomment dyn left lbp self stp match binds)
  (let ((dyn (cons (prec:process-binds binds (car dyn)) (cdr dyn))))
    (tok:read-through-comment dyn stp match)
    (prec:advance dyn)
    left))

(define (tok:read-through-comment dyn stp match)
  (set! match (if (char? match)
		  (string match)
		  (prec:de-symbolfy match)))
  (cond ((procedure? stp)
	 (let* ((len #f)
		(str (call-with-output-string
			 (lambda (sp)
			   (set! len (find-string-from-port?
				      match (cadr dyn)
				      (lambda (c) (display c sp) #f)))))))
	   (stp (and len (substring str 0 (- len (string-length match)))))))
	(else (find-string-from-port? match (cadr dyn)))))
;@
(define (prec:matchfix tk sop sep match . binds)
  (define sep-lbp 0)
  (prec:make-nud tk prec:parse-matchfix
		 sop sep-lbp sep match
		 (apply append (prec:delim match) binds)))

(define (prec:parse-matchfix dyn self sop sep-lbp sep match binds)
  (let ((dyn (cons (prec:process-binds binds (car dyn)) (cdr dyn))))
    (cond (sop (prec:apply-or-cons sop (prec:parse-delimited dyn sep sep-lbp match)))
	  ((equal? (force (caddr dyn)) match)
	   (prec:warn dyn 'expression-missing)
	   (prec:advance dyn)
	   '?)
	  (else (let ((ans (prec:parse1 dyn 0))) ;just parenthesized expression
		  (cond ((equal? (force (caddr dyn)) match)
			 (prec:advance dyn))
			((prec:delim? dyn (force (caddr dyn)))
			 (prec:warn dyn 'mismatched-delimiter (force (caddr dyn))
				    'not match)
			 (prec:advance dyn))
			(else (prec:warn dyn 'delimiter-expected--ignoring-rest
					 (force (caddr dyn)) 'expected match
					 'or-delimiter)
			      (do () ((prec:delim? dyn (force (caddr dyn))))
				(prec:parse1 dyn 0))))
		  ans)))))
;@
(define (prec:inmatchfix tk sop sep match lbp . binds)
  (define sep-lbp 0)
  (prec:make-led tk lbp prec:parse-inmatchfix
		 sop sep-lbp sep match
		 (apply append (prec:delim match) binds)))

(define (prec:parse-inmatchfix dyn left self lbp sop sep-lbp sep match binds)
  (let ((dyn (cons (prec:process-binds binds (car dyn)) (cdr dyn))))
    (prec:apply-or-cons sop (cons left (prec:parse-delimited dyn sep sep-lbp match)))))

;;;; Here is the code which actually parses.

(define (prec:advance dyn)
  (set-car! (cddr dyn) (delay (tokenize dyn))))
(define (prec:advance-return-last dyn)
  (let ((last (and (caddr dyn) (force (caddr dyn)))))
    (prec:advance dyn)
    last))

(define (prec:nudcall dyn self)
  (let ((pob (prec:nudf (car dyn) self)))
    (cond
     (pob (let ((proc (car pob)))
	    (cond ((procedure? proc) (apply proc dyn self (cdr pob)))
		  (proc (cons proc (cdr pob)))
		  (else '?))))
     ((char? self) (prec:warn dyn 'extra-separator)
      (prec:advance dyn)
      (prec:nudcall dyn  (force (caddr dyn))))
     ((string? self) (string->symbol self))
     (else self))))

(define (prec:ledcall dyn left self)
  (let* ((pob (prec:ledf (car dyn) self)))
    (apply (cadr pob) dyn left self (cdr pob))))

;;; PREC:PARSE1 is the heart.
(define (prec:parse1 dyn bp)
  (do ((left (prec:nudcall dyn (prec:advance-return-last dyn))
	     (prec:ledcall dyn left (prec:advance-return-last dyn))))
      ((or (>= bp 200)		       ;to avoid unneccesary lookahead
	   (>= bp (or (prec:lbp (car dyn) (force (caddr dyn))) 0))
	   (not left))
       left)))

(define (prec:delim? dyn token)
  (or (eof-object? token) (<= (or (prec:lbp (car dyn) token) 220) 0)))

(define (prec:parse-list dyn sep bp)
  (cond ((prec:delim? dyn (force (caddr dyn)))
	 (prec:warn dyn 'expression-missing)
	 '(?))
	(else
	 (let ((f (prec:parse1 dyn bp)))
	   (cons f (cond ((equal? (force (caddr dyn)) sep)
			  (prec:advance dyn)
			  (cond ((equal? (force (caddr dyn)) sep)
				 (prec:warn dyn 'expression-missing)
				 (prec:advance dyn)
				 (cons '? (prec:parse-list dyn sep bp)))
				((prec:delim? dyn (force (caddr dyn)))
				 (prec:warn dyn 'expression-missing)
				 '(?))
				(else (prec:parse-list dyn sep bp))))
			 ((prec:delim? dyn (force (caddr dyn))) '())
			 ((not sep) (prec:parse-list dyn sep bp))
			 ((prec:delim? dyn sep) (prec:warn dyn 'separator-missing)
			  (prec:parse-list dyn sep bp))
			 (else '())))))))

(define (prec:parse-delimited dyn sep bp delim)
  (cond ((equal? (force (caddr dyn)) sep)
	 (prec:warn dyn 'expression-missing)
	 (prec:advance dyn)
	 (cons '? (prec:parse-delimited dyn sep bp delim)))
	((prec:delim? dyn (force (caddr dyn)))
	 (if (not (equal? (force (caddr dyn)) delim))
	     (prec:warn dyn 'mismatched-delimiter (force (caddr dyn))
			'expected delim))
	 (if (not sep) (prec:warn dyn 'expression-missing))
	 (prec:advance dyn)
	 (if sep '() '(?)))
	(else (let ((ans (prec:parse-list dyn sep bp)))
		(cond ((equal? (force (caddr dyn)) delim))
		      ((prec:delim? dyn (force (caddr dyn)))
		       (prec:warn dyn 'mismatched-delimiter (force (caddr dyn))
				  'expecting delim))
		      (else (prec:warn dyn 'delimiter-expected--ignoring-rest
				       (force (caddr dyn)) '...)
			    (do () ((prec:delim? dyn (force (caddr dyn))))
			      (prec:parse1 dyn bp))))
		(prec:advance dyn)
		ans))))
;@
(define (prec:parse grammar delim column . ports)
  (define port (if (null? ports) (current-input-port) (car ports)))
  (set! delim (prec:de-symbolfy delim))
  (let ((dyn (list (append (prec:delim delim) grammar)
		   port
		   #f
		   0
		   column)))
    (prec:advance dyn)		   ; setup prec:token with first token
    (cond ((eof-object? (force (caddr dyn))) (force (caddr dyn)))
	  ((equal? (force (caddr dyn)) delim) #f)
	  (else
	   (let ((ans (prec:parse1 dyn 0)))
	     (cond ((eof-object? (force (caddr dyn))))
		   ((equal? (force (caddr dyn)) delim))
		   (else (prec:warn dyn
				    'delimiter-expected--ignoring-rest
				    (force (caddr dyn)) 'not delim)
			 (do () ((or (equal? (force (caddr dyn)) delim)
				     (eof-object? (force (caddr dyn)))))
			   (prec:advance dyn))))
	     ans)))))
;@
(define tok:decimal-digits "0123456789")
(define tok:upper-case "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define tok:lower-case "abcdefghijklmnopqrstuvwxyz")
(define tok:whitespaces
  (do ((i (+ -1 (min 256 char-code-limit)) (+ -1 i))
       (ws "" (if (char-whitespace? (integer->char i))
		  (string-append ws (string (integer->char i)))
		  ws)))
      ((negative? i) ws)))

;;;;The parse tables.
;;; Definitions accumulate in top-level variable *SYN-DEFS*.
(set! *syn-defs* '())		       ;Make sure *SYN-DEFS* is empty.

;;; Ignore Whitespace characters.
(prec:define-grammar (tok:char-group 0 tok:whitespaces #f))

;;; On MS-DOS systems, <ctrl>-Z (26) needs to be ignored in order to
;;; avoid problems at end of files.
(case (software-type)
  ((ms-dos)
   (if (not (char-whitespace? (integer->char 26)))
       (prec:define-grammar (tok:char-group 0 (integer->char 26) #f))
       )))

;;;@ Save these convenient definitions.
(define *syn-ignore-whitespace* *syn-defs*)
(set! *syn-defs* '())

;;(begin (trace-all "prec.scm") (set! *qp-width* 333))
;;(pretty-print (grammar-read-tab (get-grammar 'standard)))
;;(prec:trace)


    ))
