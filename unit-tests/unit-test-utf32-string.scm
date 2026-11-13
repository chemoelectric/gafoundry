#!/usr/bin/env scheme-r7rs

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

(import (scheme base)
        (scheme process-context)
        (gafoundry-lib format)
        (gafoundry-lib utf32-string))

(define pass-count 0)
(define fail-count 0)

(define-syntax check
  (syntax-rules ()
    ((check expr)
     (begin
       (format #t "~s" 'expr)
       (if expr
         (begin
           (set! pass-count (+ pass-count 1))
           (format #t " \x21D2; pass~&"))
         (begin
           (set! fail-count (+ fail-count 1))
           (format #t " \x21D2; FAIL~&")))))))

(check (not (utf32-string? 3)))
(check (utf32-string? (make-utf32-string 3)))
(check (utf32-string-null? (make-utf32-string 0)))
(check (not (utf32-string-null? (make-utf32-string 3))))
(check (zero? (utf32-string-length (make-utf32-string 0))))
(check (= 3 (utf32-string-length (make-utf32-string 3))))

(format #t "pass: ~a~&" pass-count)
(format #t "fail: ~a~&" fail-count)
(exit (zero? fail-count))
