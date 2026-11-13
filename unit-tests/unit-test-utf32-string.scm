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
(check (utf32-string-null? (utf32-string)))
(check (= 3 (utf32-string-length (utf32-string #\a #\b #\c))))
(check (= 3 (utf32-string-length (list->utf32-string '(#\a #\b #\c)))))
(check (equal? '(#\a #\b #\c)
               (utf32-string->list (utf32-string #\a #\b #\c))))
(check (string=? "fred flintstone"
                 (utf32-string->string
                  (string->utf32-string "fred flintstone"))))
(check (string=? "🔠🔡🉐🈹🟫⬛"
                 (utf32-string->string
                  (string->utf32-string "🔠🔡🉐🈹🟫⬛"))))
(check (= 6 (utf32-string-length
             (string->utf32-string "🔠🔡🉐🈹🟫⬛"))))
(check (string=? "srfi-152"
                 (utf32-string->string
                  (symbol->utf32-string 'srfi-152))))
(check (eq? 'srfi-152
            (utf32-string->symbol (symbol->utf32-string 'srfi-152))))
(check (string=? "12345" (utf32-string->string
                          (number->utf32-string 12345))))
(check (string=? "30071" (utf32-string->string
                          (number->utf32-string 12345 8))))
(check (= 12345 (utf32-string->number (string->utf32-string "12345"))))
(check (= 12345 (utf32-string->number
                 (string->utf32-string "30071") 8)))

(format #t "pass: ~a~&" pass-count)
(format #t "~a: ~a~&"
        (if (zero? fail-count) "fail" "FAIL")
        fail-count)
(exit (zero? fail-count))
