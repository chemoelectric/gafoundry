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
        (gafoundry-lib uc-string))

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

(check (not (uc-string? 3)))
(check (uc-string? (make-uc-string 3)))
(check (uc-string-null? (make-uc-string 0)))
(check (not (uc-string-null? (make-uc-string 3))))
(check (zero? (uc-string-length (make-uc-string 0))))
(check (= 3 (uc-string-length (make-uc-string 3))))
(check (uc-string-null? (uc-string)))
(check (= 3 (uc-string-length (uc-string #\a #\b #\c))))
(check (= 3 (uc-string-length (list->uc-string '(#\a #\b #\c)))))
(check (equal? '(#\a #\b #\c)
               (uc-string->list (uc-string #\a #\b #\c))))
(check (string=? "fred flintstone"
                 (uc-string->string
                  (string->uc-string "fred flintstone"))))
(check (string=? "🔠🔡🉐🈹🟫⬛"
                 (uc-string->string
                  (string->uc-string "🔠🔡🉐🈹🟫⬛"))))
(check (= 6 (uc-string-length
             (string->uc-string "🔠🔡🉐🈹🟫⬛"))))
(check (string=? "srfi-152"
                 (uc-string->string
                  (symbol->uc-string 'srfi-152))))
(check (eq? 'srfi-152
            (uc-string->symbol (symbol->uc-string 'srfi-152))))
(check (string=? "12345" (uc-string->string
                          (number->uc-string 12345))))
(check (string=? "30071" (uc-string->string
                          (number->uc-string 12345 8))))
(check (= 12345 (uc-string->number (string->uc-string "12345"))))
(check (= 12345 (uc-string->number
                 (string->uc-string "30071") 8)))
(check (uc-string=? (string->uc-string "🔠🔡🉐🈹🟫⬛")
                    (string->uc-string "🔠🔡🉐🈹🟫⬛")))
(check (not (uc-string=? (string->uc-string "🔠🔡🉐🈹🟫⬛")
                         (string->uc-string "fred flintstone"))))
(let ((str (string->uc-string "🔠🔡🉐🈹🟫⬛")))
  (check (char=? #\🔠 (uc-string-ref str 0)))
  (check (char=? #\🔡 (uc-string-ref str 1)))
  (check (char=? #\🉐 (uc-string-ref str 2)))
  (check (char=? #\🈹 (uc-string-ref str 3)))
  (check (char=? #\🟫 (uc-string-ref str 4)))
  (check (char=? #\⬛ (uc-string-ref str 5)))
  (uc-string-set! str 0 #\a)
  (uc-string-set! str 1 #\b)
  (uc-string-set! str 2 #\c)
  (uc-string-set! str 3 #\d)
  (uc-string-set! str 4 #\e)
  (uc-string-set! str 5 #\f)
  (check (char=? #\a (uc-string-ref str 0)))
  (check (char=? #\b (uc-string-ref str 1)))
  (check (char=? #\c (uc-string-ref str 2)))
  (check (char=? #\d (uc-string-ref str 3)))
  (check (char=? #\e (uc-string-ref str 4)))
  (check (char=? #\f (uc-string-ref str 5))))
(check (uc-string=? (string->uc-string "red")
                    (uc-string-copy
                     (string->uc-string "fred flintstone")
                     1 4)))
(check (uc-string=? (string->uc-string "flintstone")
                    (uc-string-copy
                     (string->uc-string "fred flintstone") 5)))
(check (uc-string=? (string->uc-string "fred flintstone")
                    (uc-string-copy
                     (string->uc-string "fred flintstone"))))
(let ((str (make-uc-string 10 #\.)))
  (uc-string-copy! str 9 (string->uc-string "🔠🔡🉐🈹🟫⬛") 4 5)
  (check (uc-string=? str (string->uc-string ".........🟫"))))
(let ((str (make-uc-string 10 #\.)))
  (uc-string-copy! str 4 (string->uc-string "🔠🔡🉐🈹🟫⬛") 3)
  (check (uc-string=? str (string->uc-string "....🈹🟫⬛..."))))
(let ((str (make-uc-string 10 #\.)))
  (uc-string-copy! str 2 (string->uc-string "🔠🔡🉐🈹🟫⬛"))
  (check (uc-string=? str (string->uc-string "..🔠🔡🉐🈹🟫⬛.."))))
(check (uc-string=? (string->uc-string "23456")
                    (uc-string-map (lambda (ch)
                                     (integer->char
                                      (+ 1 (char->integer ch))))
                                   (string->uc-string "12345"))))

(format #t "pass: ~a~&" pass-count)
(format #t "~a: ~a~&"
        (if (zero? fail-count) "fail" "FAIL")
        fail-count)
(exit (zero? fail-count))
