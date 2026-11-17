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
        (scheme case-lambda)
        (scheme char)
        (scheme file)
        (scheme process-context)
        (gafoundry-lib format)
        (gafoundry-lib uc-string))

;;;
;;; Run with argument “pretty” to pretty-print the test expressions.
;;;
(define *pretty*
  (make-parameter
   (and (<= 2 (length (command-line)))
        (string=? "pretty" (cadr (command-line))))))

(define pass-count 0)
(define fail-count 0)

(define-syntax check
  (syntax-rules ()
    ((check expr)
     (begin
       (if (*pretty*)
         (format #t "~y" 'expr)
         (format #t "~s" 'expr))
       (if expr
         (begin
           (set! pass-count (+ pass-count 1))
           (format #t " \x21D2; pass~&"))
         (begin
           (set! fail-count (+ fail-count 1))
           (format #t " \x21D2; FAIL~&")))))))

(define (complement pred)
  (case-lambda
    ((arg)
     (not (pred arg)))
    (arg*
     (not (apply pred arg*)))))

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
(check (equal? '(#\b #\c)
               (uc-string->list (uc-string #\a #\b #\c) 1)))
(check (equal? '(#\b #\c)
               (uc-string->list (uc-string #\a #\b #\c #\d)
                                1 3)))
(check (equal? '() (uc-string->list (uc-string #\a #\b #\c #\d)
                                    1 1)))
(check (equal? '() (uc-string->list (uc-string))))
(check (equal? '#(#\a #\b #\c)
               (uc-string->vector (uc-string #\a #\b #\c))))
(check (equal? '#(#\b #\c)
               (uc-string->vector (uc-string #\a #\b #\c) 1)))
(check (equal? '#(#\b #\c)
               (uc-string->vector (uc-string #\a #\b #\c #\d)
                                  1 3)))
(check (equal? '#() (uc-string->vector (uc-string #\a #\b #\c #\d)
                                       1 1)))
(check (equal? '#() (uc-string->vector (uc-string))))
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
                    (uc-substring
                     (string->uc-string "fred flintstone")
                     1 4)))
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
(check (uc-string=? (string->uc-string "1122334455")
                    (uc-string-map (lambda (ch) (uc-string ch ch))
                                   (string->uc-string "12345"))))
(check (uc-string=? (string->uc-string "55555")
                    (uc-string-map (lambda (ch1 ch2)
                                     (integer->char
                                      (+ (char->integer ch1)
                                         (char->integer ch2)
                                         -48)))
                                   (string->uc-string "12345678")
                                   (string->uc-string "43210"))))
(check (uc-string=? (string->uc-string "1a2b3c4d5e")
                    (uc-string-map (lambda (ch1 ch2)
                                     (uc-string ch1 ch2))
                                   (string->uc-string "12345678")
                                   (string->uc-string "abcde"))))
(check (uc-string=? (string->uc-string "1a2b3c4d5e")
                    (uc-string-map (lambda (ch1 ch2)
                                     (string ch1 ch2))
                                   (string->uc-string "12345678")
                                   (string->uc-string "abcde"))))
(let ((vec (make-vector 5))
      (i 0))
  (uc-string-for-each (lambda (ch)
                        (vector-set! vec i ch)
                        (set! i (+ i 1)))
                      (string->uc-string "abcde"))
  (check (equal? vec '#(#\a #\b #\c #\d #\e))))
(let ((vec (make-vector 5))
      (i 0))
  (uc-string-for-each (lambda (ch1 ch2 ch3)
                        (vector-set! vec i (string ch1 ch2 ch3))
                        (set! i (+ i 1)))
                      (string->uc-string "abcde")
                      (string->uc-string "12345")
                      (string->uc-string ".!@#%"))
  (check (equal? vec '#("a1." "b2!" "c3@" "d4#" "e5%"))))
(check (equal? '(#\a #\b #\c #\d #\e)
               (uc-string-fold cons '()
                               (string->uc-string "edcba"))))
(check (equal? '(#\a #\b #\c)
               (uc-string-fold cons '()
                               (string->uc-string "edcba") 2)))
(check (equal? '(#\b #\c #\d)
               (uc-string-fold cons '()
                               (string->uc-string "edcba") 1 4)))
(check (equal? '(#\a #\b #\c #\d #\e)
               (uc-string-fold-right cons '()
                                     (string->uc-string "abcde"))))
(check (equal? '(#\c #\d #\e)
               (uc-string-fold-right cons '()
                                     (string->uc-string "abcde") 2)))
(check (equal? '(#\b #\c #\d)
               (uc-string-fold-right cons '()
                                     (string->uc-string "abcde") 1 4)))
(check (uc-string=? (string->uc-string "")
                    (uc-string-concatenate '())))
(check (uc-string=? (string->uc-string "fred and wilma")
                    (uc-string-append (string->uc-string "fred")
                                      (string->uc-string " ")
                                      (string->uc-string "and")
                                      (string->uc-string " ")
                                      (string->uc-string "wilma"))))
(check (not (uc-string-index (string->uc-string "fred flintstone")
                             (lambda (ch) (char=? ch #\q)))))
(check (= 3 (uc-string-index (string->uc-string "fred flintstone")
                             (lambda (ch) (char=? ch #\d)))))
(check (= 8 (uc-string-index (string->uc-string "fred flintstone")
                             (lambda (ch) (char=? ch #\n)) 5)))
(check (not (uc-string-index (string->uc-string "fred flintstone")
                             (lambda (ch) (char=? ch #\n)) 5 8)))
(check (= 8 (uc-string-index (string->uc-string "fred flintstone")
                             (lambda (ch) (char=? ch #\n)) 5 9)))
(check (not (uc-string-skip (string->uc-string "fred flintstone")
                            (lambda (ch)
                              (or (char-alphabetic? ch)
                                  (char-whitespace? ch))))))
(check (= 4 (uc-string-skip (string->uc-string "fred flintstone")
                            (lambda (ch) (char-alphabetic? ch)))))
(check (= 4 (uc-string-skip (string->uc-string "fred flintstone")
                            (lambda (ch) (char-alphabetic? ch)) 4)))
(check (not (uc-string-skip (string->uc-string "fred flintstone")
                            (lambda (ch) (char-alphabetic? ch)) 5)))
(check (= 4 (uc-string-skip (string->uc-string "fred flintstone")
                            (lambda (ch) (char-alphabetic? ch)) 3 5)))
(check (not (uc-string-skip (string->uc-string "fred flintstone")
                            (lambda (ch) (char-alphabetic? ch)) 3 4)))
(check (not (uc-string-index-right
             (string->uc-string "fred flintstone")
             (lambda (ch) (char=? ch #\q)))))
(check (= 13 (uc-string-index-right
              (string->uc-string "fred flintstone")
              (lambda (ch) (char=? ch #\n)))))
(check (= 13 (uc-string-index-right
              (string->uc-string "fred flintstone")
              (lambda (ch) (char=? ch #\n)) 13)))
(check (not (uc-string-index-right
             (string->uc-string "fred flintstone")
             (lambda (ch) (char=? ch #\n)) 14)))
(check (= 8 (uc-string-index-right
             (string->uc-string "fred flintstone")
             (lambda (ch) (char=? ch #\n)) 8 13)))
(check (not (uc-string-skip-right
             (string->uc-string "fred flintstone")
             (lambda (ch)
               (or (char-alphabetic? ch)
                   (char-whitespace? ch))))))
(check (= 4 (uc-string-skip-right
             (string->uc-string "fred flintstone")
             (lambda (ch) (char-alphabetic? ch)))))
(check (= 4 (uc-string-skip-right
             (string->uc-string "fred flintstone")
             (lambda (ch) (char-alphabetic? ch)) 4)))
(check (not (uc-string-skip-right
             (string->uc-string "fred flintstone")
             (lambda (ch) (char-alphabetic? ch)) 5)))
(check (= 4 (uc-string-skip-right
             (string->uc-string "fred flintstone")
             (lambda (ch) (char-alphabetic? ch)) 4 5)))
(check (not (uc-string-skip-right
             (string->uc-string "fred flintstone")
             (lambda (ch) (char-alphabetic? ch)) 4 4)))
(check (uc-string=? (string->uc-string "fred and wilma")
                    (uc-string-join
                     `(,(string->uc-string "fred")
                       ,(string->uc-string "and")
                       ,(string->uc-string "wilma")))))
(check (uc-string=? (string->uc-string "fred, wilma, barney, betty")
                    (uc-string-join
                     `(,(string->uc-string "fred")
                       ,(string->uc-string "wilma")
                       ,(string->uc-string "barney")
                       ,(string->uc-string "betty"))
                     (string->uc-string ", "))))
(check (uc-string=? (string->uc-string "fred, wilma, barney, betty")
                    (uc-string-join
                     `(,(string->uc-string "fred")
                       ,(string->uc-string "wilma")
                       ,(string->uc-string "barney")
                       ,(string->uc-string "betty"))
                     (string->uc-string ", ") 'infix)))
(check (uc-string-null? (uc-string-join '())))
(check (uc-string-null? (uc-string-join
                         '() (string->uc-string ", ") 'infix)))
(check (guard (condition (#t #t))
         (begin
           (uc-string-join '() (string->uc-string ", ") 'strict-infix)
           #f)))
(check (uc-string=? (string->uc-string "fred, wilma, barney, betty")
                    (uc-string-join
                     `(,(string->uc-string "fred")
                       ,(string->uc-string "wilma")
                       ,(string->uc-string "barney")
                       ,(string->uc-string "betty"))
                     (string->uc-string ", ") 'strict-infix)))
(check (uc-string=? (string->uc-string "fred. wilma. barney. betty. ")
                    (uc-string-join
                     `(,(string->uc-string "fred")
                       ,(string->uc-string "wilma")
                       ,(string->uc-string "barney")
                       ,(string->uc-string "betty"))
                     (string->uc-string ". ") 'suffix)))
(check (uc-string=? (string->uc-string "=fred=wilma=barney=betty")
                    (uc-string-join
                     `(,(string->uc-string "fred")
                       ,(string->uc-string "wilma")
                       ,(string->uc-string "barney")
                       ,(string->uc-string "betty"))
                     (string->uc-string "=") 'prefix)))
(check (uc-string=? (string->uc-string "cba")
                    (reverse-list->uc-string '(#\a #\b #\c))))
(check (equal? '(#\a #\b #\c)
               (uc-string->reverse-list (string->uc-string "cba"))))
(let-values (((s t)
              (uc-string-span (string->uc-string "fred flintstone")
                              char-alphabetic?)))
  (check (uc-string=? s (string->uc-string "fred")))
  (check (uc-string=? t (string->uc-string " flintstone"))))
(let-values (((s t)
              (uc-string-span (string->uc-string "...fred flintstone")
                              char-alphabetic? 3)))
  (check (uc-string=? s (string->uc-string "fred")))
  (check (uc-string=? t (string->uc-string " flintstone"))))
(let-values (((s t)
              (uc-string-span (string->uc-string
                               "...fred flintstone...")
                              char-alphabetic? 3 18)))
  (check (uc-string=? s (string->uc-string "fred")))
  (check (uc-string=? t (string->uc-string " flintstone"))))
(let-values (((s t)
              (uc-string-break (string->uc-string "fred flintstone")
                               (complement char-alphabetic?))))
  (check (uc-string=? s (string->uc-string "fred")))
  (check (uc-string=? t (string->uc-string " flintstone"))))
(let-values (((s t)
              (uc-string-break (string->uc-string "...fred flintstone")
                               (complement char-alphabetic?) 3)))
  (check (uc-string=? s (string->uc-string "fred")))
  (check (uc-string=? t (string->uc-string " flintstone"))))
(let-values (((s t)
              (uc-string-break (string->uc-string
                                "...fred flintstone...")
                               (complement char-alphabetic?) 3 18)))
  (check (uc-string=? s (string->uc-string "fred")))
  (check (uc-string=? t (string->uc-string " flintstone"))))
(let-values (((s t)
              (uc-string-span (string->uc-string "fred flintstone")
                              char-numeric?)))
  (check (uc-string=? s (string->uc-string "")))
  (check (uc-string=? t (string->uc-string "fred flintstone"))))
(let-values (((s t)
              (uc-string-span (string->uc-string "fred flintstone")
                              (complement char-numeric?))))
  (check (uc-string=? s (string->uc-string "fred flintstone")))
  (check (uc-string=? t (string->uc-string ""))))
(let-values (((s t)
              (uc-string-break (string->uc-string "fred flintstone")
                               (complement char-numeric?))))
  (check (uc-string=? s (string->uc-string "")))
  (check (uc-string=? t (string->uc-string "fred flintstone"))))
(let-values (((s t)
              (uc-string-break (string->uc-string "fred flintstone")
                               char-numeric?)))
  (check (uc-string=? s (string->uc-string "fred flintstone")))
  (check (uc-string=? t (string->uc-string ""))))
(let-values (((s t)
              (uc-string-span (string->uc-string "...fred flintstone")
                              char-numeric? 3)))
  (check (uc-string=? s (string->uc-string "")))
  (check (uc-string=? t (string->uc-string "fred flintstone"))))
(let-values (((s t)
              (uc-string-span (string->uc-string "...fred flintstone")
                              (complement char-numeric?) 3)))
  (check (uc-string=? s (string->uc-string "fred flintstone")))
  (check (uc-string=? t (string->uc-string ""))))
(let-values (((s t)
              (uc-string-break (string->uc-string "...fred flintstone")
                               (complement char-numeric?) 3)))
  (check (uc-string=? s (string->uc-string "")))
  (check (uc-string=? t (string->uc-string "fred flintstone"))))
(let-values (((s t)
              (uc-string-break (string->uc-string "...fred flintstone")
                               char-numeric? 3)))
  (check (uc-string=? s (string->uc-string "fred flintstone")))
  (check (uc-string=? t (string->uc-string ""))))
(let-values (((s t)
              (uc-string-span (string->uc-string
                               "...fred flintstone...")
                              char-numeric? 3 18)))
  (check (uc-string=? s (string->uc-string "")))
  (check (uc-string=? t (string->uc-string "fred flintstone"))))
(let-values (((s t)
              (uc-string-span (string->uc-string
                               "...fred flintstone...")
                              (complement char-numeric?) 3 18)))
  (check (uc-string=? s (string->uc-string "fred flintstone")))
  (check (uc-string=? t (string->uc-string ""))))
(let-values (((s t)
              (uc-string-break (string->uc-string
                                "...fred flintstone...")
                               (complement char-numeric?) 3 18)))
  (check (uc-string=? s (string->uc-string "")))
  (check (uc-string=? t (string->uc-string "fred flintstone"))))
(let-values (((s t)
              (uc-string-break (string->uc-string
                                "...fred flintstone...")
                               char-numeric? 3 18)))
  (check (uc-string=? s (string->uc-string "fred flintstone")))
  (check (uc-string=? t (string->uc-string ""))))
(check (uc-string=? (string->uc-string "fred")
                    (uc-string-take-while
                     (string->uc-string "fred flintstone")
                     char-alphabetic?)))
(check (uc-string=? (string->uc-string "red")
                    (uc-string-take-while
                     (string->uc-string "fred flintstone")
                     char-alphabetic? 1)))
(check (uc-string=? (string->uc-string "re")
                    (uc-string-take-while
                     (string->uc-string "fred flintstone")
                     char-alphabetic? 1 3)))
(check (uc-string=? (string->uc-string "stone")
                    (uc-string-take-while
                     (string->uc-string "fred flintstone")
                     char-alphabetic? 10)))
(check (uc-string=? (string->uc-string "ston")
                    (uc-string-take-while
                     (string->uc-string "fred flintstone")
                     char-alphabetic? 10 14)))
(check (uc-string=? (string->uc-string "stone")
                    (uc-string-take-while
                     (string->uc-string "fred flintstone")
                     char-alphabetic? 10 15)))
(check (uc-string=? (string->uc-string "")
                    (uc-string-take-while
                     (string->uc-string "fred flintstone")
                     char-alphabetic? 15 15)))
(check (uc-string=? (string->uc-string " flintstone")
                    (uc-string-drop-while
                     (string->uc-string "fred flintstone")
                     char-alphabetic?)))
(check (uc-string=? (string->uc-string " flintstone")
                    (uc-string-drop-while
                     (string->uc-string "fred flintstone")
                     char-alphabetic? 4)))
(check (uc-string=? (string->uc-string "")
                    (uc-string-drop-while
                     (string->uc-string "fred flintstone")
                     char-alphabetic? 5)))
(check (uc-string=? (string->uc-string " flintston")
                    (uc-string-drop-while
                     (string->uc-string "fred flintstone")
                     char-alphabetic? 0 14)))
(check (uc-string=? (string->uc-string " flintstone")
                    (uc-string-trim
                     (string->uc-string "fred flintstone")
                     char-alphabetic?)))
(check (uc-string=? (string->uc-string " flintstone")
                    (uc-string-trim
                     (string->uc-string "fred flintstone")
                     char-alphabetic? 4)))
(check (uc-string=? (string->uc-string "")
                    (uc-string-trim
                     (string->uc-string "fred flintstone")
                     char-alphabetic? 5)))
(check (uc-string=? (string->uc-string " flintston")
                    (uc-string-trim
                     (string->uc-string "fred flintstone")
                     char-alphabetic? 0 14)))
(check (uc-string=? (string->uc-string " flintston")
                    (uc-string-trim
                     (string->uc-string "fred flintstone")
                     char-alphabetic? 0 14)))
(check (uc-string=? (string->uc-string "flintstone")
                    (uc-string-trim
                     (string->uc-string "\n\n    \tflintstone"))))
(check (uc-string=? (string->uc-string "flintstone")
                    (uc-string-take-while-right
                     (string->uc-string "fred flintstone")
                     char-alphabetic?)))
(check (uc-string=? (string->uc-string "stone")
                    (uc-string-take-while-right
                     (string->uc-string "fred flintstone")
                     char-alphabetic? 10)))
(check (uc-string=? (string->uc-string "st")
                    (uc-string-take-while-right
                     (string->uc-string "fred flintstone")
                     char-alphabetic? 10 12)))
(check (uc-string=? (string->uc-string "fred")
                    (uc-string-take-while-right
                     (string->uc-string "fred flintstone")
                     char-alphabetic? 0 4)))
(check (uc-string=? (string->uc-string "fred ")
                    (uc-string-drop-while-right
                     (string->uc-string "fred flintstone")
                     char-alphabetic?)))
(check (uc-string=? (string->uc-string "ed ")
                    (uc-string-drop-while-right
                     (string->uc-string "fred flintstone")
                     char-alphabetic? 2)))
(check (uc-string=? (string->uc-string "fred ")
                    (uc-string-drop-while-right
                     (string->uc-string "fred flintstone")
                     char-alphabetic? 0 5)))
(check (uc-string=? (string->uc-string "")
                    (uc-string-drop-while-right
                     (string->uc-string "fred flintstone")
                     char-alphabetic? 0 4)))
(check (uc-string=? (string->uc-string "fred ")
                    (uc-string-trim-right
                     (string->uc-string "fred flintstone")
                     char-alphabetic?)))
(check (uc-string=? (string->uc-string "ed ")
                    (uc-string-trim-right
                     (string->uc-string "fred flintstone")
                     char-alphabetic? 2)))
(check (uc-string=? (string->uc-string "fred ")
                    (uc-string-trim-right
                     (string->uc-string "fred flintstone")
                     char-alphabetic? 0 5)))
(check (uc-string=? (string->uc-string "")
                    (uc-string-trim-right
                     (string->uc-string "fred flintstone")
                     char-alphabetic? 0 4)))
(check (uc-string=? (string->uc-string "fred")
                    (uc-string-trim-right
                     (string->uc-string "fred      \t\t\t\n"))))
(check (uc-string=? (string->uc-string "fred")
                    (uc-string-trim-both
                     (string->uc-string "\r \r \rfred      \t\t\t\n"))))
(check (uc-string=? (string->uc-string "fred")
                    (uc-string-trim-both
                     (string->uc-string "...1234fred1234...")
                     char-numeric? 3 15)))
(check (uc-string=? (string->uc-string "fred")
                    (uc-string-trim-both
                     (string->uc-string "...1234fred1234567")
                     char-numeric? 3)))
(let-values (((s t)
              (uc-string-span-right
               (string->uc-string "fred flintstone")
               char-alphabetic?)))
  (check (uc-string=? s (string->uc-string "fred ")))
  (check (uc-string=? t (string->uc-string "flintstone"))))
(let-values (((s t)
              (uc-string-break-right
               (string->uc-string "fred flintstone")
               (complement char-alphabetic?))))
  (check (uc-string=? s (string->uc-string "fred ")))
  (check (uc-string=? t (string->uc-string "flintstone"))))
(let-values (((s t)
              (uc-string-span-right
               (string->uc-string "fred flintstone")
               char-alphabetic? 1)))
  (check (uc-string=? s (string->uc-string "red ")))
  (check (uc-string=? t (string->uc-string "flintstone"))))
(let-values (((s t)
              (uc-string-break-right
               (string->uc-string "fred flintstone")
               (complement char-alphabetic?) 1)))
  (check (uc-string=? s (string->uc-string "red ")))
  (check (uc-string=? t (string->uc-string "flintstone"))))
(let-values (((s t)
              (uc-string-span-right
               (string->uc-string "fred flintstone")
               char-alphabetic? 1 10)))
  (check (uc-string=? s (string->uc-string "red ")))
  (check (uc-string=? t (string->uc-string "flint"))))
(let-values (((s t)
              (uc-string-break-right
               (string->uc-string "fred flintstone")
               (complement char-alphabetic?) 1 10)))
  (check (uc-string=? s (string->uc-string "red ")))
  (check (uc-string=? t (string->uc-string "flint"))))
(check (= 15 (uc-string-contains
              (string->uc-string "eek -- what a geek.")
              (string->uc-string "ee") 12 18)))
(check (= 15 (uc-string-contains
              (string->uc-string "eek -- what a geek.")
              (string->uc-string "cheese") 12 18 2 4)))
(check (= 0 (uc-string-contains
             (string->uc-string "eek -- what a geek.")
             (string->uc-string "eek -- what a geek."))))
(check (not (uc-string-contains
             (string->uc-string "eek -- what a geek.")
             (string->uc-string "eek -- what a jeek."))))
(check (not (uc-string-contains
             (string->uc-string "eek -- what a geek.")
             (string->uc-string "@"))))
(check (= 0 (uc-string-contains
             (string->uc-string "eek -- what a geek.")
             (string->uc-string ""))))
(check (= 4 (uc-string-contains
             (string->uc-string "eek -- what a geek.")
             (string->uc-string "-"))))
(check (= 4 (uc-string-contains
             (string->uc-string "eek -- what a geek.")
             (string->uc-string "--"))))
(check (= 15 (uc-string-contains-right
              (string->uc-string "eek -- what a geek.")
              (string->uc-string "ee"))))
(check (= 0 (uc-string-contains-right
             (string->uc-string "eek -- what a geek.")
             (string->uc-string "ee") 0 16)))
(check (= 19 (uc-string-contains-right
              (string->uc-string "eek -- what a geek.")
              (string->uc-string ""))))
(check (= 0 (uc-string-contains-right
             (string->uc-string "eek -- what a geek.")
             (string->uc-string "eek -- what a geek."))))
(check (= 4 (uc-string-contains-right
             (string->uc-string "eek -- what a geek.")
             (string->uc-string "eek -- what a geek.") 0 19 4 6)))
(check (= 17 (uc-string-contains-right
              (string->uc-string "eek -- what a geek.")
              (string->uc-string "k"))))
(check (= 2 (uc-string-contains-right
             (string->uc-string "eek -- what a geek.")
             (string->uc-string "k") 0 17)))
(check (= 1 (uc-string-contains-right
             (string->uc-string "eek -- what a geek.")
             (string->uc-string "ek") 0 17)))
(check (uc-string=? (string->uc-string "  325")
                    (uc-string-pad (string->uc-string "325") 5)))
(check (uc-string=? (string->uc-string "71325")
                    (uc-string-pad (string->uc-string "71325") 5)))
(check (uc-string=? (string->uc-string "71325")
                    (uc-string-pad (string->uc-string "8871325") 5)))
(check (uc-string=? (string->uc-string "...25")
                    (uc-string-pad (string->uc-string "8871325")
                                   5 #\. 5)))
(check (uc-string=? (string->uc-string "....2")
                    (uc-string-pad (string->uc-string "8871325")
                                   5 #\. 5 6)))
(check (uc-string=? (string->uc-string ".....")
                    (uc-string-pad (string->uc-string "8871325")
                                   5 #\. 5 5)))
(check (uc-string=? (string->uc-string "325  ")
                    (uc-string-pad-right
                     (string->uc-string "325") 5)))
(check (uc-string=? (string->uc-string "71325")
                    (uc-string-pad-right
                     (string->uc-string "71325") 5)))
(check (uc-string=? (string->uc-string "88713")
                    (uc-string-pad-right
                     (string->uc-string "8871325") 5)))
(check (uc-string=? (string->uc-string "25...")
                    (uc-string-pad-right
                     (string->uc-string "8871325")
                     5 #\. 5)))
(check (uc-string=? (string->uc-string "2....")
                    (uc-string-pad-right
                     (string->uc-string "8871325")
                     5 #\. 5 6)))
(check (uc-string=? (string->uc-string ".....")
                    (uc-string-pad-right
                     (string->uc-string "8871325")
                     5 #\. 5 5)))
(check (uc-string-every char-numeric?
                        (string->uc-string "")))
(check (uc-string-every char-numeric?
                        (string->uc-string "") 0))
(check (uc-string-every char-numeric?
                        (string->uc-string "") 0 0))
(check (equal? #t (uc-string-every char-numeric?
                                   (string->uc-string "1234"))))
(check (equal? #t (uc-string-every char-numeric?
                                   (string->uc-string "  1234") 2)))
(check (equal? #t (uc-string-every char-numeric?
                                   (string->uc-string "  1234  ") 2 6)))
(check (not (uc-string-every char-numeric?
                             (string->uc-string "12.4"))))
(check (not (uc-string-every char-numeric?
                             (string->uc-string "  12.4") 2)))
(check (not (uc-string-every char-numeric?
                             (string->uc-string "  12.4  ") 2 6)))
(check (equal? #\4 (uc-string-every (lambda (x) x)
                                    (string->uc-string "1234"))))
(check (not (uc-string-any (complement char-numeric?)
                           (string->uc-string ""))))
(check (not (uc-string-any (complement char-numeric?)
                           (string->uc-string "") 0)))
(check (not (uc-string-any (complement char-numeric?)
                           (string->uc-string "") 0 0)))
(check (not (uc-string-any (complement char-numeric?)
                           (string->uc-string "1234"))))
(check (not (uc-string-any (complement char-numeric?)
                           (string->uc-string "  1234") 2)))
(check (not (uc-string-any (complement char-numeric?)
                           (string->uc-string "  1234  ") 2 6)))
(check (equal? #t (uc-string-any (complement char-numeric?)
                                 (string->uc-string "12.4"))))
(check (equal? #t (uc-string-any (complement char-numeric?)
                                 (string->uc-string "  12.4") 2)))
(check (equal? #t (uc-string-any (complement char-numeric?)
                                 (string->uc-string "  12.4  ") 2 6)))
(check (equal? #\1 (uc-string-any (lambda (x) x)
                                  (string->uc-string "1234"))))
(check (uc-string-null? (uc-string-tabulate integer->char 0)))
(check (uc-string=? (uc-string #\null #\x1 #\x2 #\x3 #\x4)
                    (uc-string-tabulate integer->char 5)))
(let ((port->uc-string
       (lambda (port)
         (uc-string-unfold eof-object? values
                           (lambda (x) (read-char port))
                           (read-char port))))
      (f (open-input-string "fred flintstone")))
  (check (uc-string=? (string->uc-string "fred flintstone")
                      (port->uc-string f)))
  (close-input-port f))
(let ((port->uc-string
       (lambda (port)
         (uc-string-unfold eof-object? values
                           (lambda (x) (read-char port))
                           (read-char port)
                           #\:)))
      (f (open-input-string "fred flintstone")))
  (check (uc-string=? (string->uc-string ":fred flintstone")
                      (port->uc-string f)))
  (close-input-port f))
(let ((port->uc-string
       (lambda (port)
         (uc-string-unfold eof-object? values
                           (lambda (x) (read-char port))
                           (read-char port)
                           "wilma and "
                           (lambda (x)
                             (if (eof-object? x)
                               ": eof"
                               ": not eof")))))
      (f (open-input-string "fred flintstone")))
  (check (uc-string=? (string->uc-string
                       "wilma and fred flintstone: eof")
                      (port->uc-string f)))
  (close-input-port f))
(check (uc-string=?
        (string->uc-string " The English alphabet: \
                            abcdefghijklmnopqrstuvwxyz ")
        (uc-string-unfold-right
         (lambda (n) (= (+ n 1) (char->integer #\A)))
         (lambda (n) (char-downcase (integer->char n)))
         (lambda (n) (- n 1))
         (char->integer #\Z)
         #\space
         (lambda (n) " The English alphabet: "))))
(check (uc-string=?
        (string->uc-string "abcdefghijklmnopqrstuvwxyz ")
        (uc-string-unfold-right
         (lambda (n) (= (+ n 1) (char->integer #\A)))
         (lambda (n) (char-downcase (integer->char n)))
         (lambda (n) (- n 1))
         (char->integer #\Z)
         #\space)))
(check (uc-string=?
        (string->uc-string "abcdefghijklmnopqrstuvwxyz")
        (uc-string-unfold-right
         (lambda (n) (= (+ n 1) (char->integer #\A)))
         (lambda (n) (char-downcase (integer->char n)))
         (lambda (n) (- n 1))
         (char->integer #\Z))))
(check (uc-string=?
        (string->uc-string "0123456789")
        (uc-string-unfold-right
         (lambda (n) (zero? (+ n 1)))
         (lambda (n) (number->string n))
         (lambda (n) (- n 1))
         9)))
(check (uc-string=?
        (string->uc-string "00112233445566778899")
        (uc-string-unfold-right
         (lambda (n) (zero? (+ n 1)))
         (lambda (n) (uc-string-append
                      (number->uc-string n)
                      (number->uc-string n)))
         (lambda (n) (- n 1))
         9)))
(check (null? (uc-string-segment (make-uc-string 0 #\null) 1)))
(check (equal?
        '("f" "r" "e" "d" " " "f" "l" "i" "n" "t" "s" "t" "o" "n" "e")
        (map uc-string->string
             (uc-string-segment
              (string->uc-string "fred flintstone") 1))))
(check (equal?
        '("fr" "ed" " f" "li" "nt" "st" "on" "e")
        (map uc-string->string
             (uc-string-segment
              (string->uc-string "fred flintstone") 2))))
(check (equal?
        '("fre" "d f" "lin" "tst" "one")
        (map uc-string->string
             (uc-string-segment
              (string->uc-string "fred flintstone") 3))))
(check (uc-string=?
        (string->uc-string "cdefab")
        (uc-string-replicate (string->uc-string "abcdef") 2 8)))
(check (uc-string=?
        (string->uc-string "efabcd")
        (uc-string-replicate (string->uc-string "abcdef") -2 4)))
(check (uc-string=?
        (string->uc-string "abcabca")
        (uc-string-replicate (string->uc-string "abc") 0 7)))
(check (uc-string=?
        (string->uc-string "abcabca")
        (uc-string-replicate (string->uc-string "xxxabc") 0 7 3)))
(check (uc-string=?
        (string->uc-string "abcabca")
        (uc-string-replicate (string->uc-string "xxxabcxxx") 0 7 3 6)))
(check (uc-string=?
        (string->uc-string "")
        (uc-string-replicate (string->uc-string "") 0 0)))
(check (uc-string=?
        (string->uc-string "")
        (uc-string-replicate (string->uc-string "fred flintstone")
                             7 7 5 5)))
(check (null?
        (map uc-string->string
             (uc-string-split (string->uc-string "")
                              (string->uc-string "")))))
(check (null?
        (map uc-string->string
             (uc-string-split (string->uc-string "")
                              (string->uc-string "anything")))))
(check (guard (condition (#t #t))
         (begin
           (uc-string-split (string->uc-string "")
                            (string->uc-string "anything")
                            'strict-infix)
           #f)))
(check (equal?
        '("f" "r" "e" "d" " " "f" "l" "i" "n" "t" "s" "t" "o" "n" "e")
        (map uc-string->string
             (uc-string-split (string->uc-string "fred flintstone")
                              ""))))
(check (equal?
        '("f" "r" "e" "d" " " "flintstone")
        (map uc-string->string
             (uc-string-split (string->uc-string "fred flintstone")
                              "" 'infix 5))))
(check (equal?
        '("fr" "d flintston" "")
        (map uc-string->string
             (uc-string-split (string->uc-string "fred flintstone")
                              "e"))))
(check (equal?
        '("fr" "d flintston" "")
        (map uc-string->string
             (uc-string-split (string->uc-string "fred flintstone")
                              #\e 'infix))))
(check (equal?
        '("fr" "d flintston" "")
        (map uc-string->string
             (uc-string-split (string->uc-string "fred flintstone")
                              #\e 'strict-infix))))
(check (equal?
        '("fr" "d flintston" "")
        (map uc-string->string
             (uc-string-split (string->uc-string "fred flintstone")
                              #\e 'prefix))))
(check (equal?
        '("fr" "d flintston")
        (map uc-string->string
             (uc-string-split (string->uc-string "fred flintstone")
                              #\e 'suffix))))
(check (equal?
        '("" "red " "lintstone")
        (map uc-string->string
             (uc-string-split (string->uc-string "fred flintstone")
                              #\f))))
(check (equal?
        '("" "red " "lintstone")
        (map uc-string->string
             (uc-string-split (string->uc-string "fred flintstone")
                              #\f 'infix))))
(check (equal?
        '("" "red " "lintstone")
        (map uc-string->string
             (uc-string-split (string->uc-string "fred flintstone")
                              #\f 'strict-infix))))
(check (equal?
        '("red " "lintstone")
        (map uc-string->string
             (uc-string-split (string->uc-string "fred flintstone")
                              #\f 'prefix))))
(check (equal?
        '("" "red " "lintstone")
        (map uc-string->string
             (uc-string-split (string->uc-string "fred flintstone")
                              #\f 'suffix))))
(check (equal?
        '("wilma" "stone and fred" "stone")
        (map uc-string->string
             (uc-string-split (string->uc-string
                               "wilma flintstone and fred flintstone")
                              (string->uc-string " flint")))))
(check (equal?
        '("wilma" "stone and fred" "stone")
        (map uc-string->string
             (uc-string-split
              (string->uc-string
               "..wilma flintstone and fred flintstone")
              (string->uc-string " flint")
              'infix #f 2))))
(check (equal?
        '("wilma" "stone and")
        (map uc-string->string
             (uc-string-split
              (string->uc-string
               "..wilma flintstone and fred flintstone")
              (string->uc-string " flint")
              'infix #f 2 22))))
(check (equal?
        '("wilma" "stone and fred flintstone")
        (map uc-string->string
             (uc-string-split
              (string->uc-string
               "..wilma flintstone and fred flintstone")
              (string->uc-string " flint")
              'infix 1 2))))
(check (equal?
        '("pebbles" "stone and wilma" "stone and fred flintstone")
        (map uc-string->string
             (uc-string-split
              (string->uc-string
               "pebbles flintstone and wilma \
                flintstone and fred flintstone")
              (string->uc-string " flint")
              'infix 2))))
(let ((str (string->uc-string "")))
  (uc-string-fill! str #\.)
  (check (uc-string=? (string->uc-string "") str)))
(let ((str (string->uc-string "fred flintstone")))
  (uc-string-fill! str #\.)
  (check (uc-string=? (string->uc-string "...............") str)))
(let ((str (string->uc-string "fred flintstone")))
  (uc-string-fill! str #\. 5)
  (check (uc-string=? (string->uc-string "fred ..........") str)))
(let ((str (string->uc-string "fred flintstone")))
  (uc-string-fill! str #\. 1 4)
  (check (uc-string=? (string->uc-string "f... flintstone") str)))
(check (uc-string=? (string->uc-string "")
                    (vector->uc-string '#())))
(check (uc-string=? (string->uc-string "fred")
                    (vector->uc-string '#(#\f #\r #\e #\d))))
(check (uc-string=? (string->uc-string "fred")
                    (vector->uc-string '#(#\. #\. #\f #\r #\e #\d)
                                       2)))
(check (uc-string=? (string->uc-string "fre")
                    (vector->uc-string '#(#\. #\. #\f #\r #\e #\d)
                                       2 5)))
(check (uc-string=?
        (string->uc-string
         "The miserable perl programmer endured daily ridicule.")
        (uc-string-replace
         (string->uc-string
          "The TCL programmer endured daily ridicule.")
         (string->uc-string
          "another miserable perl drone") 4 7 8 22)))
(check (uc-string=?
        (string->uc-string "It’s lots of fun to code it up in Scheme.")
        (uc-string-replace
         (string->uc-string "It’s easy to code it up in Scheme.")
         (string->uc-string "lots of fun") 5 9)))
(check (uc-string=?
        (string->uc-string "It’s fun to code it up in Scheme.")
        (uc-string-replace
         (string->uc-string "It’s easy to code it up in Scheme.")
         (string->uc-string "lots of fun") 5 9 8)))
(check (uc-string=?
        (string->uc-string
         "It’s easy and rewarding to code it up in Scheme.")
        (uc-string-replace
         (string->uc-string "It’s easy to code it up in Scheme.")
         (string->uc-string " and rewarding") 9 9)))
(check (uc-string=?
        (string->uc-string "Hello, I must be going.")
        (uc-string-concatenate-reverse
         (list (string->uc-string " must be")
               (string->uc-string "Hello, I"))
         (string->uc-string " going.XXXX") 7)))
(check (uc-string=?
        (string->uc-string "Hello, I must be going.XXXX")
        (uc-string-concatenate-reverse
         (list (string->uc-string " must be")
               (string->uc-string "Hello, I"))
         (string->uc-string " going.XXXX"))))
(check (uc-string=?
        (string->uc-string "Hello, I must be going.")
        (uc-string-concatenate-reverse
         (list (string->uc-string " going.")
               (string->uc-string " must be")
               (string->uc-string "Hello, I")))))
(check (uc-string-null? (uc-string-concatenate-reverse '())))
(check (zero? (uc-string-count (make-uc-string 0) (lambda (ch) #t))))
(check (zero? (uc-string-count
               (string->uc-string "fred flintstone") char-numeric?)))
(check (= 1 (uc-string-count
             (string->uc-string "fred flintstone") char-whitespace?)))
(check (= 14 (uc-string-count
              (string->uc-string "fred flintstone") char-lower-case?)))
(check (= 10 (uc-string-count
              (string->uc-string "fred flintstone")
              char-lower-case? 4)))
(check (= 5 (uc-string-count
             (string->uc-string "fred flintstone")
             char-lower-case? 10)))
(check (uc-string=? (string->uc-string "red linsone")
                    (uc-string-filter
                     (lambda (ch) (and (not (char=? ch #\f))
                                       (not (char=? ch #\t))))
                     (string->uc-string "fred flintstone"))))
(check (uc-string=? (string->uc-string "flisoe")
                    (uc-string-filter
                     (lambda (ch) (and (not (char=? ch #\n))
                                       (not (char=? ch #\t))))
                     (string->uc-string "fred flintstone") 5)))
(check (uc-string=? (string->uc-string "fli")
                    (uc-string-filter
                     (lambda (ch) (and (not (char=? ch #\n))
                                       (not (char=? ch #\t))))
                     (string->uc-string "fred flintstone") 5 10)))
(check (uc-string=? (string->uc-string "")
                    (uc-string-filter
                     char-numeric?
                     (string->uc-string "fred flintstone"))))
(check (uc-string=? (string->uc-string "fftt")
                    (uc-string-remove
                     (lambda (ch) (and (not (char=? ch #\f))
                                       (not (char=? ch #\t))))
                     (string->uc-string "fred flintstone"))))
(check (uc-string=? (string->uc-string "ftt")
                    (uc-string-remove
                     (lambda (ch) (and (not (char=? ch #\f))
                                       (not (char=? ch #\t))))
                     (string->uc-string "fred flintstone") 5)))
(check (uc-string=? (string->uc-string "ft")
                    (uc-string-remove
                     (lambda (ch) (and (not (char=? ch #\f))
                                       (not (char=? ch #\t))))
                     (string->uc-string "fred flintstone") 5 10)))
(check (uc-string=? (string->uc-string "fred flintstone")
                    (uc-string-remove
                     char-numeric?
                     (string->uc-string "fred flintstone"))))
(check (uc-string=? (string->uc-string "abc")
                    (string->uc-string "abc")
                    (string->uc-string "abc")
                    (string->uc-string "abc")))
(check (not (uc-string=? (string->uc-string "abc")
                         (string->uc-string "abc")
                         (string->uc-string "abcd")
                         (string->uc-string "abc"))))
(check (uc-string-ci=? (string->uc-string "abc")
                       (string->uc-string "ABC")))
(check (uc-string-ci=? (string->uc-string "abc")
                       (string->uc-string "ABC")
                       (string->uc-string "AbC")))
(check (not (uc-string-ci=? (string->uc-string "abc")
                            (string->uc-string "ABC")
                            (string->uc-string "ABCd"))))
(check (uc-string-ci<=? (string->uc-string "abc")
                        (string->uc-string "ABC")
                        (string->uc-string "ABCd")))
(check (not (uc-string-ci<? (string->uc-string "abc")
                            (string->uc-string "ABC")
                            (string->uc-string "ABCd"))))
(check (uc-string-ci<? (string->uc-string "abc")
                       (string->uc-string "ABCd")))
(check (uc-string<=? (string->uc-string "abc")
                     (string->uc-string "abc")
                     (string->uc-string "abcd")))
(check (not (uc-string<? (string->uc-string "abc")
                         (string->uc-string "abc")
                         (string->uc-string "abcd"))))
(check (uc-string<? (string->uc-string "abc")
                    (string->uc-string "abcd")))
(check (uc-string-ci>=? (string->uc-string "abCd")
                        (string->uc-string "ABC")
                        (string->uc-string "aBC")))
(check (not (uc-string-ci>? (string->uc-string "abD")
                            (string->uc-string "ABC")
                            (string->uc-string "AbC"))))
(check (uc-string-ci>? (string->uc-string "abcD")
                       (string->uc-string "ABC")))
(check (uc-string>=? (string->uc-string "abcd")
                     (string->uc-string "abc")
                     (string->uc-string "abc")))
(check (not (uc-string>? (string->uc-string "abcd")
                         (string->uc-string "abc")
                         (string->uc-string "abc"))))
(check (uc-string>? (string->uc-string "abcd")
                    (string->uc-string "abc")))
(check (uc-string=? (string->uc-string "abcd")
                    (uc-string-downcase (string->uc-string "ABCD"))))
(check (uc-string=? (string->uc-string "ABCD")
                    (uc-string-upcase (string->uc-string "abcd"))))
(check (uc-string=? (string->uc-string "abss")
                    (uc-string-foldcase (string->uc-string "ABß"))))
(check (zero? (uc-string-prefix-length
               (string->uc-string "fred flintstone")
               (string->uc-string "wilma flintstone"))))
(check (= 5 (uc-string-prefix-length
             (string->uc-string "fred flintstone")
             (string->uc-string "fred FLINTSTONE"))))
(check (= 5 (uc-string-prefix-length
             (string->uc-string "xxfred flintstone")
             (string->uc-string "fred FLINTSTONE")
             2)))
(check (= 3 (uc-string-prefix-length
             (string->uc-string "xxfred flintstone")
             (string->uc-string "fred FLINTSTONE")
             2 5)))
(check (= 2 (uc-string-prefix-length
             (string->uc-string "xxfred flintstone")
             (string->uc-string "xxxxfred FLINTSTONE")
             2 5 4 6)))
(check (zero? (uc-string-prefix-length
               (string->uc-string "wilma flintstone")
               (string->uc-string "fred flintstone"))))
(check (= 5 (uc-string-prefix-length
             (string->uc-string "fred FLINTSTONE")
             (string->uc-string "fred flintstone"))))
(check (= 5 (uc-string-prefix-length
             (string->uc-string "fred FLINTSTONE")
             (string->uc-string "xxfred flintstone")
             0 15 2)))
(check (= 3 (uc-string-prefix-length
             (string->uc-string "fred FLINTSTONE")
             (string->uc-string "xxfred flintstone")
             0 15 2 5)))
(check (= 2 (uc-string-prefix-length
             (string->uc-string "xxxxfred FLINTSTONE")
             (string->uc-string "xxfred flintstone")
             4 6 2 5)))
(let ((str (string->uc-string "fred FLINTSTONE")))
  (check (= 15 (uc-string-prefix-length str str))))
(let ((str (string->uc-string "fred FLINTSTONE")))
  (check (= 10 (uc-string-prefix-length str str 5 15 5))))
(let ((str (string->uc-string "fred FLINTSTONE")))
  (check (= 10 (uc-string-prefix-length str str 5 15 5 15))))
(let ((str (string->uc-string "fred FLINTSTONE")))
  (check (zero? (uc-string-prefix-length str str 4 15 5 15))))
(let ((str (string->uc-string "fred flintstone")))
  (check (= 1 (uc-string-prefix-length str str 0 1 5 6))))
(check (= 11 (uc-string-suffix-length
              (string->uc-string "fred flintstone")
              (string->uc-string "wilma flintstone"))))
(check (zero? (uc-string-suffix-length
               (string->uc-string "fred flintstone")
               (string->uc-string "wilma FLINTSTONE"))))
(check (= 11 (uc-string-suffix-length
              (string->uc-string "fred flintstone")
              (string->uc-string "xxwilma flintstone")
              0 15 2)))
(check (= 11 (uc-string-suffix-length
              (string->uc-string "xxfred flintstone")
              (string->uc-string "wilma flintstone")
              2)))
(check (= 1 (uc-string-suffix-length
             (string->uc-string "xxfred flintstone")
             (string->uc-string "wilma flintstone")
             2 5)))
(check (= 10 (uc-string-suffix-length
              (string->uc-string "fred flintstone")
              (string->uc-string "xxwilma flintstone")
              0 14 2 17)))
(check (uc-string-prefix?
        (string->uc-string "fred")
        (string->uc-string "fred flintstone")))
(check (not (uc-string-prefix?
             (string->uc-string "fred flintstone")
             (string->uc-string "fred"))))
(check (uc-string-prefix?
        (string->uc-string "")
        (string->uc-string "fred flintstone")))
(check (not (uc-string-prefix?
             (string->uc-string "wilma")
             (string->uc-string "fred flintstone"))))
(check (uc-string-prefix?
        (string->uc-string "xxfred")
        (string->uc-string "fred flintstone")
        2))
(check (uc-string-prefix?
        (string->uc-string "xxfredxx")
        (string->uc-string "fred flintstone")
        2 6))
(check (uc-string-prefix?
        (string->uc-string "xxfredxx")
        (string->uc-string "xxxfred flintstone")
        2 6 3))
(check (not (uc-string-prefix?
             (string->uc-string "xxfredxx")
             (string->uc-string "xxxfrexxxd flintstone")
             2 6 3)))
(check (uc-string-prefix?
        (string->uc-string "xxfredxx")
        (string->uc-string "xxxfrexxxd flintstone")
        2 5 3))
(check (not (uc-string-prefix?
             (string->uc-string "xxfredxx")
             (string->uc-string "xxxfrexxxd flintstone")
             2 5 3 5)))
(check (uc-string-prefix?
        (string->uc-string "xxfredxx")
        (string->uc-string "xxxfrexxxd flintstone")
        2 4 3 5))
(check (uc-string-suffix?
        (string->uc-string "flintstone")
        (string->uc-string "fred flintstone")))
(check (not (uc-string-suffix?
             (string->uc-string "rubble")
             (string->uc-string "fred flintstone"))))
(check (uc-string-suffix?
        (string->uc-string "")
        (string->uc-string "fred flintstone")))
(check (uc-string-suffix?
        (string->uc-string "xxxflintstone")
        (string->uc-string "fred flintstone")
        3))
(check (uc-string-suffix?
        (string->uc-string "xxxflintstonexxx")
        (string->uc-string "fred flintstone")
        3 13))
(check (uc-string-suffix?
        (string->uc-string "xxxflintstonexxx")
        (string->uc-string ".....fred flintstone")
        3 13 5))
(check (uc-string-suffix?
        (string->uc-string "xxxflintstonexxx")
        (string->uc-string ".....fred flintstone.....")
        3 13 5 20))
(let ((test-file "unit-test-uc-string.test-file"))
  (when (file-exists? test-file)
    (delete-file test-file))
  (with-output-to-file test-file
    (lambda ()
      (write-uc-string (string->uc-string "fred flintstone\n"))))
  (with-input-from-file test-file
    (lambda ()
      (let ((str (read-uc-string 16)))
        (check (eof-object? (read-uc-string 1)))
        (check (uc-string=? str (string->uc-string
                                 "fred flintstone\n"))))))
  (when (file-exists? test-file)
    (delete-file test-file)))  

(format #t "pass: ~a~&" pass-count)
(format #t "~a: ~a~&"
        (if (zero? fail-count) "fail" "FAIL")
        fail-count)
(exit (zero? fail-count))
