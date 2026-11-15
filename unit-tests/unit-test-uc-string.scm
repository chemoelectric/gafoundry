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
        (scheme char)
        (scheme case-lambda)
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
(check (uc-string? (make-uc-string 3 #\null)))
(check (uc-string-null? (make-uc-string 0 #\null)))
(check (not (uc-string-null? (make-uc-string 3 #\null))))
(check (zero? (uc-string-length (make-uc-string 0 #\null))))
(check (= 3 (uc-string-length (make-uc-string 3 #\null))))
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
         (uc-string-join '() (string->uc-string ", ") 'strict-infix)))
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

(format #t "pass: ~a~&" pass-count)
(format #t "~a: ~a~&"
        (if (zero? fail-count) "fail" "FAIL")
        fail-count)
(exit (zero? fail-count))
