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
;;;---------------------------------------------------------------------

(define-library (gafoundry-lib uc-string)

  (export uc-string?

          uc-string-null?
          uc-string-length

          make-uc-string

          uc-string

          list->uc-string
          uc-string->list

          string->uc-string
          uc-string->string

          symbol->uc-string
          uc-string->symbol

          number->uc-string
          uc-string->number

          uc-string-set!
          uc-string-ref

          uc-string-copy
          uc-string-copy!

          uc-string=?

          uc-string-index
          uc-string-index-right

          uc-string-map

          uc-string-fold
          uc-string-fold-right

          uc-string-append
          uc-string-concatenate
          )

  (import (scheme base)
          (scheme case-lambda)
          (gafoundry-lib define-record-factory))
  (cond-expand
    ((library (srfi 1))
     (import (srfi 1)))
    ((library (scheme list))
     (import (scheme list)))
    (else (import (srfi srfi-1))))
  (cond-expand
    ((library (scheme bytevector))
     (import (except (scheme bytevector)
                     bytevector-copy!)))
    ((library (rnrs bytevectors))
     (import (except (rnrs bytevectors)
                     bytevector-copy!)))
    (chicken (import (r6rs bytevectors))))

  (begin

    (cond-expand
      ((or little-endian x86_64 x86-64)
       (define-syntax endianness
         (syntax-rules ()
           ((endianness)
            'little))))
      (big-endian
       (define-syntax endianness
         (syntax-rules ()
           ((endianness)
            'big))))
      (else
       (define-syntax endianness
         (syntax-rules ()
           ((endianness)
            (native-endianness))))))

    (define (bytevec-set! bv k char)
      (bytevector-u32-set! bv (* k 4) (char->integer char)
                           (endianness)))

    (define (bytevec-ref bv k)
      (integer->char (bytevector-u32-ref bv (* k 4) (endianness))))

    (define (bytevec-cpcnt bv) ;; “Code point count”
      (truncate-quotient (bytevector-length bv) 4))

;;;---------------------------------------------------------------------

    (define-record-factory <uc-string>

      (predicate> uc-string?)

      (constructor>
       make-uc-string
       (lambda (construct)
         (case-lambda
           ((len)
            (construct (make-bytevector (* len 4) 0)))
           ((len char)
            (let* ((ichar (char->integer char))
                   (bv (make-bytevector (* len 4) 0)))
              (do ((i 0 (+ i 1)))
                  ((= i len))
                (bytevector-u32-set! bv (* i 4) ichar (endianness)))
              (construct bv))))))

      (constructor>
       list->uc-string
       (lambda (construct)
         (lambda (lst)
           (let* ((len (length lst))
                  (bv (make-bytevector (* len 4) 0)))
             (do ((p lst (cdr p))
                  (i 0 (+ i 1)))
                 ((not-pair? p))
               (bytevector-u32-set! bv (* i 4) (char->integer (car p))
                                    (endianness)))
             (construct bv)))))

      (constructor>
       string->uc-string
       (lambda (construct)
         (lambda (str)
           (construct (string->utf32 str (endianness))))))

      (constructor>
       uc-string-copy
       (lambda (construct)
         (case-lambda
           ((str)
            (construct (bytevector-copy (bytevec str))))
           ((str start)
            (construct (bytevector-copy (bytevec str) (* start 4))))
           ((str start end)
            (construct (bytevector-copy (bytevec str)
                                        (* start 4) (* end 4)))))))

      (constructor>
       uc-string-concatenate
       (lambda (construct)
         (lambda (str*)
           (let* ((bv* (map bytevec str*))
                  (total-bytes
                   (let loop ((p bv*)
                              (sum 0))
                     (if (not-pair? p)
                       sum
                       (loop (cdr p)
                             (+ sum (bytevector-length (car p)))))))
                  (out-bv (make-bytevector total-bytes 0)))
             (let loop ((p bv*)
                        (i 0))
               (if (not-pair? p)
                 (construct out-bv)
                 (let* ((bv (car p))
                        (len (bytevector-length bv)))
                   (bytevector-copy! out-bv i bv 0 len)
                   (loop (cdr p) (+ i len)))))))))

      (getter> 1 bytevec) )

;;;---------------------------------------------------------------------

    (define (uc-string-length str)
      (bytevec-cpcnt (bytevec str)))

    (define (uc-string-null? str)
      (zero? (uc-string-length str)))

    (define (uc-string . char*)
      (list->uc-string char*))

    (define (uc-string-append . str*)
      (uc-string-concatenate str*))
    
    (define uc-string-copy!
      (case-lambda
        ((to at from start end)
         (bytevector-copy! (bytevec to) (* at 4)
                           (bytevec from) (* start 4) (* end 4)))
        ((to at from start)
         (bytevector-copy! (bytevec to) (* at 4)
                           (bytevec from) (* start 4)))
        ((to at from)
         (bytevector-copy! (bytevec to) (* at 4) (bytevec from)))))

;;;---------------------------------------------------------------------

    (define (uc-string-set! str k char)
      (bytevec-set! (bytevec str) k char))

    (define (uc-string-ref str k)
      (bytevec-ref (bytevec str) k))

;;;---------------------------------------------------------------------

    (define (uc-string-map proc str)
      (let* ((in-bv (bytevec str))
             (bvlen (bytevector-length in-bv))
             (out-bv (make-bytevector bvlen 0))
             (len (truncate-quotient bvlen 4)))
        (do ((i 0 (+ i 1)))
            ((= i len))
          (bytevec-set!
           out-bv i (proc (bytevec-ref in-bv i))))
        out-bv))

    (define (uc-string-fold kons knil str)
      (let* ((bv (bytevec str))
             (len (bytevec-cpcnt bv))
             (accum knil))
        (do ((i 0 (+ i 1)))
            ((= i len))
          (set! accum (kons (bytevec-ref bv i) accum)))
        accum))

    (define (uc-string-fold-right kons knil str)
      (let* ((bv (bytevec str))
             (len (bytevec-cpcnt bv))
             (accum knil))
        (do ((i (- len 1) (- i 1)))
            ((= i -1))
          (set! accum (kons (bytevec-ref bv i) accum)))
        accum))

;;;---------------------------------------------------------------------

    (define (uc-string=? str1 str2)
      (let* ((bv1 (bytevec str1))
             (bv2 (bytevec str2))
             (len1 (bytevector-length bv1))
             (len2 (bytevector-length bv2)))
        (and (= len1 len2)
             (let loop ((i 0))
               (cond ((= i len1) #t)
                     ((= (bytevector-u32-ref bv1 i (endianness))
                         (bytevector-u32-ref bv2 i (endianness)))
                      (loop (+ i 4)))
                     (else #f))))))

;;;---------------------------------------------------------------------

    (define (uc-string-index str pred)
      (let* ((bv (bytevec str))
             (len (bytevec-cpcnt bv)))
        (let loop ((i 0))
          (cond ((= i len) #f)
                ((pred (bytevec-ref bv i)) i)
                (else (loop (+ i 1)))))))

    (define (uc-string-index-right str pred)
      (let* ((bv (bytevec str))
             (len (bytevec-cpcnt bv)))
        (let loop ((i (- len 1)))
          (cond ((= i -1) #f)
                ((pred (bytevec-ref bv i)) i)
                (else (loop (- i 1)))))))

;;;---------------------------------------------------------------------

    (define (uc-string->list str)
      (let* ((n (uc-string-length str))
             (lst (list #f)))
        (do ((i 0 (+ i 1))
             (p lst (cdr p)))
            ((= i n))
          (set-cdr! p (list (uc-string-ref str i))))
        (cdr lst)))

    (define (uc-string->string str)
      (utf32->string (bytevec str) (endianness)))

    (define (symbol->uc-string symbol)
      (string->uc-string (symbol->string symbol)))

    (define (uc-string->symbol str)
      (string->symbol (uc-string->string str)))

    (define number->uc-string
      (case-lambda
        ((z)
         (string->uc-string (number->string z)))
        ((z radix)
         (string->uc-string (number->string z radix)))))

    (define uc-string->number
      (case-lambda
        ((str)
         (string->number (uc-string->string str)))
        ((str radix)
         (string->number (uc-string->string str) radix))))

;;;---------------------------------------------------------------------

    ))
