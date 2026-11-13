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

(define-library (gafoundry-lib utf32-string)

  (export utf32-string?

          utf32-string-null?
          utf32-string-length

          make-utf32-string

          utf32-string

          list->utf32-string
          utf32-string->list

          string->utf32-string
          utf32-string->string

          symbol->utf32-string
          utf32-string->symbol

          number->utf32-string
          utf32-string->number

          utf32-string-set!
          utf32-string-ref

          utf32-string-copy
          utf32-string-copy!

          utf32-string=?

          utf32-string-index
          utf32-string-index-right

          utf32-string-map

          utf32-string-fold
          utf32-string-fold-right

          utf32-string-append
          utf32-string-concatenate
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

    (define-syntax utf32-endianness
      (syntax-rules ()
        ((utf32-endianness)
         'little)))

    (define (bytevec-set! bv k char)
      (bytevector-u32-set! bv (* k 4) (char->integer char)
                           (utf32-endianness)))

    (define (bytevec-ref bv k)
      (integer->char (bytevector-u32-ref bv (* k 4)
                                         (utf32-endianness))))

    (define (bytevec-cpcnt bv) ;; “Code point count”
      (truncate-quotient (bytevector-length bv) 4))

;;;---------------------------------------------------------------------

    (define-record-factory <utf32-string>

      (predicate> utf32-string?)

      (constructor>
       make-utf32-string
       (lambda (construct)
         (case-lambda
           ((len)
            (construct (make-bytevector (* len 4) 0)))
           ((len char)
            (let* ((ichar (char->integer char))
                   (bv (make-bytevector (* len 4) 0))
                   (endianness (utf32-endianness)))
              (do ((i 0 (+ i 1)))
                  ((= i len))
                (bytevector-u32-set! bv (* i 4) ichar endianness))
              (construct bv))))))

       (constructor>
        list->utf32-string
        (lambda (construct)
          (lambda (lst)
            (let* ((len (length lst))
                   (bv (make-bytevector (* len 4) 0))
                   (endianness (utf32-endianness)))
              (do ((p lst (cdr p))
                   (i 0 (+ i 1)))
                  ((not-pair? p))
                (bytevector-u32-set! bv (* i 4) (char->integer (car p))
                                     endianness))
              (construct bv)))))

       (constructor>
        string->utf32-string
        (lambda (construct)
          (lambda (str)
            (construct (string->utf32 str (utf32-endianness))))))

       (constructor>
        utf32-string-copy
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
        utf32-string-concatenate
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

    (define (utf32-string-length str)
      (bytevec-cpcnt (bytevec str)))

    (define (utf32-string-null? str)
      (zero? (utf32-string-length str)))

    (define (utf32-string . char*)
      (list->utf32-string char*))

    (define (utf32-string-append . str*)
      (utf32-string-concatenate str*))
    
    (define utf32-string-copy!
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

    (define (utf32-string-set! str k char)
      (bytevec-set! (bytevec str) k char))

    (define (utf32-string-ref str k)
      (bytevec-ref (bytevec str) k))

;;;---------------------------------------------------------------------

    (define (utf32-string-map proc str)
      (let* ((in-bv (bytevec str))
             (bvlen (bytevector-length in-bv))
             (out-bv (make-bytevector bvlen 0))
             (len (truncate-quotient bvlen 4)))
        (do ((i 0 (+ i 1)))
            ((= i len))
          (bytevec-set!
           out-bv i (proc (bytevec-ref in-bv i))))
        out-bv))

    (define (utf32-string-fold kons knil str)
      (let* ((bv (bytevec str))
             (len (bytevec-cpcnt bv))
             (accum knil))
        (do ((i 0 (+ i 1)))
            ((= i len))
          (set! accum (kons (bytevec-ref bv i) accum)))
        accum))

    (define (utf32-string-fold-right kons knil str)
      (let* ((bv (bytevec str))
             (len (bytevec-cpcnt bv))
             (accum knil))
        (do ((i (- len 1) (- i 1)))
            ((= i -1))
          (set! accum (kons (bytevec-ref bv i) accum)))
        accum))

;;;---------------------------------------------------------------------

    (define (utf32-string=? str1 str2)
      (let* ((bv1 (bytevec str1))
             (bv2 (bytevec str2))
             (len1 (bytevector-length bv1))
             (len2 (bytevector-length bv2)))
        (and (= len1 len2)
             (let ((endian (utf32-endianness)))
               (let loop ((i 0))
                 (cond ((= i len1) #t)
                       ((= (bytevector-u32-ref bv1 i endian)
                           (bytevector-u32-ref bv2 i endian))
                        (loop (+ i 4)))
                       (else #f)))))))

;;;---------------------------------------------------------------------

    (define (utf32-string-index str pred)
      (let* ((bv (bytevec str))
             (len (bytevec-cpcnt bv)))
        (let loop ((i 0))
          (cond ((= i len) #f)
                ((pred (bytevec-ref bv i)) i)
                (else (loop (+ i 1)))))))

    (define (utf32-string-index-right str pred)
      (let* ((bv (bytevec str))
             (len (bytevec-cpcnt bv)))
        (let loop ((i (- len 1)))
          (cond ((= i -1) #f)
                ((pred (bytevec-ref bv i)) i)
                (else (loop (- i 1)))))))

;;;---------------------------------------------------------------------

    (define (utf32-string->list str)
      (let* ((n (utf32-string-length str))
             (lst (list #f)))
        (do ((i 0 (+ i 1))
             (p lst (cdr p)))
            ((= i n))
          (set-cdr! p (list (utf32-string-ref str i))))
        (cdr lst)))

    (define (utf32-string->string str)
      (utf32->string (bytevec str) (utf32-endianness)))

    (define (symbol->utf32-string symbol)
      (string->utf32-string (symbol->string symbol)))

    (define (utf32-string->symbol str)
      (string->symbol (utf32-string->string str)))

    (define number->utf32-string
      (case-lambda
        ((z)
         (string->utf32-string (number->string z)))
        ((z radix)
         (string->utf32-string (number->string z radix)))))

    (define utf32-string->number
      (case-lambda
        ((str)
         (string->number (utf32-string->string str)))
        ((str radix)
         (string->number (utf32-string->string str) radix))))

;;;---------------------------------------------------------------------

    ))
