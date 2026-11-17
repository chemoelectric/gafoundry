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

          uc-string-every
          uc-string-any

          make-uc-string
          uc-string-tabulate
          uc-string-unfold
          uc-string-unfold-right

          uc-string
          list->uc-string
          uc-string->list
          reverse-list->uc-string
          uc-string->reverse-list
          vector->uc-string
          uc-string->vector

          string->uc-string
          uc-string->string

          symbol->uc-string
          uc-string->symbol

          number->uc-string
          uc-string->number

          uc-string-set!
          uc-string-ref

          uc-substring
          uc-string-copy
          uc-string-copy!
          uc-string-fill!

          uc-string=?
          uc-string<?
          uc-string<=?
          uc-string>?
          uc-string>=?
          uc-string-ci=?
          uc-string-ci<?
          uc-string-ci<=?
          uc-string-ci>?
          uc-string-ci>=?

          uc-string-index
          uc-string-index-right
          uc-string-skip
          uc-string-skip-right

          uc-string-map
          uc-string-for-each
          uc-string-filter
          uc-string-remove
          uc-string-count

          uc-string-fold
          uc-string-fold-right

          uc-string-append
          uc-string-concatenate
          uc-string-concatenate-reverse
          uc-string-join

          uc-string-take-while
          uc-string-take-while-right
          uc-string-drop-while
          uc-string-drop-while-right
          uc-string-trim
          uc-string-trim-right
          uc-string-trim-both
          uc-string-span
          uc-string-span-right
          uc-string-break
          uc-string-break-right

          uc-string-contains
          uc-string-contains-right

          uc-string-pad
          uc-string-pad-right

          uc-string-segment
          uc-string-split
          uc-string-replicate

          uc-string-replace
          )

  (import (scheme base)
          (scheme case-lambda)
          (scheme char)
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

    (define (check-substring-bounds str start end)
      (unless (<= 0 start end (uc-string-length str))
        (error "expected 0 <= start <= end <= (uc-string-length string)"
               start end (uc-string-length str))))

    (define (check-vector-bounds vec start end)
      (unless (<= 0 start end (vector-length vec))
        (error "expected 0 <= start <= end <= (vector-length vector)"
               start end (vector-length vec))))

    (define (->uc-string obj)
      (cond ((uc-string? obj) obj)
            ((char? obj) (uc-string obj))
            ((string? obj) (string->uc-string obj))
            (else (error "expected uc-string, string, or char" obj))))

    (define (complement pred)
      (case-lambda
        ((arg)
         (not (pred arg)))
        (arg*
         (not (apply pred arg*)))))

;;;---------------------------------------------------------------------

    (define-record-factory <uc-string>

      (predicate> uc-string?)

      (constructor>
       make-uc-string
       (lambda (construct)
         (case-lambda
           ((len char)
            (let* ((ichar (char->integer char))
                   (len4 (* len 4))
                   (bv (make-bytevector len4)))
              (do ((i 0 (+ i 4)))
                  ((= i len4))
                (bytevector-u32-set! bv i ichar (endianness)))
              (construct bv)))
           ((len)
            (construct (make-bytevector (* len 4)))))))

      (constructor>
       uc-string-tabulate
       (lambda (construct)
         (lambda (proc len)
           (let* ((len4 (* len 4))
                  (bv (make-bytevector len4)))
             (do ((i 0 (+ i 1)))
                 ((= i len))
               (bytevector-u32-set! bv (* i 4)
                                    (char->integer (proc i))
                                    (endianness)))
             (construct bv)))))

      (constructor>
       list->uc-string
       (lambda (construct)
         (lambda (lst)
           (let* ((len (length lst))
                  (len4 (* len 4))
                  (bv (make-bytevector len4)))
             (do ((p lst (cdr p))
                  (i 0 (+ i 4)))
                 ((not-pair? p))
               (bytevector-u32-set! bv i (char->integer (car p))
                                    (endianness)))
             (construct bv)))))

      (constructor>
       reverse-list->uc-string
       (lambda (construct)
         (lambda (lst)
           (let* ((len (length lst))
                  (len4 (* len 4))
                  (bv (make-bytevector len4)))
             (do ((p lst (cdr p))
                  (i (- len4 4) (- i 4)))
                 ((not-pair? p))
               (bytevector-u32-set! bv i (char->integer (car p))
                                    (endianness)))
             (construct bv)))))

      (constructor>
       vector->uc-string
       (lambda (construct)
         (define (vec->str vec start end)
           (let* ((len (- end start))
                  (len4 (* len 4))
                  (bv (make-bytevector len4)))
             (do ((i 0 (+ i 1)))
                 ((= i len))
               (bytevector-u32-set! bv (* i 4)
                                    (char->integer
                                     (vector-ref vec (+ start i)))
                                    (endianness)))
             (construct bv)))
         (case-lambda
           ((vec)
            (let ((end (vector-length vec)))
              (vec->str vec 0 end)))
           ((vec start)
            (let ((end (vector-length vec)))
              (check-vector-bounds vec start end)
              (vec->str vec start end)))
           ((vec start end)
            (check-vector-bounds vec start end)
            (vec->str vec start end)))))

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
            (check-substring-bounds str start end)
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

      (constructor>
       uc-string-join
       (let ((space (make-uc-string 1 #\space))
             (empty-bv (make-bytevector 0)))
         (lambda (construct)
           (define (check-args str* delimiter grammar)
             (unless (or (eq? grammar 'infix)
                         (eq? grammar 'strict-infix)
                         (eq? grammar 'suffix)
                         (eq? grammar 'prefix))
               (error "not a legal uc-string-join grammar" grammar))
             (when (and (null-list? str*)
                        (eq? grammar 'strict-infix))
               (error "empty list not allowed \
                          with strict-infix grammar")))
           (define (strjoin str* delimiter grammar)
             (check-args str* delimiter grammar)
             (let* ((bv* (map bytevec str*))
                    (strcnt (length bv*))
                    (delim-bv (bytevec delimiter))
                    (delim-len (bytevector-length delim-bv)))
               (let-values
                   (((delim-count initial-len initial-delim
                                  final-len final-delim)
                     (case grammar
                       ((infix strict-infix)
                        (values (max 0 (- strcnt 1))
                                0 empty-bv
                                0 empty-bv))
                       ((suffix)
                        (values strcnt
                                0 empty-bv
                                delim-len delim-bv))
                       ((prefix)
                        (values strcnt
                                delim-len delim-bv
                                0 empty-bv)))))
                 (let* ((total-bytes
                         (let loop ((p bv*)
                                    (sum 0))
                           (if (not-pair? p)
                             (+ sum (* delim-count delim-len))
                             (loop (cdr p)
                                   (+ sum (bytevector-length
                                           (car p)))))))
                        (out-bv (make-bytevector total-bytes 0)))
                   (bytevector-copy! out-bv 0
                                     initial-delim 0 initial-len)
                   (let loop ((p bv*)
                              (i initial-len))
                     (if (not-pair? p)
                       (construct out-bv)
                       (let* ((bv (car p))
                              (len (bytevector-length bv))
                              (j i))
                         (bytevector-copy! out-bv j bv 0 len)
                         (set! j (+ i len))
                         (if (pair? (cdr p))
                           (begin
                             (bytevector-copy! out-bv j
                                               delim-bv 0 delim-len)
                             (set! j (+ j delim-len)))
                           (begin
                             (bytevector-copy! out-bv j
                                               final-delim 0 final-len)
                             (set! j (+ j final-len))))
                         (loop (cdr p) j))))))))
           (case-lambda
             ((str*)
              (strjoin str* space 'infix))
             ((str* delimiter)
              (strjoin str* delimiter 'infix))
             ((str* delimiter grammar)
              (strjoin str* delimiter grammar))))))

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

    (define (uc-substring str start end)
      (uc-string-copy str start end))
    
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

    (define uc-string-fill!
      (let ((fill-it! (lambda (str fill start end)
                        (let ((bv (bytevec str))
                              (ifill (char->integer fill))
                              (istart (* 4 start))
                              (iend (* 4 end)))
                          (do ((i istart (+ i 4)))
                              ((= i iend))
                            (bytevector-u32-set! bv i ifill
                                                 (endianness)))))))
        (case-lambda
          ((str fill)
           (let ((end (uc-string-length str)))
             (fill-it! str fill 0 end)))
          ((str fill start)
           (let ((end (uc-string-length str)))
             (check-substring-bounds str start end)
             (fill-it! str fill start end)))
          ((str fill start end)
           (check-substring-bounds str start end)
           (fill-it! str fill start end)))))

    (define uc-string-concatenate-reverse
      (let ((nullstr (make-uc-string 0)))
        (case-lambda
          ((str*)
           (uc-string-concatenate-reverse str* nullstr 0))
          ((str* final-str)
           (uc-string-concatenate-reverse
            str* final-str (uc-string-length final-str)))
          ((str* final-str end)
           (unless (<= 0 end (uc-string-length final-str))
             (error "uc-string-concatenate-reverse: \
                    end index out of bounds"
                    end final-str))
           (let ((total-len
                  (let loop ((lst str*)
                             (len end))
                    (if (null-list? lst)
                      len
                      (let ((item (car lst)))
                        (unless (uc-string? item)
                          (error "uc-string-concatenate-reverse: \
                                  element is not a uc-string"
                                 item))
                        (loop (cdr lst)
                              (+ len (uc-string-length item))))))))
             (let ((result (make-uc-string total-len))
                   (start-pos (- total-len end)))
               (uc-string-copy! result start-pos final-str 0 end)
               (let loop ((lst str*)
                          (pos start-pos))
                 (if (null? lst)
                   result
                   (let* ((str (car lst))
                          (len (uc-string-length str))
                          (next-pos (- pos len)))
                     (uc-string-copy! result next-pos str 0 len)
                     (loop (cdr lst) next-pos))))))))))

;;;---------------------------------------------------------------------

    (define (uc-string-set! str k char)
      (bytevec-set! (bytevec str) k char))

    (define (uc-string-ref str k)
      (bytevec-ref (bytevec str) k))

;;;---------------------------------------------------------------------

    (define uc-string-map
      (case-lambda
        ((proc str)
         (let* ((len (uc-string-length str))
                (lst (list '())))
           (do ((i 0 (+ i 1))
                (p lst (cdr p)))
               ((= i len))
             (let ((elem (->uc-string (proc (uc-string-ref str i)))))
               (set-cdr! p (list elem))))
           (uc-string-concatenate (cdr lst))))
        ((proc str . str*)
         (let* ((str* (cons str str*))
                (len (apply min (map uc-string-length str*)))
                (lst (list '())))
           (do ((i 0 (+ i 1))
                (p lst (cdr p)))
               ((= i len))
             (let ((elem (->uc-string
                          (apply proc (map (lambda (s)
                                             (uc-string-ref s i))
                                           str*)))))
               (set-cdr! p (list elem))))
           (uc-string-concatenate (cdr lst))))))

    (define uc-string-for-each
      (case-lambda
        ((proc str)
         (let ((len (uc-string-length str)))
           (do ((i 0 (+ i 1)))
               ((= i len))
             (proc (uc-string-ref str i)))))
        ((proc str . str*)
         (let* ((str* (cons str str*))
                (len (apply min (map uc-string-length str*))))
           (do ((i 0 (+ i 1)))
               ((= i len))
             (apply proc (map (lambda (s) (uc-string-ref s i))
                              str*)))))))

    (define uc-string-filter
      (case-lambda
        ((pred str)
         (uc-string-filter pred str 0 (uc-string-length str)))
        ((pred str start)
         (uc-string-filter pred str start (uc-string-length str)))
        ((pred str start end)
         (check-substring-bounds str start end)
         (let ((vec (uc-string->vector str start end))
               (substr-len (- end start))
               (count 0))
           (do ((i 0 (+ i 1)))
               ((= i substr-len))
             (if (pred (vector-ref vec i))
               (set! count (+ count 1))
               (vector-set! vec i #f)))
           (let ((result (make-uc-string count)))
             (let loop ((i 0)
                        (j 0))
               (if (= i substr-len)
                 result
                 (let ((ch (vector-ref vec i)))
                   (if ch
                     (begin
                       (uc-string-set! result j ch)
                       (loop (+ i 1) (+ j 1)))
                     (loop (+ i 1) j))))))))))

    (define uc-string-remove
      (case-lambda
        ((pred str)
         (uc-string-filter (complement pred)
                           str 0 (uc-string-length str)))
        ((pred str start)
         (uc-string-filter (complement pred)
                           str start (uc-string-length str)))
        ((pred str start end)
         (uc-string-filter (complement pred) str start end))))

    (define uc-string-count
      (case-lambda
        ((str pred start end)
         (check-substring-bounds str start end)
         (let ((count 0))
           (do ((i start (+ i 1)))
               ((= i end))
             (when (pred (uc-string-ref str i))
               (set! count (+ count 1))))
           count))
        ((str pred start)
         (uc-string-count str pred start (uc-string-length str)))
        ((str pred)
         (uc-string-count str pred 0 (uc-string-length str)))))

    (define uc-string-unfold
      (let* ((nullstr (make-uc-string 0))
             (default-make-final (lambda (x) nullstr)))
        (define (strunfold stop? mapper successor seed
                           base make-final)
          (let ((lst (list '())))
            (let loop ((seed seed)
                       (p lst))
              (if (stop? seed)
                (let ((base (->uc-string base))
                      (final (->uc-string (make-final seed))))
                  (uc-string-concatenate `(,base ,@(cdr lst) ,final)))
                (let ((str (->uc-string (mapper seed))))
                  (set-cdr! p (list str))
                  (loop (successor seed) (cdr p)))))))
        (case-lambda
          ((stop? mapper successor seed base make-final)
           (strunfold stop? mapper successor seed
                      base make-final))
          ((stop? mapper successor seed base)
           (strunfold stop? mapper successor seed
                      base default-make-final))
          ((stop? mapper successor seed)
           (strunfold stop? mapper successor seed
                      nullstr default-make-final)))))

    (define uc-string-unfold-right
      (let* ((nullstr (make-uc-string 0))
             (default-make-final (lambda (x) nullstr)))
        (define (strunfold-right stop? mapper successor seed
                                 base make-final)
          (let loop ((seed seed)
                     (lst '()))
            (if (stop? seed)
              (let ((base (->uc-string base))
                    (final (->uc-string (make-final seed))))
                (uc-string-concatenate `(,final ,@lst ,base)))
              (let ((str (->uc-string (mapper seed))))
                (loop (successor seed) (cons str lst))))))
        (case-lambda
          ((stop? mapper successor seed base make-final)
           (strunfold-right stop? mapper successor seed
                            base make-final))
          ((stop? mapper successor seed base)
           (strunfold-right stop? mapper successor seed
                            base default-make-final))
          ((stop? mapper successor seed)
           (strunfold-right stop? mapper successor seed
                            nullstr default-make-final)))))

;;;---------------------------------------------------------------------

    (define uc-string-fold
      (case-lambda
        ((kons knil str start end)
         (check-substring-bounds str start end)
         (let* ((bv (bytevec str))
                (accum knil))
           (do ((i start (+ i 1)))
               ((= i end))
             (set! accum (kons (bytevec-ref bv i) accum)))
           accum))
        ((kons knil str start)
         (uc-string-fold kons knil str start (uc-string-length str)))
        ((kons knil str)
         (uc-string-fold kons knil str 0 (uc-string-length str)))))

    (define uc-string-fold-right
      (case-lambda
        ((kons knil str start end)
         (check-substring-bounds str start end)
         (let* ((bv (bytevec str))
                (accum knil)
                (finish (- start 1)))
           (do ((i (- end 1) (- i 1)))
               ((= i finish))
             (set! accum (kons (bytevec-ref bv i) accum)))
           accum))
        ((kons knil str start)
         (uc-string-fold-right kons knil str start
                               (uc-string-length str)))
        ((kons knil str)
         (uc-string-fold-right kons knil str 0
                               (uc-string-length str)))))

;;;---------------------------------------------------------------------

    (define uc-string=?
      (case-lambda
        ((str1 str2) 
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
        ((str1 str2 . str*)
         (let ((len (uc-string-length str1))
               (str* (cons str2 str*)))
           (and (every (lambda (s) (= len (uc-string-length s))) str*)
                (every (lambda (s) (uc-string=? str1 s)) str*))))))

    (define-syntax uc-str-cmp?
      (syntax-rules ()
        ((uc-str-cmp? str-cmp?)
         (case-lambda
           ((str1 str2)
            (str-cmp? (uc-string->string str1)
                      (uc-string->string str2)))
           ((str1 str2 . str*)
            (apply str-cmp? `(,(uc-string->string str1)
                              ,(uc-string->string str2)
                              . ,(map uc-string->string str*))))))))

    (define uc-string<? (uc-str-cmp? string<?))
    (define uc-string<=? (uc-str-cmp? string<=?))
    (define uc-string>? (uc-str-cmp? string>?))
    (define uc-string>=? (uc-str-cmp? string>=?))
    (define uc-string-ci=? (uc-str-cmp? string-ci=?))
    (define uc-string-ci<? (uc-str-cmp? string-ci<?))
    (define uc-string-ci<=? (uc-str-cmp? string-ci<=?))
    (define uc-string-ci>? (uc-str-cmp? string-ci>?))
    (define uc-string-ci>=? (uc-str-cmp? string-ci>=?))

;;;---------------------------------------------------------------------

    (define uc-string-index
      (case-lambda
        ((str pred start end)
         (check-substring-bounds str start end)
         (let ((bv (bytevec str)))
           (let loop ((i start))
             (cond ((= i end) #f)
                   ((pred (bytevec-ref bv i)) i)
                   (else (loop (+ i 1)))))))
        ((str pred start)
         (uc-string-index str pred start (uc-string-length str)))
        ((str pred)
         (uc-string-index str pred 0 (uc-string-length str)))))

    (define uc-string-index-right
      (case-lambda 
        ((str pred start end)
         (check-substring-bounds str start end)
         (let ((bv (bytevec str))
               (finish (- start 1)))
           (let loop ((i (- end 1)))
             (cond ((= i finish) #f)
                   ((pred (bytevec-ref bv i)) i)
                   (else (loop (- i 1)))))))
        ((str pred start)
         (uc-string-index-right str pred start (uc-string-length str)))
        ((str pred)
         (uc-string-index-right str pred 0 (uc-string-length str)))))

    (define uc-string-skip
      (case-lambda
        ((str pred start end)
         (check-substring-bounds str start end)
         (let ((bv (bytevec str)))
           (let loop ((i start))
             (cond ((= i end) #f)
                   ((pred (bytevec-ref bv i)) (loop (+ i 1)))
                   (else i)))))
        ((str pred start)
         (uc-string-skip str pred start (uc-string-length str)))
        ((str pred)
         (uc-string-skip str pred 0 (uc-string-length str)))))

    (define uc-string-skip-right
      (case-lambda 
        ((str pred start end)
         (check-substring-bounds str start end)
         (let ((bv (bytevec str))
               (finish (- start 1)))
           (let loop ((i (- end 1)))
             (cond ((= i finish) #f)
                   ((pred (bytevec-ref bv i)) (loop (- i 1)))
                   (else i)))))
        ((str pred start)
         (uc-string-skip-right str pred start (uc-string-length str)))
        ((str pred)
         (uc-string-skip-right str pred 0 (uc-string-length str)))))

;;;---------------------------------------------------------------------

    (define uc-string->list
      (let ((str->list
             (lambda (str start end)
               (let ((lst (list '())))
                 (do ((i start (+ i 1))
                      (p lst (cdr p)))
                     ((= i end))
                   (set-cdr! p (list (uc-string-ref str i))))
                 (cdr lst)))))
        (case-lambda
          ((str)
           (let ((end (uc-string-length str)))
             (str->list str 0 end)))
          ((str start)
           (let ((end (uc-string-length str)))
             (check-substring-bounds str start end)
             (str->list str start end)))
          ((str start end)
           (check-substring-bounds str start end)
           (str->list str start end)))))

    (define (uc-string->reverse-list str)
      (let* ((n (uc-string-length str))
             (lst '()))
        (do ((i 0 (+ i 1)))
            ((= i n))
          (set! lst (cons (uc-string-ref str i) lst)))
        lst))

    (define uc-string->vector
      (let ((str->vec
             (lambda (str start end)
               (let ((vec (make-vector (- end start))))
                 (do ((i start (+ i 1)))
                     ((= i end))
                   (vector-set! vec (- i start)
                                (uc-string-ref str i)))
                 vec))))
        (case-lambda
          ((str)
           (let ((end (uc-string-length str)))
             (str->vec str 0 end)))
          ((str start)
           (let ((end (uc-string-length str)))
             (check-substring-bounds str start end)
             (str->vec str start end)))
          ((str start end)
           (check-substring-bounds str start end)
           (str->vec str start end)))))

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

    (define (uc-string-span-break-etc search returns str pred start end)
      (check-substring-bounds str start end)
      (let ((i (search str pred start end)))
        (case returns
          ((both)
           (values (uc-string-copy str start i)
                   (uc-string-copy str i end)))
          ((first)
           (uc-string-copy str start i))
          ((second)
           (uc-string-copy str i end)))))

    (define uc-string-span
      (let ((search (lambda (str pred start end)
                      (let ((i (uc-string-skip
                                str pred start end)))
                        (if i i end)))))
        (case-lambda
          ((str pred start end)
           (uc-string-span-break-etc search 'both
                                     str pred start end))
          ((str pred start)
           (uc-string-span-break-etc search 'both
                                     str pred start
                                     (uc-string-length str)))
          ((str pred)
           (uc-string-span-break-etc search 'both
                                     str pred 0
                                     (uc-string-length str))))))

    (define uc-string-span-right
      (let ((search (lambda (str pred start end)
                      (let ((i (uc-string-skip-right
                                str pred start end)))
                        (if i (+ i 1) start)))))
        (case-lambda
          ((str pred start end)
           (uc-string-span-break-etc search 'both
                                     str pred start end))
          ((str pred start)
           (uc-string-span-break-etc search 'both
                                     str pred start
                                     (uc-string-length str)))
          ((str pred)
           (uc-string-span-break-etc search 'both
                                     str pred 0
                                     (uc-string-length str))))))

    (define uc-string-break
      (let ((search (lambda (str pred start end)
                      (let ((i (uc-string-index
                                str pred start end)))
                        (if i i end)))))
        (case-lambda
          ((str pred start end)
           (uc-string-span-break-etc search 'both
                                     str pred start end))
          ((str pred start)
           (uc-string-span-break-etc search 'both
                                     str pred start
                                     (uc-string-length str)))
          ((str pred)
           (uc-string-span-break-etc search 'both
                                     str pred 0
                                     (uc-string-length str))))))

    (define uc-string-break-right
      (let ((search (lambda (str pred start end)
                      (let ((i (uc-string-index-right
                                str pred start end)))
                        (if i (+ i 1) start)))))
        (case-lambda
          ((str pred start end)
           (uc-string-span-break-etc search 'both
                                     str pred start end))
          ((str pred start)
           (uc-string-span-break-etc search 'both
                                     str pred start
                                     (uc-string-length str)))
          ((str pred)
           (uc-string-span-break-etc search 'both
                                     str pred 0
                                     (uc-string-length str))))))

    (define uc-string-take-while
      (let ((search (lambda (str pred start end)
                      (let ((i (uc-string-skip
                                str pred start end)))
                        (if i i end)))))
        (case-lambda
          ((str pred start end)
           (uc-string-span-break-etc search 'first
                                     str pred start end))
          ((str pred start)
           (uc-string-span-break-etc search 'first
                                     str pred start
                                     (uc-string-length str)))
          ((str pred)
           (uc-string-span-break-etc search 'first
                                     str pred 0
                                     (uc-string-length str))))))

    (define uc-string-take-while-right
      (let ((search (lambda (str pred start end)
                      (let ((i (uc-string-skip-right
                                str pred start end)))
                        (if i (+ i 1) start)))))
        (case-lambda
          ((str pred start end)
           (uc-string-span-break-etc search 'second
                                     str pred start end))
          ((str pred start)
           (uc-string-span-break-etc search 'second
                                     str pred start
                                     (uc-string-length str)))
          ((str pred)
           (uc-string-span-break-etc search 'second
                                     str pred 0
                                     (uc-string-length str))))))

    (define uc-string-drop-while
      (let ((search (lambda (str pred start end)
                      (let ((i (uc-string-skip
                                str pred start end)))
                        (if i i end)))))
        (case-lambda
          ((str pred start end)
           (uc-string-span-break-etc search 'second
                                     str pred start end))
          ((str pred start)
           (uc-string-span-break-etc search 'second
                                     str pred start
                                     (uc-string-length str)))
          ((str pred)
           (uc-string-span-break-etc search 'second
                                     str pred 0
                                     (uc-string-length str))))))

    (define uc-string-drop-while-right
      (let ((search (lambda (str pred start end)
                      (let ((i (uc-string-skip-right
                                str pred start end)))
                        (if i (+ i 1) start)))))
        (case-lambda
          ((str pred start end)
           (uc-string-span-break-etc search 'first
                                     str pred start end))
          ((str pred start)
           (uc-string-span-break-etc search 'first
                                     str pred start
                                     (uc-string-length str)))
          ((str pred)
           (uc-string-span-break-etc search 'first
                                     str pred 0
                                     (uc-string-length str))))))

    (define uc-string-trim
      (case-lambda
        ((str pred start end)
         (uc-string-drop-while str pred start end))
        ((str pred start)
         (uc-string-drop-while str pred start))
        ((str pred)
         (uc-string-drop-while str pred))
        ((str)
         (uc-string-drop-while str char-whitespace?))))

    (define uc-string-trim-right
      (case-lambda
        ((str pred start end)
         (uc-string-drop-while-right str pred start end))
        ((str pred start)
         (uc-string-drop-while-right str pred start))
        ((str pred)
         (uc-string-drop-while-right str pred))
        ((str)
         (uc-string-drop-while-right str char-whitespace?))))

    (define uc-string-trim-both
      (case-lambda
        ((str pred start end)
         (uc-string-trim-right
          (uc-string-trim str pred start end)
          pred))
        ((str pred start)
         (uc-string-trim-both str pred start (uc-string-length str)))
        ((str pred)
         (uc-string-trim-both str pred 0 (uc-string-length str)))
        ((str)
         (uc-string-trim-both str char-whitespace?
                              0 (uc-string-length str)))))

;;;---------------------------------------------------------------------

    (define (make-knuth-morris-pratt-table pattern)
      (let* ((m (uc-string-length pattern))
             (longest-proper-prefix-suffix (make-vector m 0)))
        (if (zero? m)
          longest-proper-prefix-suffix
          (let loop ((i 1)
                     (len 0))
            (cond
              ((= i m) longest-proper-prefix-suffix)
              ((char=? (uc-string-ref pattern i)
                       (uc-string-ref pattern len))
               (vector-set! longest-proper-prefix-suffix i (+ len 1))
               (loop (+ i 1) (+ len 1)))
              ((zero? len)
               (vector-set! longest-proper-prefix-suffix i 0)
               (loop (+ i 1) 0))
              (else
               (loop i (vector-ref longest-proper-prefix-suffix
                                   (- len 1)))))))))

    (define (knuth-morris-pratt-search text pattern
                                       text-start text-end
                                       pattern-start pattern-end)
      (check-substring-bounds text text-start text-end)
      (check-substring-bounds pattern pattern-start pattern-end)
      (let* ((sub-pattern
              (if (and (zero? pattern-start)
                       (= pattern-end (uc-string-length pattern)))
                pattern
                (uc-string-copy pattern pattern-start pattern-end)))
             (m (uc-string-length sub-pattern)))
        (cond
          ((zero? m) text-start)
          ((< (- text-end text-start) m) #f)
          (else
           (let ((longest-proper-prefix-suffix
                  (make-knuth-morris-pratt-table sub-pattern)))
             (let loop ((i text-start)
                        (j 0))
               (cond
                 ((= j m) (- i m))
                 ((>= i text-end) #f)
                 ((char=? (uc-string-ref text i)
                          (uc-string-ref sub-pattern j))
                  (loop (+ i 1) (+ j 1)))
                 ((zero? j)
                  (loop (+ i 1) 0))
                 (else
                  (loop i (vector-ref longest-proper-prefix-suffix
                                      (- j 1)))))))))))

    (define uc-string-contains
      (case-lambda
        ((text pattern)
         (knuth-morris-pratt-search
          text pattern
          0 (uc-string-length text)
          0 (uc-string-length pattern)))
        ((text pattern text-start)
         (knuth-morris-pratt-search
          text pattern
          text-start (uc-string-length text)
          0 (uc-string-length pattern)))
        ((text pattern text-start text-end)
         (knuth-morris-pratt-search
          text pattern
          text-start text-end
          0 (uc-string-length pattern)))
        ((text pattern text-start text-end pattern-start)
         (knuth-morris-pratt-search
          text pattern
          text-start text-end
          pattern-start (uc-string-length pattern)))
        ((text pattern text-start text-end pattern-start pattern-end)
         (knuth-morris-pratt-search
          text pattern
          text-start text-end
          pattern-start pattern-end))))

;;;---------------------------------------------------------------------

    (define (make-reverse-knuth-morris-pratt-table pattern)
      (let* ((m (uc-string-length pattern))
             (longest-proper-prefix-suffix (make-vector m 0)))
        (if (zero? m)
          longest-proper-prefix-suffix
          (let loop ((i (- m 2))
                     (len 0))
            (cond
              ((= i -1) longest-proper-prefix-suffix)
              ((char=? (uc-string-ref pattern i)
                       (uc-string-ref pattern (- m 1 len)))
               (vector-set! longest-proper-prefix-suffix i (+ len 1))
               (loop (- i 1) (+ len 1)))
              ((zero? len)
               (vector-set! longest-proper-prefix-suffix i 0)
               (loop (- i 1) 0))
              (else
               (loop i (vector-ref longest-proper-prefix-suffix
                                   (- m len)))))))))

    (define (knuth-morris-pratt-search-right text pattern
                                             text-start text-end
                                             pattern-start pattern-end)
      (check-substring-bounds text text-start text-end)
      (check-substring-bounds pattern pattern-start pattern-end)
      (let* ((sub-pattern
              (if (and (zero? pattern-start)
                       (= pattern-end (uc-string-length pattern)))
                pattern
                (uc-string-copy pattern pattern-start pattern-end)))
             (m (uc-string-length sub-pattern)))
        (cond
          ((zero? m) text-end)
          ((< (- text-end text-start) m) #f)
          (else
           (let ((longest-proper-prefix-suffix
                  (make-reverse-knuth-morris-pratt-table sub-pattern)))
             (let loop ((i (- text-end 1))
                        (j 0))
               (cond
                 ((= j m) (+ i 1))
                 ((< i text-start) #f)
                 ((char=? (uc-string-ref text i)
                          (uc-string-ref sub-pattern (- m 1 j)))
                  (loop (- i 1) (+ j 1)))
                 ((positive? j)
                  (loop i (vector-ref longest-proper-prefix-suffix
                                      (- m j))))
                 (else
                  (loop (- i 1) 0)))))))))

    (define uc-string-contains-right
      (case-lambda
        ((text pattern)
         (knuth-morris-pratt-search-right
          text pattern
          0 (uc-string-length text)
          0 (uc-string-length pattern)))
        ((text pattern text-start)
         (knuth-morris-pratt-search-right
          text pattern
          text-start (uc-string-length text)
          0 (uc-string-length pattern)))
        ((text pattern text-start text-end)
         (knuth-morris-pratt-search-right
          text pattern
          text-start text-end
          0 (uc-string-length pattern)))
        ((text pattern text-start text-end pattern-start)
         (knuth-morris-pratt-search-right
          text pattern
          text-start text-end
          pattern-start (uc-string-length pattern)))
        ((text pattern text-start text-end pattern-start pattern-end)
         (knuth-morris-pratt-search-right
          text pattern
          text-start text-end
          pattern-start pattern-end))))

;;;---------------------------------------------------------------------

    (define uc-string-pad
      (case-lambda
        ((str len char start end)
         (check-substring-bounds str start end)
         (let ((result (make-uc-string len char))
               (ncopy (min (- end start) len)))
           (uc-string-copy! result (- len ncopy) str (- end ncopy) end)
           result))
        ((str len char start)
         (uc-string-pad str len char
                        start (uc-string-length str)))
        ((str len char)
         (uc-string-pad str len char
                        0 (uc-string-length str)))
        ((str len)
         (uc-string-pad str len #\space
                        0 (uc-string-length str)))))

    (define uc-string-pad-right
      (case-lambda
        ((str len char start end)
         (check-substring-bounds str start end)
         (let ((result (make-uc-string len char))
               (ncopy (min (- end start) len)))
           (uc-string-copy! result 0 str start (+ start ncopy))
           result))
        ((str len char start)
         (uc-string-pad-right str len char
                              start (uc-string-length str)))
        ((str len char)
         (uc-string-pad-right str len char
                              0 (uc-string-length str)))
        ((str len)
         (uc-string-pad-right str len #\space
                              0 (uc-string-length str)))))

;;;---------------------------------------------------------------------

    (define uc-string-every
      (case-lambda
        ((pred str start end)
         (check-substring-bounds str start end)
         (if (= start end)
           #t
           (let loop ((i start)
                      (result #f))
             (cond ((= i end) result)
                   ((pred (uc-string-ref str i)) =>
                    (lambda (obj) (loop (+ i 1) obj)))
                   (else #f)))))
        ((pred str start)
         (uc-string-every pred str start (uc-string-length str)))
        ((pred str)
         (uc-string-every pred str 0 (uc-string-length str)))))

    (define uc-string-any
      (case-lambda
        ((pred str start end)
         (check-substring-bounds str start end)
         (let loop ((i start))
           (cond ((= i end) #f)
                 ((pred (uc-string-ref str i)) => values)
                 (else (loop (+ i 1))))))
        ((pred str start)
         (uc-string-any pred str start (uc-string-length str)))
        ((pred str)
         (uc-string-any pred str 0 (uc-string-length str)))))

;;;---------------------------------------------------------------------

    (define (uc-string-segment str k)
      (let ((len (uc-string-length str)))
        (if (zero? len)
          '()
          (begin
            (unless (positive? k)
              (error "expected a positive uc-string segment length" k))
            (let ((lst (list '())))
              (do ((i 0 (+ i k))
                   (p lst (cdr p)))
                  ((<= len i))
                (set-cdr! p (list (uc-string-copy
                                   str i (min len (+ i k))))))
              (cdr lst))))))

    (define uc-string-replicate
      (let ((nullstr (make-uc-string 0)))
        (case-lambda
          ((str from to start end)
           (check-substring-bounds str start end)
           (let ((len (uc-string-length str)))
             (unless (<= from to)
               (error "invalid replication bounds" from to))
             (let ((sub-len (- end start))
                   (out-len (- to from)))
               (cond
                 ((zero? out-len) nullstr)
                 ((zero? sub-len)
                  (error "cannot replicate an empty substring" from to))
                 (else
                  (let* ((result (make-uc-string out-len))
                         (offset (floor-remainder from sub-len))
                         (first-len (min out-len (- sub-len offset)))
                         (copied 0))

                    ;; Copy the tail of the first period (offset to
                    ;; end).
                    (uc-string-copy! result 0 str (+ start offset)
                                     (+ start offset first-len))
                    (set! copied (+ copied first-len))

                    (when (< first-len out-len)
                      ;; Copy the head of the first period (start to
                      ;; offset).
                      (let ((second-len
                             (min (- out-len first-len) offset)))
                        (uc-string-copy! result first-len str
                                         start (+ start second-len))
                        (set! copied (+ copied second-len))))

                    ;; Use geometric doubling to fill the rest: copy
                    ;; the existing contents, then copy the new longer
                    ;; contents, then again, etc. (This is not really
                    ;; exponential-time processing unless the copies
                    ;; are done in parallel. If the copies remain
                    ;; serial, the time complexity remains linear, but
                    ;; actual execution may be faster.)
                    (let loop ((copied copied))
                      (if (= copied out-len)
                        result
                        (let ((chunk (min copied (- out-len copied))))
                          (uc-string-copy! result copied result 0 chunk)
                          (loop (+ copied chunk)))))))))))

          ((str from to start)
           (uc-string-replicate str from to
                                start (uc-string-length str)))

          ((str from to)
           (uc-string-replicate str from to
                                0 (uc-string-length str))))))

    (define uc-string-split
      (case-lambda
        ((str delim)
         (uc-string-split str delim 'infix
                          #f 0 (uc-string-length str)))
        ((str delim grammar)
         (uc-string-split str delim grammar
                          #f 0 (uc-string-length str)))
        ((str delim grammar limit)
         (uc-string-split str delim grammar
                          limit 0 (uc-string-length str)))
        ((str delim grammar limit start)
         (uc-string-split str delim grammar
                          limit start (uc-string-length str)))
        ((str delim grammar limit start end)

         (unless (memq grammar '(infix strict-infix prefix suffix))
           (error "uc-string-split: invalid grammar" grammar))
         (unless (or (not limit)
                     (and (exact-integer? limit)
                          (not (negative? limit))))
           (error "uc-string-split: limit must be #f or \
                   a non-negative exact integer" limit))

         (check-substring-bounds str start end)

         ;;
         ;; The following implementation caches a match index, to
         ;; avoid a need to call uc-string-contains again.
         ;;
         (let* ((delim (->uc-string delim))
                (delim-len (uc-string-length delim)))
           (define (is-zero? x)
             (and x (zero? x)))
           (define (make-state i more)
             (vector i more
                     (and (not (is-zero? more))
                          (uc-string-contains str delim i end))))
           (cond
             ((= start end)
              ;;
              ;; An empty string.
              ;;
              (when (eq? grammar 'strict-infix)
                (error "uc-string-split: empty string \
                        with strict-infix grammar"))
              '())

             ((zero? delim-len)
              ;;
              ;; An empty delimiter splits the string into
              ;; single-character strings.
              ;;
              (unfold
               (lambda (st)
                 (or (not st)
                     (= (car st) end)))
               (lambda (st)
                 (let ((i (car st))
                       (more (cdr st)))
                   (if (is-zero? more)
                     (uc-string-copy str i end)
                     (uc-string (uc-string-ref str i)))))
               (lambda (st)
                 (let ((i (car st))
                       (more (cdr st)))
                   (if (is-zero? more)
                     #f
                     (cons (+ i 1)
                           (and more (- more 1))))))
               (cons start limit)))

             (else
              ;;
              ;; Split with delimiter.
              ;;
              (let ((raw-parts
                     (unfold
                      not
                      (lambda (st)
                        (let ((i (vector-ref st 0))
                              (cached-pos (vector-ref st 2)))
                          (uc-string-copy str i (or cached-pos end))))
                      (lambda (st)
                        (let ((more (vector-ref st 1))
                              (cached-pos (vector-ref st 2)))
                          (and cached-pos
                               (make-state (+ cached-pos delim-len)
                                           (and more (- more 1))))))
                      (make-state start limit))))

                (case grammar
                  ((prefix)
                   ;;
                   ;; Suppress empty prefixes.
                   ;;
                   (if (and (pair? raw-parts)
                            (uc-string-null? (car raw-parts)))
                     (cdr raw-parts)
                     raw-parts))
                  ((suffix)
                   ;;
                   ;; Suppress empty suffixes.
                   ;;
                   (if (and (pair? raw-parts)
                            (uc-string-null? (last raw-parts)))
                     (drop-right raw-parts 1)
                     raw-parts))
                  (else raw-parts)))))))))

;;;---------------------------------------------------------------------

    (define uc-string-replace
      (case-lambda
        ((str1 str2 start1 end1)
         (uc-string-replace str1 str2 start1 end1
                            0 (uc-string-length str2)))
        ((str1 str2 start1 end1 start2)
         (uc-string-replace str1 str2 start1 end1
                            start2 (uc-string-length str2)))
        ((str1 str2 start1 end1 start2 end2)
         (check-substring-bounds str1 start1 end1)
         (check-substring-bounds str2 start2 end2)
         (let ((replacement (uc-string-copy str2 start2 end2))
               (prefix (uc-string-copy str1 0 start1))
               (suffix (uc-string-copy str1 end1)))
           (uc-string-append prefix replacement suffix)))))

;;;---------------------------------------------------------------------

    ))
