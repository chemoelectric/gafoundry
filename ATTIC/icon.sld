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

(define-library (gafoundry-lib icon)

  (import (scheme base)
          (scheme case-lambda)
          (gafoundry-lib co-expression)
          (gafoundry-lib uc-string))

  (export &fail
          &fail?
          &null
          &null?
          icon-with-fail
          icon-drive-generator
          icon-first-result
          icon-trail-push!
          icon-trail-unwind!
          <-
          <<-
          icon-or
          icon-and
          icon-every
          icon-limit
          icon-if
          icon-not
          fail-unless
          fail-if
          icon-to
          icon!
          icon-current-index-base
          icon-index->offset
          offset->icon-index)

  (begin

;;;---------------------------------------------------------------------
;;;
;;; The equivalent of Icon’s &fail keyword.
;;;
;;; Here it is a record type and can be tested for by the &fail?
;;; predicate.
;;;

    (define-syntax define-valueless-icon-keyword
      (syntax-rules ()
        ((define-icon-keyword type name predicate)
         (begin
           (define-record-type type
             (maker)
             predicate)
           (define name (maker))))))

    (define-valueless-icon-keyword <&fail> &fail &fail?)

;;;---------------------------------------------------------------------
;;;
;;; The &null keyword is made in the same way as &fail and comes with
;;; a &null? predicate.
;;;

    (define-valueless-icon-keyword <&null> &null &null?)

;;;---------------------------------------------------------------------
;;;
;;; Dynamic undo trail.
;;;

    (define *trail* (make-parameter '()))

    (define (icon-trail-push! undo-thunk)
      (*trail* (cons undo-thunk (*trail*))))

    (define (icon-trail-unwind! saved-trail)
      (let loop ((t (*trail*)))
        (unless (eq? t saved-trail)
          ((car t))
          (loop (cdr t))))
      (*trail* saved-trail))

    (define-syntax <-
      ;;
      ;; Reversible variable assignment (evaluates to `new`).
      ;;
      (syntax-rules ()
        ((_ var val-expr)
         (let ((old var)
               (new val-expr))
           (icon-trail-push! (lambda () (set! var old)))
           (set! var new)
           new))))

    (define-syntax <<-
      ;;
      ;; Reversible parameter mutation (evaluates to `new`).
      ;;
      (syntax-rules ()
        ((_ param val-expr)
         (let ((old (param))
               (new val-expr))
           (icon-trail-push! (lambda () (param old)))
           (param new)
           new))))

;;;---------------------------------------------------------------------
;;;
;;; Bounded goal-directed drivers.
;;;

    (define-syntax icon-with-fail
      (syntax-rules ()
        ((_ body ...)
         (parameterize ((current-end-of-sequence (lambda () &fail))
                        (*trail* '()))
           body ...))))

    (define (icon-first-result thunk)
      ;;
      ;; Evaluates “thunk” in a single-result (bounded) context.
      ;; Discards remaining choice points immediately upon the first
      ;; result.
      ;;
      (let ((checkpoint (*trail*))
            (g (make-generator thunk)))
        (let ((v (g)))
          (when (&fail? v)
            (icon-trail-unwind! checkpoint))
          v)))

    (define (icon-drive-generator thunk yield)
      ;;
      ;; Drives a generator `thunk` through all alternatives,
      ;; unwinding the trail whenever downstream backtracks or “thunk”
      ;; is exhausted.
      ;;
      (let ((g (make-generator thunk))
            (checkpoint (*trail*)))
        (let loop ()
          (let ((v (g)))
            (if (&fail? v)
              (icon-trail-unwind! checkpoint)
              (begin
                (yield v)
                (icon-trail-unwind! checkpoint)
                (loop)))))))

;;;---------------------------------------------------------------------
;;;
;;; Icon control combinators.
;;;

    (define-syntax icon-or
      ;;
      ;; Alternation (Icon’s “|” operator): yields all results of e1,
      ;; then e2, etc.
      ;;
      (syntax-rules ()
        ((_ e) e)
        ((_ e1 e2 ...)
         (begin
           (icon-drive-generator (lambda () e1) suspend)
           (icon-or e2 ...)))))

    (define-syntax icon-and
      ;;
      ;; Conjunction (Icon’s “&” operator) with optional “(var <=
      ;; expr)” binding. Backtracks left-to-right across conjuncts.
      (syntax-rules (<=)
        ((_ e) e)
        ((_ (var <= e1) e2 ...)
         (begin
           (icon-drive-generator
            (lambda () e1)
            (lambda (var)
              (icon-drive-generator (lambda () (icon-and e2 ...))
                                    suspend)))
           &fail))
        ((_ e1 e2 ...)
         (begin
           (icon-drive-generator
            (lambda () e1)
            (lambda (_)
              (icon-drive-generator (lambda () (icon-and e2 ...))
                                    suspend)))
           &fail))))

    (define-syntax icon-every
      ;;
      ;; Exhaustive driver (Icon’s “every” construct).
      ;;
      (syntax-rules ()
        ((_ gen-expr)
         (icon-with-fail
          (icon-drive-generator (lambda () gen-expr)
                                (lambda (_) #f))
          &fail))
        ((_ (var gen-expr) body ...)
         (icon-with-fail
          (icon-drive-generator (lambda () gen-expr)
                                (lambda (var) body ...))
          &fail))))

    (define-syntax icon-limit
      ;;
      ;; Limitation (Icon’s “expr \ n” operator). Bounds “gen-expr” to
      ;; at most “n” results.
      ;;
      (syntax-rules ()
        ((_ n-expr gen-expr)
         (let ((n n-expr)
               (count 0)
               (checkpoint (*trail*)))
           (when (positive? n)
             (let ((g (make-generator (lambda () gen-expr))))
               (let loop ()
                 (when (< count n)
                   (let ((v (g)))
                     (if (&fail? v)
                       (icon-trail-unwind! checkpoint)
                       (begin
                         (set! count (+ count 1))
                         (suspend v)
                         (icon-trail-unwind! checkpoint)
                         (loop))))))))
           &fail))))

    (define-syntax icon-if
      ;;
      ;; Bounded conditional (Icon’s “if test then e1 [else e2]”).
      ;;
      (syntax-rules (then else)
        ((_ test-expr then then-expr)
         (let ((checkpoint (*trail*)))
           (if (&fail? (icon-first-result (lambda () test-expr)))
             &fail
             (let ((res then-expr))
               (when (&fail? res)
                 (icon-trail-unwind! checkpoint))
               res))))
        ((_ test-expr then then-expr else else-expr)
         (let ((checkpoint (*trail*)))
           (if (&fail? (icon-first-result (lambda () test-expr)))
             else-expr
             (let ((res then-expr))
               (when (&fail? res)
                 (icon-trail-unwind! checkpoint))
               res))))))

    (define-syntax icon-not
      ;;
      ;; Bounded negation (Icon’s “not expr”).
      ;;
      (syntax-rules ()
        ((_ expr)
         (let ((checkpoint (*trail*)))
           (let ((v (icon-first-result (lambda () expr))))
             (icon-trail-unwind! checkpoint)
             (if (&fail? v)
               &null
               &fail))))))

;;;---------------------------------------------------------------------
;;;
;;; Generator helpers.
;;;

    (define (fail-unless pred)
      (if pred &null &fail))

    (define (fail-if pred)
      (if pred &fail &null))

    (define-syntax icon-to
      ;;
      ;; Suspend a macro-expanded sequence of values.
      ;; 
      (syntax-rules ()
        ((_ val ...)
         (begin (suspend val) ... &fail))))

    (define-syntax icon!-vector-loop
      (syntax-rules ()
        ((icon!-vector-loop obj obj-length obj-ref)
         (let ((len (obj-length obj)))
           (do ((i 0 (+ i 1)))
               ((= len))
             (suspend (obj-ref obj i)))
           &fail))))

    (define (icon! obj)
      ;;
      ;; Element generator (Icon’s “!” operator).
      ;;
      (cond
        ((uc-string? obj)
         (icon!-vector-loop obj uc-string-length uc-string-ref))
        ((string? obj)
         (icon!-vector-loop obj string-length string-ref))
        ((pair? obj)
         (do ((xs obj (cdr xs)))
             ((not (pair? xs)))
           (suspend (car xs)))
         &fail)
        ((vector? obj)
         (icon!-vector-loop obj vector-length vector-ref))
        ((bytevector? obj)
         (icon!-vector-loop obj bytevector-length bytevector-u8-ref))
        (else
         (error "icon! is not implemented for this object type" obj))))

;;;---------------------------------------------------------------------
;;;
;;; Generalized Icon indexing.
;;;
;;; Icon indexing goes (1..length+1) and equivalently (-length..0). We
;;; generalize that by allowing any index base other than 1.
;;;

    (define icon-current-index-base
      (make-parameter
       1
       (lambda (obj)
         (unless (exact-integer? obj)
           (error "icon-current-index-base: \
                   expected an exact integer" obj))
         obj)))

    (define (icon-index->offset len)
      (let ((ix->offset
             (lambda (i index-base)
               (let ((offset (if (< i index-base)
                               (+ (- i index-base) len 1)
                               (- i index-base))))
                 (if (<= 0 offset len)
                   offset
                   &fail)))))
        (case-lambda
          ((i) (ix->offset i (icon-current-index-base)))
          ((i index-base) (ix->offset i index-base)))))

    (define offset->icon-index
      ;;
      ;; Convert a zero-based offset to a generalized Icon index.
      ;;
      (case-lambda
        ((offset) (+ offset (icon-current-index-base)))
        ((offset index-base) (+ offset index-base))))

;;;---------------------------------------------------------------------

    ))
