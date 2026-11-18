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

(define-library (gafoundry-lib co-expression)

  (import (scheme base))

  (export make-generator
          make-co-expression
          make-co-expression*
          suspend
          current-end-of-sequence)

  (begin

    (define identity* (lambda ψ* (apply values ψ*)))

    (define current-end-of-sequence
      (make-parameter
       eof-object
       (lambda (obj)
         (unless (procedure? obj)
           (error "current-end-of-sequence: \
                   expected a thunk" obj))
         obj)))

    (define *suspend*
      (make-parameter identity*))

    (define (suspend . ψ*)
      (apply (*suspend*) ψ*))

    (define (make-generator thunk)
      ;;
      ;; No-values in, single-value out generator.
      ;;
      (letrec
          ((resumption-point
            (lambda (ω)
              (let ((what-suspend-runs
                     (lambda (ψ)
                       (let ((ω₁ (call/cc
                                  (lambda (α)
                                    (set! resumption-point α)
                                    ;; Do the suspension: pass control
                                    ;; back to the caller.
                                    (ω ψ)))))
                         (set! ω ω₁)))))
                (parameterize ((*suspend* what-suspend-runs))
                  ;; When thunk terminates, its return value is
                  ;; automatically yielded via suspend. In functional
                  ;; Scheme, the thunk returns its final value (such as
                  ;; (eof-object) to signal normal exhaustion).
                  (call-with-values thunk suspend)
                  ;; Subsequent invocations after exhaustion yield
                  ;; an end-of-sequence indefinitely:
                  (let loop ()
                    (suspend ((current-end-of-sequence)))
                    (loop)))))))
        (lambda ()
          (call/cc
           (lambda (ω)
             (resumption-point ω))))))

    (define (make-co-expression thunk)
      ;;
      ;; Single-value in, single-value out co-expressions.
      ;;
      (letrec
          ((resumption-point
            (lambda (ω ξ)
              (let ((what-suspend-runs
                     (lambda (ψ)
                       (let-values
                           (((ω₁ ξ₁)
                             (call/cc
                              (lambda (α)
                                (set! resumption-point α)
                                ;; Do the suspension: pass control
                                ;; back to the caller.
                                (ω ψ)))))
                         (set! ω ω₁)
                         ξ₁))))
                (parameterize ((*suspend* what-suspend-runs))
                  ;; When thunk terminates, its return value is
                  ;; automatically yielded via suspend. In functional
                  ;; Scheme, the thunk returns its final value (such
                  ;; as (eof-object) to signal normal exhaustion).
                  (call-with-values thunk suspend)
                  ;; Subsequent invocations after exhaustion yield
                  ;; an end-of-sequence indefinitely:
                  (let loop ()
                    (suspend ((current-end-of-sequence)))
                    (loop)))))))
        (lambda (ξ)
          (call/cc
           (lambda (ω)
             (resumption-point ω ξ))))))

    (define (make-co-expression* thunk)
      ;;
      ;; Multiple-values in, multiple-values out co-expressions.
      ;;
      (letrec
          ((resumption-point
            (lambda (ω . ξ*)
              (let ((what-suspend-runs
                     (lambda ψ*
                       (let-values
                           (((ω₁ . ξ₁*)
                             (call/cc
                              (lambda (α)
                                (set! resumption-point α)
                                ;; Do the suspension: pass control
                                ;; back to the caller.
                                (apply ω ψ*)))))
                         (set! ω ω₁)
                         (apply values ξ₁*)))))
                (parameterize ((*suspend* what-suspend-runs))
                  ;; When thunk terminates, its return value(s) are
                  ;; automatically yielded via suspend. In functional
                  ;; Scheme, the thunk returns its final value (such
                  ;; as (eof-object) to signal normal exhaustion).
                  (call-with-values thunk suspend)
                  ;; Subsequent invocations after exhaustion yield
                  ;; an end-of-sequence indefinitely:
                  (let loop ()
                    (suspend ((current-end-of-sequence)))
                    (loop)))))))
        (lambda ξ*
          (call/cc
           (lambda (ω)
             (apply resumption-point (cons ω ξ*)))))))

    ))
