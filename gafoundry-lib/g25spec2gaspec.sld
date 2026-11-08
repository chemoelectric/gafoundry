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

(define-library (gafoundry-lib g25spec2gaspec)

  (import (scheme base)
          (scheme char)
          (scheme write)
          (scheme process-context)
          (only (srfi 1) proper-list? append! any every)
          (srfi 37)
          (gafoundry-lib define-record-factory)
          (gafoundry-lib match)
          (gafoundry-lib format)
          (gafoundry-lib version)
          (gafoundry-lib xml-parse))

  (export g25spec2gaspec)

  (begin

;;;---------------------------------------------------------------------

    (define (check-string . strlst)
      (lambda (str)
        (unless (any (lambda (s) (string=? s str)) strlst)
          (error "expected a string to be one of those listed"
                 str strlst))
        str))

    (define (g25bool->boolean str)
      (unless (and (string? str)
                   (or (string=? str "false")
                       (string=? str "true")))
        (error "expected \"false\" or \"true\"" str))
      (string=? str "true"))

    (define sxml->gaspec
      (match-lambda
        (`(*TOP* . ,sxml)
         (sxml->gaspec sxml))
        (`((*PROCESSING-INSTRUCTIONS* . ,_) . ,sxml)
         (sxml->gaspec sxml))
        (`((g25spec . ,sxml))
         `(gaspec ,(g25spec->gaspec sxml)))
        (sxml
         (error "this might not be a g25spec" (format #f "~Y" sxml)))))

    (define g25spec->gaspec
      (match-lambda
        (`((@ . ,g25spec-attributes) . ,sxml)
         `(,@(map g25spec-attribute->gaspec g25spec-attributes)
           . ,(g25spec->gaspec sxml)))
        (fixme fixme)))

    (define g25spec-attribute->gaspec
      (match-lambda
        (`(license ,(? string? str))
         `(license ,((check-string "custom" "gpl" "bsd") str)))
        (`(language ,(? string? str))
         `(language ,(string-downcase str)))
        (`(namespace ,(? string? str))
         `(namespace ,str))
        (`(coordStorage ,(? string? str))
         `(coord-storage ,((check-string "array" "variables") str)))
        (`(defaultOperatorBindings ,(? string? str))
         `(default-operator-bindings ,(g25bool->boolean str)))
        (`(dimension ,(? string? str))
         (let ((n (string->number str)))
           (unless (and (exact-integer? n)
                        (positive? n))
             (error "dimension must be an exact positive integer" n))
           `(dimension ,n)))
        (`(testSuite ,(? string? str))
         `(test-suite ,(g25bool->boolean str)))
        (`(reportUsage ,(? string? str))
         `(report-usage ,(g25bool->boolean str)))
        (`(gmvCode ,(? string? str))
         `(gmv-code ,((check-string "expand" "runtime") str)))
        (`(parser ,(? string? str))
         `(parser ,((check-string "none" "builtin" "antlr") str)))
        (`(copyright ,(? string? str))
         `(copyright ,str))))

;;;---------------------------------------------------------------------

    (define-record-factory <seed>

      (predicate> seed?)

      (constructor> initial-seed
                    (lambda (construct)
                      (lambda ()
                        (construct (list) ; positionals
                                   ))))

      ;;
      ;; Positional arguments.
      ;;
      (getter> 1 get-positionals)
      (setter> 1 set-positionals!
               (lambda (setter!)
                 (lambda (obj value)
                   (unless (and (proper-list? value)
                                (every string? value))
                     (error "expected proper list of strings" value))
                   (setter! obj value))))
      )

    (define (usage-handler exit-status)
      (format #t "FIXME~&") ;;;;;; FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME
      (exit exit-status))

    (define options
      (list
       (option
        '("help") #f #f
        (lambda (opt name arg seed)
          (usage-handler 0)))
       (option
        '("version") #f #f
        (lambda (opt name arg seed)
          (show-version-and-exit 0)))
       ))

    (define (parse-arguments arguments)
      (define (handle-unknown-option opt name arg seed)
        (let ((port (current-error-port))
              (progname (car (command-line))))
          (format port "~a: unrecognized option: ~a~&"
                  progname name)
          (format port "Try “~a” --help' for more information.~&"
                  progname)
          (exit 1)))
      (define (handle-positionals str seed)
        (set-positionals!
         seed (append! (get-positionals seed) (list str)))
        seed)
      (args-fold arguments options
                 handle-unknown-option
                 handle-positionals
                 (initial-seed)))

    (define (exception-handler err)
      (let ((port (current-error-port)))
        (display "Error: " port)
        (cond ((error-object? err)
               (display (error-object-message err) port)
               (let ((irritants (error-object-irritants err)))
                 (unless (null? irritants)
                   (display ": " port)
                   (write irritants port))))
              ((string? err)
               (display err port))
              (else
               (write err port)))
        (newline port)
        (exit 1)))

    (define (g25spec2gaspec . arguments)
      (guard (exc (else (exception-handler exc)))
        (let* ((seed (parse-arguments arguments))
               (positionals (get-positionals seed)))
          (format #t "~Y"
                  (sxml->gaspec
                   (ssax:xml->sxml (current-input-port) (list)))))))

;;;---------------------------------------------------------------------

    ))
