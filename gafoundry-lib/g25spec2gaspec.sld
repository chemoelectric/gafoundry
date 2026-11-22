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
          (scheme cxr)
          (scheme char)
          (scheme write)
          (scheme process-context)
          (only (srfi 1)
                drop-right take-right
                proper-list? append!
                any every)
          (srfi 14)
          (srfi 37)
          (only (srfi 152)
                string-drop string-every
                string-null? string-prefix?
                string-split string-trim-both)
          (gafoundry-lib define-record-factory)
          (gafoundry-lib match)
          (gafoundry-lib format)
          (gafoundry-lib version)
          (gafoundry-lib xml-parse)
          (gafoundry-lib uc-string)
          (gafoundry-lib snobol-match)
          (gafoundry-lib snobol-char-set))

  (export g25spec2gaspec)

  (begin

;;;---------------------------------------------------------------------

    (define (getvar key match-result)
      (and match-result
           (let ((p (assoc key (cdr match-result))))
             (and p (cdr p)))))

;;;---------------------------------------------------------------------

    (define (sxml->gaspec sxml)

      (define dimension #f)
      (define basis-vector-names #f)

      (define (match-basis-vector str)
        (let loop ((lst basis-vector-names))
          (cond ((null? lst) #f)
                ((snobol-match
                  (p:seq
                   (p:maybe-many (p:span-char-set char-set:whitespace))
                   (p:assign-local (p:lit (car lst)) 'vector-name)
                   (p:alt
                    (p:seq
                     (p:maybe-many
                      (p:span-char-set char-set:whitespace))
                     (p:assign-local
                      (p:alt
                       (p:lit ".")
                       (p:lit "^")
                       (p:lit "=")
                       (p:lit "\x0;"))
                      'operator))
                    (p:assign-local
                     (p:span-char-set char-set:whitespace)
                     'operator)))
                  str) =>
                  (lambda (match-result)
                    (values (getvar 'vector-name match-result)
                            (getvar 'operator match-result))))
                (else (loop (cdr lst))))))

      (define (check-string . strlst)
        (lambda (str)
          (unless (any (lambda (s) (string=? s str)) strlst)
            (error "expected a string to be one of those listed"
                   str strlst))
          str))

      (define (check-symbol . strlst)
        (lambda (str)
          (string->symbol ((apply check-string strlst) str))))

      (define (g25bool->boolean str)
        (unless (and (string? str)
                     (or (string=? str "false")
                         (string=? str "true")))
          (error "expected \"false\" or \"true\"" str))
        (string=? str "true"))

      (define sxml->gaspec--entry-point
        (match-lambda
          (`(*TOP* . ,sxml)
           (sxml->gaspec sxml))
          (`((*PROCESSING-INSTRUCTIONS* . ,_) . ,sxml)
           (sxml->gaspec sxml))
          (`((g25spec . ,sxml))
           `(gaspec ,(g25spec->gaspec sxml)))
          (sxml
           (error "this might not be a g25spec"
                  (format #f "~Y" sxml)))))

      (define g25spec->gaspec
        (match-lambda
          (`((@ . ,g25spec-attributes) . ,g25spec-elements)
           (set-dimension! g25spec-attributes)
           (set-basis-vector-names! g25spec-elements)
;;;;;;           (let-values (((x y) (match-basis-vector (string->uc-string "  e1   e2"))))(format #t "~y ~y" x y));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           `(,@(map g25spec-attribute->gaspec g25spec-attributes)
             . ,(map g25spec-element->gaspec g25spec-elements)))))

      (define (set-dimension! g25spec-attributes)
        (let ((dimension-entry (assq 'dimension g25spec-attributes)))
          (unless (and dimension-entry
                       (= 2 (length dimension-entry))
                       (string? (cadr dimension-entry)))
            (error "no valid dimension entry found"))
          (set! dimension (str->dimension (cadr dimension-entry)))))

      (define (set-basis-vector-names! g25spec-elements)
        (let ((basisVectorNames-attributes
               (assq 'basisVectorNames g25spec-elements)))
          (unless (and basisVectorNames-attributes
                       (eq? '@ (caadr basisVectorNames-attributes))))
          (set! basis-vector-names
            (map symbol->uc-string
                 (basisVectorNames->gaspec
                  (cdadr basisVectorNames-attributes))))))

      (define g25spec-element->gaspec
        (match-lambda
          (`(inline (@ . ,inline-attributes))
           `(inline ,@(map inline-attribute->gaspec inline-attributes)))
          (`(floatType (@ . ,floatType-attributes))
           `(float-type ,@(map floatType-attribute->gaspec
                               floatType-attributes)))
          (`(basisVectorNames (@ . ,basisVectorNames-attributes))
           `(basis-vector-names ,(basisVectorNames->gaspec
                                  basisVectorNames-attributes)))
          (`(metric (@ . ,metric-attributes) . ,metric-elements)
           `(metric . ,(metric->gaspec metric-attributes
                                       metric-elements)))
          (`(unaryOperator (@ . ,unaryOperator-attributes))
           `(unary-operator ,@(map unaryOperator-attribute->gaspec
                                   unaryOperator-attributes)))
          (`(binaryOperator (@ . ,binaryOperator-attributes))
           `(binary-operator ,@(map binaryOperator-attribute->gaspec
                                    binaryOperator-attributes)))
          (`(mv (@ . ,mv-attributes) . ,mv-elements)
           `(mv . ,(mv->gaspec mv-attributes mv-elements)))
          (fixme
           fixme)))

      (define (str->dimension str)
        (let ((n (string->number str)))
          (unless (and (exact-integer? n)
                       (positive? n))
            (error "dimension must be an exact positive integer" n))
          n))

      (define g25spec-attribute->gaspec
        (match-lambda
          (`(license ,(? string? str))
           `(license ,((check-symbol "custom" "gpl" "bsd") str)))
          (`(language ,(? string? str))
           `(language ,(string->symbol (string-downcase str))))
          (`(namespace ,(? string? str))
           `(namespace ,str))
          (`(coordStorage ,(? string? str))
           `(coord-storage ,((check-symbol "array" "variables") str)))
          (`(defaultOperatorBindings ,(? string? str))
           `(default-operator-bindings ,(g25bool->boolean str)))
          (`(dimension ,(? string? str))
           `(dimension ,(str->dimension str)))
          (`(testSuite ,(? string? str))
           `(test-suite ,(g25bool->boolean str)))
          (`(reportUsage ,(? string? str))
           `(report-usage ,(g25bool->boolean str)))
          (`(gmvCode ,(? string? str))
           `(gmv-code ,((check-symbol "expand" "runtime") str)))
          (`(parser ,(? string? str))
           `(parser ,((check-symbol "none" "builtin" "antlr") str)))
          (`(copyright ,(? string? str))
           `(copyright ,str))))

      (define inline-attribute->gaspec
        (match-lambda
          (`(constructors ,(? string? str))
           `(constructors ,(g25bool->boolean str)))
          (`(set ,(? string? str))
           `(set ,(g25bool->boolean str)))
          (`(assign ,(? string? str))
           `(assign ,(g25bool->boolean str)))
          (`(operators ,(? string? str))
           `(operators ,(g25bool->boolean str)))
          (`(functions ,(? string? str))
           `(functions ,(g25bool->boolean str)))))

      (define floatType-attribute->gaspec
        (match-lambda
          (`(type ,(? string? str))
           `(type ,str))
          (`(suffix ,(? string? str))
           `(suffix ,str))
          (`(prefix ,(? string? str))
           `(prefix ,str))))

      (define (basisVectorNames->gaspec attributes)
        (let ((lst (list #f)))
          (do ((i 1 (+ i 1))
               (p lst (cdr p)))
              ((= i (+ dimension 1)))
            (let* ((key (string->symbol
                         (string-append "name" (number->string i))))
                   (value (cadr (assq key attributes))))
              (set-cdr! p (list (string->symbol value)))))
          (cdr lst)))

      (define (metric->gaspec metric-attributes metric-elements)
        (let* ((lst (list #f))
               (p lst))
          (define (put! entry)
            (set-cdr! p (list entry))
            (set! p (cdr p)))
          (define put-attribute!
            (match-lambda
              (`(name ,(? string? str))
               (put! `(name ,str)))
              (`(round ,(? string? str))
               (put! `(round ,(g25bool->boolean str))))))
          (define (put-element! elem)
            (let* ((parts (string-split elem "=" 'strict-infix))
                   (pairs (map (lambda (s)
                                 (map string->symbol
                                      (map string-trim-both
                                           (string-split
                                            s "." 'strict-infix))))
                               (drop-right parts 1)))
                   (value (string-trim-both
                           (car (take-right parts 1)))))
              ;;
              ;; “sp” for the scalar product.
              ;;
              (put! `(sp ,@pairs ,value))))
          (for-each put-attribute! metric-attributes)
          (for-each put-element! metric-elements)
          (cdr lst)))

      (define unaryOperator-attribute->gaspec
        (match-lambda
          (`(symbol ,(? string? str))
           `(symbol ,str))
          (`(prefix ,(? string? str))
           `(fixity ,(if (g25bool->boolean str) 'prefix 'suffix)))
          (`(function ,(? string? str))
           `(function ,str))))

      (define binaryOperator-attribute->gaspec
        (match-lambda
          (`(symbol ,(? string? str))
           `(symbol ,str))
          (`(function ,(? string? str))
           `(function ,str))))

      (define (mv->gaspec mv-attributes mv-elements)
        (let* ((lst (list #f))
               (p lst)
               (compress 'by-grade)
               (coord-order 'default)
               (mem-alloc 'full))
          (define (put! entry)
            (set-cdr! p (list entry))
            (set! p (cdr p)))
          (define put-attribute!
            (match-lambda
              (`(name ,(? string? str))
               (put! `(name ,str)))
              (`(compress ,(? string? str))
               (set! compress ((check-symbol "byGrade" "byGroup") str))
               (case compress
                 ((byGrade) (set! compress 'by-grade))
                 ((byGroup) (set! compress 'by-group)))
               (put! `(compress ,compress)))
              (`(coordinateOrder ,(? string? str))
               (set! coord-order ((check-symbol "default" "custom")
                                  str))
               (put! `(coordinate-order ,coord-order)))
              (`(memAlloc ,(? string? str))
               (set! mem-alloc ((check-symbol "full" "parityPure"
                                              "dynamic")
                                str))
               (when (eq? mem-alloc 'parityPure)
                 (set! mem-alloc 'parity-pure))
               (put! `(mem-alloc ,mem-alloc)))))
          (define (put-mv-elements!)
            (case compress
              ((by-grade) (put-mv-elements-by-grade!))
              ((by-group) (put-mv-elements-by-group!))
              (else (error "internal error: unexpected branch"))))
          (define (put-mv-elements-by-grade!)
            'FIXME)
          (define (put-mv-elements-by-group!)
            'FIXME)
          (for-each put-attribute! mv-attributes)
          (when (eq? coord-order 'custom)
            (put-mv-elements!))
          (cdr lst)))

      (sxml->gaspec--entry-point sxml)

      ) ;; sxml->gaspec

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
