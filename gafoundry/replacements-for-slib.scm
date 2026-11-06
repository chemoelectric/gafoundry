(define (software-type) 'unix)

(define slib:tab #\x09)
(define slib:form-feed #\x0C)

(define char-code-limit (+ 1 #x10ffff))

(define (identity x) x)

(define slib:error
  (let ((error error))
    (lambda args
      (let ((cep (current-error-port)))
        ;;(if (provided? 'trace) (print-call-stack cep))
        (display "Error: " cep)
        (for-each (lambda (x) (display #\space cep) (write x cep)) args)
        (error "")))))

(define slib:warn
  (lambda args
    (let ((cep (current-error-port)))
      ;;(if (provided? 'trace) (print-call-stack cep))
      (display "Warn: " cep)
      (for-each (lambda (x) (display #\space cep) (write x cep)) args))))

(define (call-with-input-string str proc)
  (let* ((port (open-input-string str))
         (result (proc port)))
    (close-input-port port)
    result))

(define (call-with-output-string proc)
  (let ((port (open-output-string))
        (str #f))
    (proc port)
    (set! str (get-output-string port))
    (close-output-port port)
    str))

;;---------------------------------------------------------------------
;;"sc2.scm" Implementation of rev2 procedures eliminated in subsequent versions.
;; Copyright (C) 1991, 1993 Aubrey Jaffer
;;
;;Permission to copy this software, to modify it, to redistribute it,
;;to distribute modified versions, and to use it for any purpose is
;;granted, subject to the following restrictions and understandings.
;;
;;1.  Any copy made of this software must include this copyright notice
;;in full.
;;
;;2.  I have made no warranty or representation that the operation of
;;this software will be error-free, and I am under no obligation to
;;provide any services, by way of maintenance, update, or otherwise.
;;
;;3.  In conjunction with products arising from the use of this
;;material, there shall be no use of my name in any advertising,
;;promotional, or sales literature without prior written consent in
;;each case.
(define (substring-move-left! string1 start1 end1 string2 start2)
  (do ((i start1 (+ i 1))
       (j start2 (+ j 1))
       (l (- end1 start1) (- l 1)))
      ((<= l 0))
    (string-set! string2 j (string-ref string1 i))))
(define (string-null? str)
  (= 0 (string-length str)))
;;---------------------------------------------------------------------

