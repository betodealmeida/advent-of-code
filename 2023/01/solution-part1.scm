#!/usr/local/bin/guile
!#

(use-modules (ice-9 rdelim))
(use-modules (ice-9 regex))

(define (find-digits str)
  (let* ((digit-regexp (make-regexp "[0-9]"))
        (matches (list-matches digit-regexp str)))
    (if (not (null? matches))
        (let ((first-digit (match:substring (car matches) 0))
              (last-digit (match:substring (car (last-pair matches)) 0)))
          (values first-digit last-digit))
        (values #f #f))))

(define (combine-digits first-digit last-digit)
  (if (and first-digit last-digit)
      (string->number (string-append first-digit last-digit))
      0))

(define (process-line line)
  (call-with-values
    (lambda () (find-digits line))
    combine-digits))

(define (read-file-safe filename process-func)
  (catch 'system-error
    (lambda ()
      (let ((in-port (open-input-file filename)))
        (let loop ((line (read-line in-port))
                   (acc 0))
          (if (eof-object? line)
              (begin
                (close-input-port in-port)
                acc)
              (loop (read-line in-port) (+ acc (process-func line)))))))
    (lambda args
      (format #t "Error reading file: ~A~%" args)
      0)))

(define total-sum (read-file-safe "input" process-line))
(display "Total sum: ")
(display total-sum)
(newline)
