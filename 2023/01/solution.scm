#!/usr/local/bin/guile
!#

(use-modules (ice-9 rdelim))
(use-modules (ice-9 regex))

(define (find-first-and-last-digit str)
  (let* ((digit-regexp (make-regexp "[0-9]"))
         (first-match (regexp-exec digit-regexp str))
         (first-digit (if first-match
                          (match:substring first-match 0)
                          #f))
         (reversed-str (list->string (reverse (string->list str))))
         (last-match (regexp-exec digit-regexp reversed-str))
         (last-digit (if last-match
                         (match:substring last-match 0)
                         #f)))
    (if (and first-digit last-digit)
        (values first-digit last-digit)
        #f)))

(define (combine-digits first-digit last-digit)
  (let ((combined-str (string-append first-digit last-digit)))
    (string->number combined-str)))

(define (process-line line)
  (call-with-values
    (lambda () (find-first-and-last-digit line))
    combine-digits))

(define (read-file filename process-func)
  (let ((in-port (open-input-file filename)))
    (let loop ((line (read-line in-port))
               (acc 0))
      (if (eof-object? line)
          (begin
            (close-input-port in-port)
            acc)
          (loop (read-line in-port) (+ acc (process-func line)))))))


(define total-sum (read-file "input" process-line))
(display "Total sum: ")
(display total-sum)
(newline)
