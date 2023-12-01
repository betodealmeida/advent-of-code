#!/usr/local/bin/guile
!#

(use-modules (ice-9 rdelim))
(use-modules (ice-9 regex))

(define (all-overlapping-matches regexp text)
  (let loop ((start 0) (matches '()))
    (let ((match (regexp-exec regexp text start)))
      (if match
          (loop (+ (match:start match) 1) (cons match matches))
          (reverse matches)))))

(define (replace-with-number str)
  (cond ((string=? str "one") "1")
        ((string=? str "two") "2")
        ((string=? str "three") "3")
        ((string=? str "four") "4")
        ((string=? str "five") "5")
        ((string=? str "six") "6")
        ((string=? str "seven") "7")
        ((string=? str "eight") "8")
        ((string=? str "nine") "9")
        (else str)))

(define (find-digits str)
  (let* ((digit-regexp (make-regexp "([0-9]|one|two|three|four|five|six|seven|eight|nine)"))
        (matches (all-overlapping-matches digit-regexp str)))
    (if (not (null? matches))
        (let ((first-digit (match:substring (car matches) 0))
              (last-digit (match:substring (car (last-pair matches)) 0)))
          (values (replace-with-number first-digit) (replace-with-number last-digit)))
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
