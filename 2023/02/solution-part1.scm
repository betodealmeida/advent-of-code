#!/usr/local/bin/guile
!#

(use-modules (ice-9 rdelim))
(use-modules (ice-9 regex))

(define (extract-cube-count str)
  ; "14 red, 4 green, 5 blue" => {red: 14, green: 4, blue: 5}
  (let ((hash-map (make-hash-table))
        (pairs (string-split str #\,)))
    (for-each
     (lambda (pair)
       (let* ((split-pair (string-split (string-trim pair #\space) #\space))
              (quantity (string->number (car split-pair)))
              (color (cadr split-pair)))
         (hash-set! hash-map color quantity)))
     pairs)
    hash-map))

(define (process-line line)
  (let* ((split-pair (string-split line #\:))
         (id (string->number (cadr (string-split (car split-pair) #\space))))
         (games (string-split (cadr split-pair) #\;)))
    (let loop ((games games))
      (if (null? games)
        id
        (let ((cube-count (extract-cube-count (car games))))
          (if (or (> (hash-ref cube-count "red" 0) 12)
                  (> (hash-ref cube-count "green" 0) 13)
                  (> (hash-ref cube-count "blue" 0) 14))
              0
              (loop (cdr games))))))))

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
(display "Sum of valid games: ")
(display total-sum)
(newline)
