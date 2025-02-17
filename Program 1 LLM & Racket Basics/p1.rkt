#lang racket

(define (read-integers filename)
  (with-input-from-file filename
    (lambda () (map string->number (string-split (read-line))))))

(define (count-frequencies lst)
  (foldl (lambda (x acc)
            (hash-update acc x add1 0))
         (make-immutable-hash '())
         lst))

(define (reconstruct-list freq-hash)
  (apply append (map (lambda (pair) (make-list (cdr pair) (car pair))) (hash->list freq-hash))))

(define (process-file filename output-filename)
  (let* ((numbers (read-integers filename))
         (freqs (count-frequencies numbers))
         (sorted-freqs (sort (hash->list freqs) < #:key car))
         (sorted-list (reconstruct-list (make-hash sorted-freqs))))
    ;; Open the output file for writing with the proper exists mode
    (with-output-to-file output-filename
      (lambda ()
        (printf "Processing ~a:\n" filename)
        (for-each (lambda (pair)
                    (printf "~a appears ~a times\n" (car pair) (cdr pair)))
                  sorted-freqs)
        (printf "        Sorted list: ~a\n" sorted-list))
      #:exists 'replace)  ;; This will replace the file if it exists
    ;; Return the sorted list
    sorted-list))

(define (process-multiple-files filenames output-filename)
  (for-each (lambda (filename) (process-file filename output-filename)) filenames))

;; Process files Data-1.txt to Data-7.txt
(define filenames (map (lambda (i) (string-append "Data-" (number->string i) ".txt")) (range 1 8)))

(process-multiple-files filenames "sorted_ints.txt")
