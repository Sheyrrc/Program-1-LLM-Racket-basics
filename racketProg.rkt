#lang racket

(displayln "Welcome <3!")
(require racket/string)
(require racket/list)

; Utility function 1: Convert list to value-count pairs efficiently
(define (make-value-count-pairs numbers)
  (define counts (make-hash))
  (for ([n numbers]) (hash-update! counts n add1 0))
  (sort (hash-map counts list) < #:key first))

; Utility function 2: Convert pairs to sorted list efficiently
(define (pairs->sorted-list pairs)
  (apply append (map (lambda (pair) (build-list (second pair) (λ (_) (first pair)))) pairs)))

; Utility function 3: Check if list is sorted
(define (is-sorted? lst)
  (for/and ([a lst] [b (cdr lst)]) (<= a b)))

; File reading function optimized for large files
(define (read-file-contents filename)
  (displayln (format "Attempting to open ~a" filename))
  (display "\t...")
  (with-handlers ([exn:fail? (lambda (exn)
                               (define msg (exn-message exn))
                               (displayln (format "Debug: Exception caught - ~a" msg))
                               (cond
                                 [(string-contains? msg "No such file or directory")
                                  (displayln "Error: File not found.")]
                                 [(string-contains? msg "Permission denied")
                                  (displayln "Error: Permission denied.")]
                                 [else
                                  (displayln "Error: Unable to read file.")])
                               #f)])
    (call-with-input-file filename
      (lambda (in)
        (define numbers '())
        (for ([line (in-lines in)])
          (set! numbers (append numbers (map string->number (string-split line)))))
        numbers))))

; User input functions
(define (get-operation)
  (displayln "\nEnter 'Q' once to return to the file selection menu.")
  (displayln "\nWhat would you like to do?")
  (displayln "A) Convert list to value-count pairs")
  (displayln "B) Sort value-count pairs")
  (displayln "C) Given a sorted list of value-count pairs, produce a sorted list of integers")
  (displayln "D) Given a list of integers, confirm the list is sorted")
  (displayln "Q) Quit")
  (display "Your choice (A/B/C/D/Q): ")
  (read-line))

(define (ask-for-filename)
  (newline)
  (display "Enter the file name (or 'q' to quit): ")
  (let ([filename (string-trim (read-line))])
    (if (string-ci=? filename "q")
        #f
        filename)))

(define (process-operation choice numbers)
  (let ([choice (string-upcase (string-trim choice))])
    (cond
      [(string=? choice "A")
       (let ([pairs (make-value-count-pairs numbers)])
         (displayln "\nOriginal numbers:")
         (displayln numbers)
         (displayln "\nValue-count pairs:")
         (displayln pairs))
       #t]
      [(string=? choice "B")
       (let ([sorted-pairs (make-value-count-pairs numbers)])
         (displayln "\nSorted pairs:")
         (displayln sorted-pairs))
       #t]
      [(string=? choice "C")
       (let* ([pairs (make-value-count-pairs numbers)]
              [sorted-list (pairs->sorted-list pairs)])
         (displayln "\nOriginal numbers:")
         (displayln numbers)
         (displayln "\nSorted numbers:")
         (displayln sorted-list))
       #t]
      [(string=? choice "D")
       (displayln "\nChecking if list is sorted...")
       (displayln (if (is-sorted? numbers)
                     "The list is sorted!"
                     "The list is NOT sorted."))
       #t]
      [(string=? choice "Q")
       #f]
      [else
       (displayln "\nInvalid choice. Please enter A, B, C, D, or Q.")
       #t])))

(define (main)
  (let filename-loop ()
    (define filename (ask-for-filename))
    (when filename
      (let ([numbers (read-file-contents filename)])
        (if numbers
            (let operation-loop ()
              (let ([continue? (process-operation (get-operation) numbers)])
                (when continue?
                  (operation-loop))))
            (displayln "Please try again.")))
      (filename-loop)))
  (displayln "\nGoodbye! <3"))

(main)