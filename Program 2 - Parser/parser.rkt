#lang racket

;; Lexical analyzer
(define (tokenize input-string)
  (printf "Starting lexical analysis...\n")
  (define tokens '())
  (define current-line 1)
  (define line-positions '())
  
  (define (add-token type value)
    (set! tokens (append tokens (list (list type value current-line)))))
  
  (define (add-line-position pos)
    (set! line-positions (append line-positions (list (cons current-line pos)))))
  
  ;; Add initial line position for first line
  (add-line-position 0)
  
  (let loop ((i 0))
    (when (< i (string-length input-string))
      (let ((c (string-ref input-string i)))
        (cond
          ;; New line
          [(or (char=? c #\newline) (char=? c #\return))
           (when (or (not (< (add1 i) (string-length input-string)))
                     (not (and (char=? c #\return)
                               (char=? (string-ref input-string (add1 i)) #\newline))))
             (set! current-line (add1 current-line))
             (add-line-position (add1 i)))
           (loop (add1 i))]
          
          ;; Whitespace
          [(char-whitespace? c)
           (loop (add1 i))]
          
          ;; Keywords and identifiers
          [(char-alphabetic? c)
           (let id-loop ((j (add1 i)))
             (if (and (< j (string-length input-string))
                      (char-alphabetic? (string-ref input-string j)))
                 (id-loop (add1 j))
                 (let ((word (substring input-string i j)))
                   (cond
                     [(string=? word "if") (add-token 'IF word)]
                     [(string=? word "endif") (add-token 'ENDIF word)]
                     [(string=? word "read") (add-token 'READ word)]
                     [(string=? word "write") (add-token 'WRITE word)]
                     [else (add-token 'ID word)])
                   (loop j))))]
          
          ;; Numbers: digits
          [(char-numeric? c)
           (let num-loop ((j (add1 i)))
             (if (and (< j (string-length input-string))
                      (char-numeric? (string-ref input-string j)))
                 (num-loop (add1 j))
                 (begin
                   (add-token 'NUM (substring input-string i j))
                   (loop j))))]
          
          ;; Signs and operators
          [(char=? c #\+) (add-token 'PLUS "+") (loop (add1 i))]
          [(char=? c #\-) (add-token 'MINUS "-") (loop (add1 i))]
          [(char=? c #\=)
           (if (and (< (add1 i) (string-length input-string))
                   (char=? (string-ref input-string (add1 i)) #\=))
               (begin
                 (add-token 'EQUALS "==")
                 (loop (+ i 2)))
               (begin
                 (add-token 'ASSIGN "=")
                 (loop (add1 i))))]
          [(char=? c #\<)
           (if (and (< (add1 i) (string-length input-string))
                   (char=? (string-ref input-string (add1 i)) #\=))
               (begin
                 (add-token 'LESSEQ "<=")
                 (loop (+ i 2)))
               (begin
                 (add-token 'LESS "<")
                 (loop (add1 i))))]
          [(char=? c #\>)
           (if (and (< (add1 i) (string-length input-string))
                   (char=? (string-ref input-string (add1 i)) #\=))
               (begin
                 (add-token 'GREATEQ ">=")
                 (loop (+ i 2)))
               (begin
                 (add-token 'GREATER ">")
                 (loop (add1 i))))]
          [(char=? c #\!)
           (if (and (< (add1 i) (string-length input-string))
                   (char=? (string-ref input-string (add1 i)) #\=))
               (begin
                 (add-token 'NOTEQ "!=")
                 (loop (+ i 2)))
               (begin
                 (add-token 'ERROR "!")
                 (loop (add1 i))))]
          
          ;; Special characters
          [(char=? c #\;) (add-token 'SEMICOLON ";") (loop (add1 i))]
          [(char=? c #\() (add-token 'LPAREN "(") (loop (add1 i))]
          [(char=? c #\)) (add-token 'RPAREN ")") (loop (add1 i))]
          [(char=? c #\$)
           (if (and (< (add1 i) (string-length input-string))
                   (char=? (string-ref input-string (add1 i)) #\$))
               (begin
                 (add-token 'ENDMARKER "$$")
                 (loop (+ i 2)))
               (begin
                 (add-token 'ERROR "$")
                 (loop (add1 i))))]
          
          ;; Invalid character
          [else
           (add-token 'ERROR (string c))
           (loop (add1 i))]))))
  
  (printf "Lexical analysis complete. Found ~a tokens.\n" (length tokens))
  (values tokens line-positions))

;; Parse a line of code from a given file
(define (get-line-at-position input line-positions line-number)
  (define start-pos 
    (cond
      [(= line-number 1) 0]
      [(> line-number 1) 
       (let loop ([i 0])
         (cond
           [(>= i (length line-positions)) 0]
           [(= (caar line-positions) (sub1 line-number)) (cdar line-positions)]
           [(= (caar line-positions) line-number) (cdar line-positions)]
           [else (loop (add1 i))]))]
      [else 0]))
  
  (define end-pos
    (let loop ([i 0])
      (cond
        [(>= i (length line-positions)) (string-length input)]
        [(= (caar line-positions) line-number) (cdar line-positions)]
        [else (loop (add1 i))])))
  
  (if (< start-pos (string-length input))
      (substring input start-pos (min end-pos (string-length input)))
      ""))

;; Format parse tree with proper indentation for better readability
(define (format-parse-tree tree [indent 0])
  (define indent-str (make-string (* indent 2) #\space))
  
  (cond
    [(null? tree) ""]
    [(pair? tree)
     (let ([head (car tree)]
           [tail (cdr tree)])
       (cond
         [(pair? head)
          (string-append indent-str "(\n" 
                         (format-parse-tree head (add1 indent)) "\n"
                         (format-parse-tree tail (add1 indent)) "\n"
                         indent-str ")")]
         [(symbol? head)
          (string-append indent-str "(" (symbol->string head) "\n"
                         (format-parse-tree tail (add1 indent)) "\n"
                         indent-str ")")]
         [else
          (string-append indent-str (format "~a" head) "\n"
                         (format-parse-tree tail indent))]))]
    [(symbol? tree)
     (string-append indent-str (symbol->string tree))]
    [else
     (string-append indent-str (format "~a" tree))]))

;; Parser
(define (parse filename file-index)
  (printf "Starting to parse file: ~a\n" filename)
  
  (define output-filename (format "output~a.txt" file-index))
  (define output-port (open-output-file output-filename #:exists 'replace))

  (define (write-output str)
    (display str output-port)
    (newline output-port))
  
  (define (print-progress msg)
    (printf "~a\n" msg)
    (write-output msg))
  
  (print-progress (format "Processing file: ~a" filename))
  
  (when (not (file-exists? filename))
    (print-progress (format "Error: File not found: ~a" filename))
    (close-output-port output-port)
    (error (format "File not found: ~a" filename)))
  
  (define input (file->string filename))
  
  (print-progress "Starting lexical analysis...")
  (define-values (tokens line-positions) (tokenize input))
  (print-progress (format "Lexical analysis complete. Found ~a tokens." (length tokens)))
  
  (define current-token-index 0)
  (define parse-error #f)
  
  ;; Look at current token without consuming it
  (define (peek-token)
    (if (< current-token-index (length tokens))
        (list-ref tokens current-token-index)
        (list 'EOF "" 0)))
  
  ;; Consume and return the current token
  (define (consume-token)
    (let ((token (peek-token)))
      (set! current-token-index (add1 current-token-index))
      token))
  
  ;; Match a specific token type
  (define (match-token expected-type)
    (let ((token (peek-token)))
      (if (eq? (first token) expected-type)
          (consume-token)
          (begin
            (set! parse-error (format "Syntax error at line ~a: expected ~a but got ~a"
                                      (third token)
                                      expected-type
                                      (first token)))
            #f))))
  
  ;; Forward declarations for mutually recursive functions
  (define parse-expr #f)
  (define parse-stmt #f)
  (define parse-stmt-list #f)
  
  ;; program -> {stmt_list} $$
  (define (parse-program)
    (print-progress "Parsing program...")
    (let ((result (parse-stmt-list)))
      (if (and result (match-token 'ENDMARKER))
          (begin
            (print-progress "Program parsed successfully.")
            (list 'PROGRAM result))
          (begin
            (print-progress "Error parsing program.")
            #f))))
  
  ;; num -> numsign digit digit*
  (define (parse-num)
    (let ((token (peek-token)))
      (cond
        [(eq? (first token) 'PLUS)
         (consume-token) ; consume +
         (let ((num (match-token 'NUM)))
           (if num
               (list 'NUM (string->number (string-append "+" (second num))))
               #f))]
        
        [(eq? (first token) 'MINUS)
         (consume-token) ; consume -
         (let ((num (match-token 'NUM)))
           (if num
               (list 'NUM (string->number (string-append "-" (second num))))
               #f))]
        
        [(eq? (first token) 'NUM)
         (let ((num (match-token 'NUM)))
           (if num
               (list 'NUM (string->number (second num)))
               #f))]
        
        [else #f])))
  
  ;; compare -> < | <= | > | >= | == | !=
  (define (parse-compare)
    (let ((token (peek-token)))
      (cond
        [(eq? (first token) 'LESS)
         (consume-token)
         '<]
        [(eq? (first token) 'LESSEQ)
         (consume-token)
         '<=]
        [(eq? (first token) 'GREATER)
         (consume-token)
         '>]
        [(eq? (first token) 'GREATEQ)
         (consume-token)
         '>=]
        [(eq? (first token) 'EQUALS)
         (consume-token)
         '==]
        [(eq? (first token) 'NOTEQ)
         (consume-token)
         '!=]
        [else #f])))
  
  ;; etail -> + expr | - expr | compare expr | epsilon
  (define (parse-etail)
    (let ((token (peek-token)))
      (cond
        [(eq? (first token) 'PLUS)
         (consume-token) ; consume +
         (let ((expr-result (parse-expr)))
           (if expr-result
               (list 'ETAIL '+ expr-result)
               #f))]
        
        [(eq? (first token) 'MINUS)
         (consume-token) ; consume -
         (let ((expr-result (parse-expr)))
           (if expr-result
               (list 'ETAIL '- expr-result)
               #f))]
        
        [(or (eq? (first token) 'LESS)
             (eq? (first token) 'LESSEQ)
             (eq? (first token) 'GREATER)
             (eq? (first token) 'GREATEQ)
             (eq? (first token) 'EQUALS)
             (eq? (first token) 'NOTEQ))
         (let ((compare (parse-compare)))
           (if compare
               (let ((expr-result (parse-expr)))
                 (if expr-result
                     (list 'ETAIL compare expr-result)
                     #f))
               #f))]
        
        [else '()]))) ; epsilon case
  
  ;; Set the recursive functions after they've been declared
  (set! parse-expr
        (lambda ()
          (print-progress "Parsing expression...")
          (let ((token (peek-token)))
            (cond
              [(eq? (first token) 'ID)
               (let ((id (match-token 'ID)))
                 (if id
                     (let ((etail (parse-etail)))
                       (if (and etail (not (null? etail)))
                           (list 'EXPR (list 'ID (second id)) etail)
                           (list 'EXPR (list 'ID (second id)))))
                     #f))]
              
              [(or (eq? (first token) 'NUM)
                   (eq? (first token) 'PLUS)
                   (eq? (first token) 'MINUS))
               (let ((num (parse-num)))
                 (if num
                     (let ((etail (parse-etail)))
                       (if (and etail (not (null? etail)))
                           (list 'EXPR num etail)
                           (list 'EXPR num)))
                     #f))]
              
              [else #f]))))
  
  (set! parse-stmt
        (lambda ()
          (print-progress "Parsing statement...")
          (let ((token (peek-token)))
            (cond
              [(eq? (first token) 'ID)
               (let ((id (match-token 'ID)))
                 (if (and id (match-token 'ASSIGN))
                     (let ((expr (parse-expr)))
                       (if (and expr (match-token 'SEMICOLON))
                           (begin
                             (print-progress "Parsed assignment statement.")
                             (list 'ASSIGN_STMT (second id) expr))
                           #f))
                     #f))]
              
              [(eq? (first token) 'IF)
               (consume-token) ; consume IF
               (print-progress "Parsing IF statement...")
               (if (match-token 'LPAREN)
                   (let ((expr (parse-expr)))
                     (if (and expr (match-token 'RPAREN))
                         (let ((stmt-list (parse-stmt-list)))
                           (if (and stmt-list 
                                    (match-token 'ENDIF) 
                                    (match-token 'SEMICOLON))
                               (begin
                                 (print-progress "Parsed IF statement.")
                                 (list 'IF_STMT expr stmt-list))
                               #f))
                         #f))
                   #f)]
              
              [(eq? (first token) 'READ)
               (consume-token) ; consume READ
               (print-progress "Parsing READ statement...")
               (let ((id (match-token 'ID)))
                 (if (and id (match-token 'SEMICOLON))
                     (begin
                       (print-progress "Parsed READ statement.")
                       (list 'READ_STMT (second id)))
                     #f))]
              
              [(eq? (first token) 'WRITE)
               (consume-token) ; consume WRITE
               (print-progress "Parsing WRITE statement...")
               (let ((expr (parse-expr)))
                 (if (and expr (match-token 'SEMICOLON))
                     (begin
                       (print-progress "Parsed WRITE statement.")
                       (list 'WRITE_STMT expr))
                     #f))]
              
              [else #f]))))
  
  (set! parse-stmt-list
        (lambda ()
          (print-progress "Parsing statement list...")
          (let ((token (peek-token)))
            (if (or (eq? (first token) 'ID) 
                    (eq? (first token) 'IF)
                    (eq? (first token) 'READ)
                    (eq? (first token) 'WRITE))
                (let ((stmt (parse-stmt)))
                  (if stmt
                      (let ((stmt-list (parse-stmt-list)))
                        (if stmt-list
                            (list 'STMT_LIST stmt stmt-list)
                            (list 'STMT_LIST stmt)))
                      (list 'STMT_LIST)))  ; error in stmt, return empty list
                (begin
                  (print-progress "End of statement list.")
                  (list 'STMT_LIST))))))  ; epsilon case
  
  ;; Start parsing
  (let ((parse-tree (parse-program)))
    (if parse-error
        (let* ((error-token (peek-token))
               (line-number (third error-token))
               (line-content (get-line-at-position input line-positions line-number))
               (error-message (string-append parse-error "\nLine " (number->string line-number) ": " line-content)))
          (print-progress (format "Parse Error: ~a" error-message))
          (print-progress "Parsing failed.")
          (close-output-port output-port)
          error-message)
        (let ((result (format "Accept\n~a" parse-tree))
              (formatted-tree (format-parse-tree parse-tree)))
          (print-progress "Parsing successful.")
          (print-progress "Parse Tree:")
          (write-output formatted-tree)
          (print-progress "Final Result: Accept")
          (close-output-port output-port)
          (printf "Results written to: ~a\n" output-filename)
          result))))

;; Main function
(define (process-files filenames)
  (printf "Starting to process ~a files...\n" (length filenames))
  (for ([filename filenames]
        [i (in-naturals 1)])
    (printf "\nProcessing file ~a/~a: ~a\n" i (length filenames) filename)
    (parse filename i))
  (printf "\nAll files processed.\n"))

;; Export the parse function for use
(provide parse process-files)

;; Process files
(define files-to-process '("file1.txt" "file2.txt" "file3.txt" "file4.txt" "file5.txt"))
(process-files files-to-process)