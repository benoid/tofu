#lang racket/base

(require racket/string
         racket/list
         racket/match
         racket/set
         racket/port
         syntax/strip-context)
(provide (rename-out [soy-read read]
                     [soy-read-syntax read-syntax]))

(define loop-size 5)
(define (builder x) (lambda (y) x))

(struct chef-tok (value) #:transparent)
(struct chef-byte chef-tok () #:transparent)
(struct chef-read-var chef-tok ()#:transparent)
(struct chef-write-var chef-tok ()#:transparent)

(define (chef-parse in)
  (define chef-reader read-char)

  (define (get-var-tok acc delim)
    (define current-byte (chef-reader in))
    (when (eof-object? current-byte)
      (error "Tofu Chef Syntax error: expected " 
        (make-string 1 delim) 
        " to close expression"))
  (if (eq? current-byte delim) 
    acc 
    (get-var-tok (cons current-byte acc) delim)))
  (define (process acc)
    (define current-byte (chef-reader in))
    (cond [(eof-object? current-byte) acc]
          [(eq? current-byte #\\)
           (let ([next-byte (chef-reader in)])
             (if (set-member? (set #\\ #\{ #\} #\( #\)) next-byte) 
               (process (cons (chef-byte (char->integer next-byte)) acc))
               (error "Tofu Chef Syntax error: invalid escape character \\" 
                      next-byte)))]
          [(eq? current-byte #\{) 
           (process 
             (cons 
               (chef-read-var
                 (list->string 
                   (reverse (get-var-tok (list) #\}))))
               acc))]
          [(eq? current-byte #\() 
           (process 
             (cons 
               (chef-write-var
                 (list->string 
                   (reverse (get-var-tok (list) #\)))))
               acc))]
          [(eq? current-byte #\}) 
             (error "Tofu Chef Syntax error: unexpected closing brace")]
          [else
            (process (cons (chef-byte (char->integer current-byte)) acc))]))
  (when (eq? (peek-char in) #\newline)
    (read-char in))
  (when (eq? (peek-char in) #\newline)
    (read-char in))
   (reverse (process (list))))

(define (tofufy input)
  (define env (make-hash))
  (define (process in last-byte acc)
    (if (null? in) acc
    (let ([stx-obj (car in)])
      (match stx-obj
        [(chef-read-var v)
         (let ([address (hash-count env)])
           (hash-set! env v address)
           (process (cdr in) last-byte 
             (cons
               (string-append
                 "%|||"
                 (string-join (build-list address (builder "|[|]")) "")
                 "|,__________[^^^^^^^^^^|,__________]I[I]"
                 (string-join (build-list address (builder "I[I]")) "")
                 "III")
               acc)))]
        [(chef-write-var v)
         (let ([address (hash-ref env v)])
           (process (cdr in) last-byte 
             (cons
               (string-append
                 "|||"
                 (string-join (build-list address (builder "|[|]")) "")
                 "|[.|]I[I]"
                 (string-join (build-list address (builder "I[I]")) "")
                 "III")
               acc)))]
        [(chef-byte current-byte)
         (let* 
           ([diff (- current-byte last-byte)]
            [op (if (negative? diff) #\_ #\^)]
            [post-op (if (negative? diff) #\_ #\^)]
            [loops (quotient (abs diff) loop-size)]
            [post (modulo (abs diff) loop-size)])
           (process (cdr in) current-byte 
            (cons
             (string-append
               (if (> loops 0) "|" "")
               (build-string loops (builder #\^))
               (if (> loops 0) 
               (string-append "[I"
               (build-string loop-size (builder op))
               "|_]")
               "")
               (if (> loops 0) "I" "")
               (build-string post (builder post-op))
               ".")
            acc)))]
       [_ acc]))))
  (string-join (reverse (process input 0 (list))) ""))

(define (cubify str acc)
  (define strlen (string-length str))
  (define newstrlen (random 1 (max strlen 2)))
  (define left-pad 
    (string-append "\n"
      (build-string (random 1 65) (lambda (x) #\space))))
  (define (make-line x)
    (substring str (car x) (cdr x)))
  (define width (random (min 3 newstrlen) (min (+ newstrlen 1) 25)))
  (define line-count (min (max (quotient width 2) 1) (quotient newstrlen width)))
  (define rem (- strlen (* width line-count)))
  (define leftover 
    (substring str (- strlen rem)))
  (define strs
    (string-join
      #:before-first left-pad
      (map make-line
      (for/list ([i (range line-count)])
        (cons (* i width) (+ width (* i width)) ))) left-pad))
  (if (<= rem 1) (cons strs acc)
    (cubify leftover (cons strs acc))))

(define (soy-read in)
  (soy-read-syntax #f in))

(define (soy-read-syntax src in)
 (define in+newline
   (input-port-append #f in (open-input-string "\n")))
 (with-syntax 
   ([tofu (string-join 
            (reverse (cubify (tofufy (chef-parse in+newline)) (list)))
          "\n\n")])
  (strip-context 
#'(module soy racket/base
        (printf "#lang tofu\n\n~a\n" tofu)))))
