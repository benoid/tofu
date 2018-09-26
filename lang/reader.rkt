#lang racket/base

(require racket/port
         syntax/strip-context
         racket/match
         racket/string
         racket/list
         "soylint.rkt")

(provide (rename-out [soy-read read]
                     [soy-read-syntax read-syntax]))

(define (soy-read in)
  (soy-read-syntax #f in))

(struct soy-inc () #:transparent)
(struct soy-dec () #:transparent)
(struct soy-left() #:transparent)
(struct soy-right() #:transparent)
(struct soy-in () #:transparent)
(struct soy-out () #:transparent)
(struct soy-begin () #:transparent)
(struct soy-end () #:transparent)
(struct soy-loop [stuff] #:transparent)

(define soy-begin-char #\[)
(define soy-end-char #\])

(define tokmap
(make-hash
  (list
    (cons #\^  soy-inc)
    (cons #\_  soy-dec)
    (cons #\|  soy-left)
    (cons #\I  soy-right)
    (cons #\,  soy-in)
    (cons #\.  soy-out))))

(define (remove-comments in)
  (define lines (port->lines in))
  (define new-lines
    (for/list ([str lines])
      (define str-list (string-split str "`"))
      (string-join
        (for/list ([s str-list]
                   [i (range (length str-list))]
                    #:when (even? i))
        s) "")))
   (string-join new-lines "\n"))

(define (soy-lex in)
  (define (tokenize in-port toklist)
    (define byte (read-byte in))
    (if (eof-object? byte) toklist
      (let ([char (integer->char byte)])
        (cond 
          [(char-whitespace? char) (tokenize in-port toklist)]
          [(eq? char soy-begin-char) 
           (tokenize in-port
             (cons 
               (soy-loop (reverse (tokenize in-port (list))))
               toklist))]
          [(eq? char soy-end-char) toklist]
          [(hash-has-key? tokmap char) 
           (tokenize in-port (cons ((hash-ref tokmap char)) toklist))]
          [else (error "invalid token: " char)]))))
  (reverse (tokenize in (list))))

(define (soy-eval prg)
  (define tape (make-hash))
  (define pointer 0)
  (define (current-byte)
    (check-byte!)
    (hash-ref tape pointer))
  (define (check-byte!)
    (unless (hash-has-key? tape pointer)
      (hash-set! tape pointer 0)))
  (define (move-left!)
    (set! pointer (+ pointer 1)))
  (define (move-right!)
    (set! pointer (- pointer 1)))
  (define (inc-byte!)
    (check-byte!)
    (hash-set! tape pointer (+ (hash-ref tape pointer) 1)))
  (define (dec-byte!)
    (check-byte!)
    (hash-set! tape pointer (- (hash-ref tape pointer) 1)))
  (define (read-byte!)
    (check-byte!)
    (hash-set! tape pointer (read-byte)))
  (define (write-byte!)
    (check-byte!)
    (write-byte (hash-ref tape pointer)))
    ;(printf "~a\n\n" tape))
  (define (loop-eval! stuff)
    (if (= (current-byte) 0) (void)
      (begin (stx-eval! stuff) (loop-eval! stuff))))
  (define (stx-eval! tree)
    (for ([tok tree])
      (match tok
        [(soy-inc) (inc-byte!)]
        [(soy-dec) (dec-byte!)]
        [(soy-left) (move-left!)]
        [(soy-right) (move-right!)]
        [(soy-in) (read-byte!)]
        [(soy-out) (write-byte!)]
        [(soy-loop stuff) 
           (loop-eval! stuff)])))
  (stx-eval! prg))

(define (soy-read-syntax src in)
 (define raw (port->string in))
 (define src (remove-comments (open-input-string raw)))
 (unless (soylint (port->lines (open-input-string raw)))
   (error "Tofu Syntax Error: cubed or go home"))
 (with-syntax ([data1 (soy-lex (open-input-string src))]
               [eval soy-eval])
  (strip-context 
#'(module soy racket/base
        (provide data)
        (define soy-eval 'eval)
        (define data 'data1)
        (soy-eval data)
        (newline)))))
