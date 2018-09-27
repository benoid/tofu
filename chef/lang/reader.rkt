#lang racket/base

(require racket/string
         racket/list
         racket/port
         syntax/strip-context)
(provide (rename-out [soy-read read]
                     [soy-read-syntax read-syntax]))

(define loop-size 5)
(define (builder x) (lambda (y) x))

(define (tofufy in)
  (define (process last-byte acc)
    (define current-byte (read-byte in))
    (if (eof-object? current-byte) acc
     (let* ([diff (- current-byte last-byte)]
            [op (if (negative? diff) #\_ #\^)]
            [post-op (if (negative? diff) #\_ #\^)]
            [loops (quotient (abs diff) loop-size)]
            [post (modulo (abs diff) loop-size)])
      (process current-byte 
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
        acc)))))
  (when (eq? (peek-char in) #\newline)
    (read-char in))
  (when (eq? (peek-char in) #\newline)
    (read-char in))
  (string-join (reverse (process 0 (list))) ""))

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
            (reverse (cubify (tofufy in+newline) (list))) 
            "\n\n")])
  (strip-context 
#'(module soy racket/base
        (printf "#lang tofu\n\n~a\n" tofu)))))
