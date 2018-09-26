#lang racket/base

(require racket/list 
         racket/string
         racket/format
         racket/vector)

(provide soylint)

(define (erase-char! vlines i j)
  (string-set! (vector-ref vlines i) j #\space))

(define (get-char vlines i j)
  (string-ref (vector-ref vlines i) j))

(define (get-tofu-width str)
  (string-length 
    (list-ref (string-split str) 0)))

(define (get-tofu-height vlines i j)
  (cond [(<= (vector-length vlines) i) 0]
         [(<= (string-length (vector-ref vlines i)) j) 0]
         [(char-whitespace? (get-char vlines i j)) 0]
         [else
           (+ (get-tofu-height vlines (+ i 1) j) 1)]))

(define (char-tofu?! vlines i j)
  (define width (get-tofu-width (vector-ref vlines i)))
  (define height (get-tofu-height vlines i j))
  (define bottom-width 
    (get-tofu-width (vector-ref vlines (+ i (- height 1)))))
  (if (not (and (= bottom-width width))) #f
    (for/and ([k (range i (+ i height))])
      (define start (get-char vlines k j))
      (define end (get-char vlines k (+ j (- width 1))))
      (define result
        (not (or (char-whitespace? start)  (char-whitespace? end))))
        (for ([l (range width)])
          (erase-char! vlines k (+ l j)))
        result)))

(define (soylint lines)
  (define string-width 
    (foldl (lambda (x y)
      (max (string-length x) y)) 0 lines))
  (define cpy-lines 
   (list->vector
     (map (lambda (x) 
       (~a (apply string (string->list x)) 
           #:min-width (+ string-width 1))) 
       lines)))
  (for/and ([str cpy-lines]
            [i (range (vector-length cpy-lines))])
    (for/and ([char str]
              [j (range (string-length str))])
      (if (char-whitespace? char)
        #t
        (char-tofu?! cpy-lines i j)))))

