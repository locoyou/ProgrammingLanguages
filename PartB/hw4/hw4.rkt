#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (<= n 0)
      null
      (letrec ([x (s)])
        (cons (car x) (stream-for-n-steps (cdr x) (- n 1))))))

(define funny-number-stream
  (letrec ([neg (lambda(x) (if (= 0 (remainder x 5)) (- x) x))]
           [f (lambda(x) (cons (neg x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" dan))])
    dan))

(define (stream-add-zero s)
  (lambda () (letrec ([x (s)])
               (cons (cons 0 (car x)) (stream-add-zero (cdr x))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (if (< n (vector-length vec))
                    (letrec ([x (vector-ref vec n)])
                      (if (and (pair? x) (equal? (car x) v))
                          x
                          (f (+ n 1))))
                    #f))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [index 0])
    (lambda (v)
      (letrec ([vc (vector-assoc v cache)])
        (if vc
            vc
            (letrec ([x (assoc v xs)])
              (if x
                  (begin
                    (vector-set! cache index x)
                    (set! index (remainder (+ 1 index) n))
                    x)
                  #f)))))))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([x e1]
              [f (lambda ()
                   (if (< e2 x)
                       (f)
                       #t))])
       (f))]))