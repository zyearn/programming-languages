#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence l h s)
  (if (> l h)
      null
      (cons l (sequence (+ l s) h s))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(= (length xs) 0) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (letrec ([f (lambda (s n)
               (cond [(= n 0) null]
                     [#t (cons (car (s)) (f (cdr (s)) (- n 1)))]))])
    (f s n)))

(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= (remainder x 5) 0) (- 0 x) x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) (cons x (lambda () (f (if (equal? x "dan.jpg") "dog.jpg" "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

(define (stream-add-zero s)
  (letrec ([f (lambda (x) (cons (cons 0 (car (x))) (lambda () (f (cdr (x))))))])
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x) (cons (cons (list-nth-mod xs x) (list-nth-mod ys x)) (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
              (cond [(= n (vector-length vec)) #f]
                    [(pair? (vector-ref vec n)) (if (equal? v (car (vector-ref vec n))) (vector-ref vec n) (f (+ n 1)))]
                    [#t (f (+ n 1))]))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [slot 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v memo)])
                  (if ans
                      ans
                      (let ([new-ans (assoc v xs)])
                        (if new-ans
                            (begin (vector-set! memo slot new-ans)
                                   (set! slot (remainder (+ slot 1) n))
                                   new-ans)
                            #f)))))])
    f))
