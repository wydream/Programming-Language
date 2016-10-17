#lang racket
;Problem 1
(define (sequence low high stride)
  (if (< high low) null
  (if (<= (+ low stride) high)
      (cons low (sequence (+ low stride) high stride))
      (cons low null))))

;Problem 2
(define (string-append-map xs suffix)
  (map (lambda(str) (string-append str suffix)) xs))

;Problem 3
(define (list-nth-mod xs n)
  (if (< n 0) (error "list-nth-mod: negative number")
      (if (null? xs) (error "list-nth-mod: empty list")
          (let ([i (remainder n (length xs))])
            (car (list-tail xs i))))))

;Problem 4
(define (stream-for-n-steps s n)
  (if (= 0 n)
      null
      (let ([x (car (s))])
        (if (>= 0 n) null (cons x (stream-for-n-steps (cdr (s)) (- n 1)))))))

;Problem 5
(define funny-number-stream
  (letrec ([f (lambda (x) (cons x (lambda () (f (if (= 0 (remainder (+ x 1) 5)) (* -1 (+ x 1)) (+ (abs x) 1) )))))])
    (lambda () (f 1))))

;Problem 6
(define dan-then-dog
  (letrec ([f (lambda (x) (cons x (lambda () (f (if (string=? x "dan.jpg") "dog.jpg" "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

;Problem 7
(define (stream-add-zero s)
  (letrec ([f (lambda (x) (cons (cons 0 (car (x))) (lambda () (f (cdr (x))))))])
    (lambda () (f s))))

;Problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

;Problem 9
(define (vector-assoc v vec)
  (letrec ([f (lambda (pos)
                (if (= pos (vector-length vec)) #f
                    (if (pair? (vector-ref vec pos))
                        (if (equal? v (car (vector-ref vec pos))) (vector-ref vec pos)
                            (f (+ pos 1)))
                        (f (+ pos 1)))))])
    (f 0)))

;Problem 10
(define (cached-assoc xs n)
  (letrec ([pos 0]
           [cache (make-vector n #f)]
           [f (lambda(v)
              (let ([ans (vector-assoc v cache)])
                (if ans ans
                    (let ([new-ans (assoc v xs)])
                      (if new-ans
                      (begin
                        (vector-set! cache pos new-ans)
                        (set! pos (remainder (+ pos 1) n))
                                     new-ans)
                      #f)))))])
    f))

;Problem 11
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([_e1 e1])
     (letrec ([loop (lambda ()
                      (if (<= _e1 e2)
                          #t
                          (loop)))])
       (loop)))]))
