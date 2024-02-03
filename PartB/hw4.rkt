#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (define (helper ls high_list)
    (if (> (+ high_list stride) high)
        ls
        (helper (append ls (list (+ high_list stride))) (+ high_list stride))))
  (if (> low high) null (helper (list low) low)))

(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

(define (list-nth-mod xs n)
  (if (< n 0) (error "list-nth-mod: negative number")
      (if (null? xs) (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))

(define (stream-for-n-steps s n)
  (define (helper nth st acc)
    (if (= nth n) acc
        (helper (+ nth 1) (cdr (st)) (append acc (list (car (st)))))))
  (helper 0 s '() ))

(define funny-number-stream
  (letrec ((helper (lambda (n)
    (if (= 0 (remainder n 5)) (lambda () (cons (- 0 n) (helper (+ n 1))))
        (lambda () (cons n (helper (+ n 1))))))))
  (helper 1)))

(define dan-then-dog
  (letrec ((f (lambda () (cons "dan.jpg" g)))
           (g (lambda () (cons "dog.jpg" f))))
    f))

(define (stream-add-zero stream)
  (lambda () (cons (cons 0 (car (stream)))
                  (stream-add-zero (cdr (stream))))))

(define (cycle-lists xs ys)
  (letrec ((helper (lambda (n)
                     (lambda ()
                       (cons (cons (list-nth-mod xs n)
                                   (list-nth-mod ys n))
                             (helper (+ n 1)))))))
    (helper 0)))

(define (vector-assoc v vec)
  (define (helper n)
      (if (= n (vector-length vec)) #f
          (if (null? (vector-ref vec n)) (helper (+ n 1))
              (if (null? (cdr (vector-ref vec n))) (helper (+ n 1))
                  (if (= v (car (vector-ref vec n))) (vector-ref vec n)
                      (helper (+ n 1)))))))
  (helper 0))

(define (cached-assoc xs n)
  (define cache (make-vector n #f))
  (define pos 0)
  (lambda (v)
    (let ((vec-search (vector-assoc v cache)))
      (if vec-search
          vec-search
          (begin
          (let ((list-search (assoc v xs)))
            (if list-search
                (begin
                  (vector-set! cache pos list-search)
                  (if (= pos (- n 1))
                      (set! pos 0)
                      (set! pos (+ pos 1)))
                  list-search)
                #f)))))))
                
          