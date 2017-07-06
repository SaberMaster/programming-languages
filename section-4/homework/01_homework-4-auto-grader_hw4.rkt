#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))


;; (define (string-append-map xs suffix)
;;   (if (null? xs)
;;       null
;;       (cons (string-append (car xs) suffix) (string-append-map (cdr xs) suffix))))


(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

(define (list-nth-mod xs n)
  (cond ((< n 0) (error "list-nth-mod: negative number"))
        ((null? xs) (error "list-nth-mod: empty list"))
        (#t (car (list-tail xs (remainder n (length xs)))))))

;; (define (stream-for-n-steps s n)
;;   (if (< n 1)
;;       null
;;       (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define (stream-for-n-steps s n)
  (if (< n 1)
      null
      (let ((tmp (s)))
        (cons (car tmp) (stream-for-n-steps (cdr tmp) (- n 1))))))

(define funny-number-stream
  (letrec ((f (lambda (x) (cons (if (= 0 (remainder x 5))
                                    (- x)
                                    x)
                                (lambda () (f (+ x 1)))))))
    (lambda () (f 1))))

;; with no arg cat use parentheses wrap the func name
;; (define funny-number-stream
;;   (letrec ((f (lambda (x) (cons x (lambda () (f (+ x 1)))))))
;;     (lambda () (f 1))))

(define dan-then-dog
  (letrec ((f (lambda (x y) (cons x (lambda () (f y x))))))
    (lambda () (f "dan.jpg" "dog.jpg"))))

(define (stream-add-zero s)
  (lambda () (let ((tmp (s)))
               (cons (cons 0 (car tmp)) (stream-add-zero (cdr tmp))))))

(define (cycle-lists xs ys)
  (letrec ((f (lambda (i)
                (cons (cons (list-nth-mod xs i) (list-nth-mod ys i))
                      (lambda () (f (+ i 1)))))))
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ((len (vector-length vec))
           (f (lambda (i) (if (< i len)
                              (let ((ith (vector-ref vec i)))
                                (if (and (cons? ith) (equal? v (car ith)))
                                    ith
                                    (f (+ i 1))))
                              #f))))
    (f 0)))

(define (cached-assoc xs n)
  (letrec ((cache-vector (make-vector n #f))
           (cache-index 0))
    (lambda (v)
      (or (vector-assoc v cache-vector)
          (let ((rst (assoc v xs)))
            (and rst
                (begin
                  (vector-set! cache-vector cache-index rst)
                  (set! cache-index (remainder (+ cache-index 1) n))
                  rst)))))))

;; here let var name can equal with the params, so e1 change to e_1
(define-syntax while-less
  (syntax-rules (do)
    ((while-less e1 do e2)
     (let ((e_1 e1))
       (letrec ((f (lambda () (let ((e_2 e2))
                                (if (or (not (number? e_2)) (< e_2 e_1))
                                    (f)
                                    #t)))))
         (f))))))
