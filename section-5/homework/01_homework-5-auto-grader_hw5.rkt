;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist xs)
  (if (null? xs)
      (aunit)
      (apair (car xs) (racketlist->mupllist (cdr xs)))))

;; (define (mupllist->racketlist xs)
;;   (if (isaunit xs)
;;       null
;;       (cons (fst xs) (mupllist->racketlist (snd xs)))))


(define (mupllist->racketlist xs)
  (if (aunit? xs)
      null
      (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here

        [(int? e) e]

        [(fun? e) (closure env e)]

        [(ifgreater? e)
         (let ((v1 (eval-under-env (ifgreater-e1 e) env))
               (v2 (eval-under-env (ifgreater-e2 e) env)))
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL greater applied to non-number")))]

        [(mlet? e)
         (let ((v (mlet-var e))
               (value (eval-under-env (mlet-e e) env)))
           (if (string? v)
               (eval-under-env (mlet-body e) (cons (cons v value) env))
               (error "MUPL mlet applied to non-string")))]

        [(call? e)
         (let ((v1 (eval-under-env (call-funexp e) env))
               (v2 (eval-under-env (call-actual e) env)))
           (if (closure? v1)
               (let* ((v3 (closure-fun v1))
                      (bodyenv (cons (cons (fun-formal v3) v2) (closure-env v1)))
                      (bodyenv (if (fun-nameopt v3)
                                   ;; should bind funname to closure but also fun
                                   (cons (cons (fun-nameopt v3) v1) bodyenv)
                                   bodyenv)))
                 (eval-under-env (fun-body v3) bodyenv))
               (error "MUPL call applied to non-closure")))]

        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env)
                (eval-under-env (apair-e2 e) env))]

        [(fst? e)
         (let ((v1 (eval-under-env (fst-e e) env)))
           (if (apair? v1)
               (apair-e1 v1)
               (error "MUPL fst appliedto non-pair")))]

        [(snd? e)
         (let ((v1 (eval-under-env (snd-e e) env)))
           (if (apair? v1)
               (apair-e2 v1)
               (error "MUPL snd appliedto non-pair")))]


        [(isaunit? e)
         (let ((v1 (eval-under-env (isaunit-e e) env)))
           (if (aunit? v1)
               (int 1)
               (int 0)))]

        [(closure? e) e]

        [(aunit? e) e]

        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))
;; (define (ifaunit e1 e2 e3)
;;   (if (eq? (int 1) (isaunit e1))
;;       e2
;;       e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst))
            (cdr (car lstlst))
            (mlet* (cdr lstlst) e2))))

;;  here is eq not greater
(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y")
                         e4
                         (ifgreater (var "_y") (var "_x")
                                    e4
                                    e3)))))

;; Problem 4

(define mupl-map
  (fun "map" "x"
       (fun #f "xs"
            ;; here not call (fst (var "xs")) because (aunit) is not apair
            (ifaunit (var "xs")
                     (aunit)
                     (apair (call (var "x") (fst (var "xs")))
                            (call (call (var "map") (var "x"))
                                  (snd (var "xs"))))))))


(define mupl-mapAddN
  (mlet "map" mupl-map
        (fun #f "x"
             ;; can be call mupl-map
             (call (var "map")
                   (fun #f "y" (add (var "x") (var "y")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; ;; We will test this function directly, so it must do
;; ;; as described in the assignment
(define (compute-free-vars e)
  (define (f e)
    (cond ((var? e)
           (cons e (set (var-string e))))
          ((add? e)
           (cons e (set-union (cdr (f (add-e1 e))) (cdr (f (add-e2 e))))))
          ((ifgreater? e)
           (cons e (set-union (cdr (f (ifgreater-e1 e))) (cdr (f (ifgreater-e2 e))) (cdr (f (ifgreater-e3 e))) (cdr (f (ifgreater-e4 e))))))
          ((mlet? e)
           (let ((all-set (cdr (f (mlet-body e))))
                 (none-free (mlet-var e))
                 ;; add mlet-e
                 (free-e (cdr (f (mlet-e e)))))
             (cons e (set-union free-e (set-remove all-set none-free)))))
          ((fun? e)
           (let* ((all-set (cdr (f (fun-body e))))
                  (none-free (fun-formal e))
                  ;; remove fun name var
                  (free-vars (if (fun-nameopt e)
                                 (set-remove (set-remove all-set none-free) (fun-nameopt e))
                                 (set-remove all-set none-free))))
             (cons
              (fun-challenge (fun-nameopt e) (fun-formal e) (fun-body e)
                             free-vars)
              free-vars)))
          ((call? e)
           (cons e (set-union
                    (cdr (f (call-funexp e)))
                    ;; union actual
                    (cdr (f (call-actual e))))))

          ((apair? e)
           (cons e (set-union (cdr (f (apair-e1 e))) (cdr (f (apair-e2 e))))))
          ((fst? e)
           (cons e (cdr (f (fst-e e)))))
          ((snd? e)
           (cons e (cdr (f (snd-e e)))))
          ((isaunit? e)
           (cons e (cdr (f (isaunit-e e)))))
          ((closure? e)
           (cons e (cdr (f (closure-fun e)))))
          (#t (cons e (set)))
          ))
  (car (f e)))

;; ;; Do NOT share code with eval-under-env because that will make
;; ;; auto-grading and peer assessment more difficult, so
;; ;; copy most of your interpreter here and make minor changes
;; ;; (define (eval-under-env-c e env) "CHANGE")

(define (eval-under-env-c e env)
  ;; add a case
  (cond [(fun-challenge? e)
         (closure (set-map (fun-challenge-freevars e)
                           (lambda (s) (cons s (envlookup env s)))) e)]
        [(var? e)
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here

        [(int? e) e]

        [(fun? e) (closure env e)]

        [(ifgreater? e)
         (let ((v1 (eval-under-env-c (ifgreater-e1 e) env))
               (v2 (eval-under-env-c (ifgreater-e2 e) env)))
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL greater applied to non-number")))]

        [(mlet? e)
         (let ((v (mlet-var e))
               (value (eval-under-env-c (mlet-e e) env)))
           (if (string? v)
               (eval-under-env-c (mlet-body e) (cons (cons v value) env))
               (error "MUPL mlet applied to non-string")))]

        [(call? e)
         (let ((v1 (eval-under-env-c (call-funexp e) env))
               (v2 (eval-under-env-c (call-actual e) env)))
           (if (closure? v1)
               (let* ((v3 (closure-fun v1))
                      (bodyenv (cons (cons (fun-challenge-formal v3) v2) (closure-env v1)))
                      (bodyenv (if (fun-challenge-nameopt v3)
                                   ;; should bind funname to closure but also fun
                                   (cons (cons (fun-challenge-nameopt v3) v1) bodyenv)
                                   bodyenv)))
                 (eval-under-env-c (fun-challenge-body v3) bodyenv))
               (error "MUPL call applied to non-closure")))]

        [(apair? e)
         (apair (eval-under-env-c (apair-e1 e) env)
                (eval-under-env-c (apair-e2 e) env))]

        [(fst? e)
         (let ((v1 (eval-under-env-c (fst-e e) env)))
           (if (apair? v1)
               (apair-e1 v1)
               (error "MUPL fst appliedto non-pair")))]

        [(snd? e)
         (let ((v1 (eval-under-env-c (snd-e e) env)))
           (if (apair? v1)
               (apair-e2 v1)
               (error "MUPL snd appliedto non-pair")))]


        [(isaunit? e)
         (let ((v1 (eval-under-env-c (isaunit-e e) env)))
           (if (aunit? v1)
               (int 1)
               (int 0)))]

        [(closure? e) e]

        [(aunit? e) e]

        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
