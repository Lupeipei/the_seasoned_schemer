; Chapter 17 We change, Therefore, we are !
;
; define sub1 from chapter 04, we will use it later.
;
(define sub1
  (lambda (n)
    (- n 1)))
;
; define add1 from chapter 04, we will use it later.
;
(define add1
  (lambda (n)
    (+ n 1)))
;
; define atom? from chapter 02, we will use it later.
;
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
;
; define eqan? from chapter 04, we will use it later.
;
(define eqan?
  (lambda (a b)
    (cond
      ((and (number? a) (number? b)) (= a b))
      ((or (number? a) (number? b)) #f)
      (else (eq? a b)))))
;
; define equal? from chapter 05, we will use it later.
;
(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))
;
; rewrite eqlist? using equal? from chapter 05, we will use it later.
;
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
      )))
;
; define find from chapter 16
;
(define find
  (lambda (n Ns Rs)
    (letrec
      ((A (lambda (ns rs)
            (cond
              ((null? ns) #f)
              ((= n (car ns)) (car rs))
              (else (A (cdr ns) (cdr rs)))))))
        (A Ns Rs))))
;
; redefine deep with if
;
(define deep
  (lambda (m)
    (if (zero? m)
        (quote pizza)
        (cons (deep (sub1 m)) (quote ())))))
;
; redefine deepM with the new version of deep
;
(define deepM
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (letrec
      ((D (lambda (m)
            (if (zero? m)
                (quote pizza)
                (cons (D (sub1 m)) (quote ()))))))
        (lambda (n)
          (let ((exists (find n Ns Rs)))
            (if (atom? exists)
                (let ((result (D n)))
                  (set! Rs (cons result Rs))
                  (set! Ns (cons n Ns))
                  result)
                  exists))))))
;
; examples of deepM
;
(deepM 2)
;Value : ((pizza))

; replace (D (sub1 m)) with (deepM (sub1 m)), and using (let ....) to simplify again.
;
; redefine deepM with the new version of deep
;
(define deepM
  (let ((Rs (quote ()))
        (Ns (quote ()))
        (D (lambda (m)
              (if (zero? m)
                  (quote pizza)
                  (cons (deepM (sub1 m)) (quote ()))))))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (D n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))
;
; replace D with it's definition.
;
(define deepM
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result ((lambda (m)
                            (if (zero? m)
                                (quote pizza)
                                (cons (deepM (sub1 m)) (quote ())))) n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))
;
; examples of deepM
;
(deepM 2)
;Value : ((pizza))

; replace ((lambda (m) ....) n) with (let ((m n)) (lambda ....))
;
;
(define deepM
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (let ((m n))
                            (if (zero? m)
                                (quote pizza)
                                (cons (deepM (sub1 m)) (quote ()))))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))
;
;
; again, remove (let ((m n)) ....)
;
;
(define deepM
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (if (zero? n)
                              (quote pizza)
                              (cons (deepM (sub1 n)) (quote ())))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))
;
; examples of deepM
;
(deepM 2)
;Value : ((pizza))
;
; define consC to count the times on using cons.
;
(define consC
  (let ((N 0))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))
;
; redefine deep with consC
;
;
(define deep
  (lambda (m)
    (if (zero? m)
        (quote pizza)
        (consC (deep (sub1 m)) (quote ())))))
;
; define counter
;
(define counter)

; redefine consC with counter
;
(define consC
  (let ((N 0))
    (set! counter
      (lambda ()
        N))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))
;
(deep 5)
;Value : (((((pizza)))))

(counter)
;Value : 5

(deep 7)
;Value : (((((((pizza)))))))

(counter)
;Value: 12

; define supercounter
;
(define supercounter
  (lambda (f)
    (letrec
      ((S (lambda (n)
            (if (zero? n)
                (f n)
                (let ()
                  (f n)
                  (S (sub1 n)))))))
        (S 1000)
        (counter))))
;
; exmaples of supercounter
;
(supercounter deep)
;Value: 500512

; define set-counter
;
(define set-counter)

; redefine consC again.
;
(define consC
  (let ((N 0))
    (set! counter
      (lambda ()
        N))
    (set! set-counter
      (lambda (x)
        (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))
;
; reset N to 0
(set-counter 0)

; recalculate supercounter again
;
(supercounter deep)
;Value: 500500

; redefine deepM with consC
;
(define deepM
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (if (zero? n)
                              (quote pizza)
                              (consC (deepM (sub1 n)) (quote ())))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))
;
;

(deepM 5)
;Value : (((((pizza)))))

(counter)
;Value: 500505

(set-counter 0)

(deepM 5)
;Value : (((((pizza)))))

; cause we already have (((((pizza))))) in Rs, so the counter is zero.
;
(counter)
;Value: 0

(deepM 6)
;Value : ((((((pizza))))))

(counter)
;Value: 1

(deep 7)
;Value : (((((((pizza)))))))

(counter)
;Value: 8

(set-counter 0)

; cause deepM 6, deepM 5..... deepM 1 already in Rs
(supercounter deepM)
;Value: 994

; we can use set-Rs-Ns to reset Rs and Ns to empty list
;
(define set-Rs-Ns)

; redefine deepM with set-Rs-Ns
;
(define deepM
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (set! set-Rs-Ns
      (lambda ()
        (set! Rs (quote ()))
        (set! Ns (quote ()))))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (if (zero? n)
                              (quote pizza)
                              (consC (deepM (sub1 n)) (quote ())))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))
;
(set-counter 0)

(set-Rs-Ns)

; and now, counter should be 1000

(supercounter deepM)
;Value: 1000

; define rember1* from chapter 14
;
(define rember1*
  (lambda (a l)
    (letrec
      ((R (lambda (l oh)
              (cond
                ((null? l) (oh (quote no)))
                ((atom? (car l))
                  (if (eq? a (car l))
                      (cdr l)
                      (cons (car l) (R (cdr l) oh))))
                (else
                  (let ((new-car (call-with-current-continuation
                                    (lambda (oh)
                                      (R (car l) oh)))))
                    (if (atom? new-car)
                        (cons (car l) (R (cdr l) oh))
                        (cons new-car (cdr l)))))))))

        (if (atom? (call-with-current-continuation
                      (lambda (oh)
                        (R l oh))))
              l
              (R l (quote ()))))))
;
; redefine rember1* with consC
;
(define rember1*
  (lambda (a l)
    (letrec
      ((R (lambda (l oh)
              (cond
                ((null? l) (oh (quote no)))
                ((atom? (car l))
                  (if (eq? a (car l))
                      (cdr l)
                      (consC (car l) (R (cdr l) oh))))
                (else
                  (let ((new-car (call-with-current-continuation
                                    (lambda (oh)
                                      (R (car l) oh)))))
                    (if (atom? new-car)
                        (consC (car l) (R (cdr l) oh))
                        (consC new-car (cdr l)))))))))

        (if (atom? (call-with-current-continuation
                      (lambda (oh)
                        (R l oh))))
              l
              (R l (quote ()))))))
;
(set-counter 0)

; examples of rember1*
;
(rember1* (quote noodles) (quote ((food) more (food))))
;Value : ((food) more (food))

(counter)
;Value: 0

; using let with rember1* from chapter 14.
;
(define rember1*
  (lambda (a l)
    (letrec
      ((R (lambda (l)
            (cond
              ((null? l) (quote ()))
              ((atom? (car l))
                (cond
                  ((eq? a (car l)) (cdr l))
                  (else (cons (car l) (R (cdr l))))))
              (else
                (let ((av (R (car l))))
                  (if (eqlist? av (car l))
                      (cons (car l) (R (cdr l)))
                      (cons av (cdr l)))))))))
          (R l))))
;
; rewrite the function with consC and named it rember1*C2
;
(define rember1*C2
  (lambda (a l)
    (letrec
      ((R (lambda (l)
            (cond
              ((null? l) (quote ()))
              ((atom? (car l))
                (cond
                  ((eq? a (car l)) (cdr l))
                  (else (consC (car l) (R (cdr l))))))
              (else
                (let ((av (R (car l))))
                  (if (eqlist? av (car l))
                      (consC (car l) (R (cdr l)))
                      (consC av (cdr l)))))))))
          (R l))))
;
; examples of rember1*C2
;
(rember1*C2 (quote noodles) (quote ((food) more (food))))
;Value : ((food) more (food))

(counter)
;Value: 5

; ok, time for lunch.
