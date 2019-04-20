; Chapter 14 Let there be games
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
; define add1 from chapter 04, we will use it later.
;
(define add1
  (lambda (n)
    (+ n 1)))
;
; define sub1 from chapter 04, we will use it later.
;
(define sub1
  (lambda (n)
    (- n 1)))
;
; define pick from chapter 04, we will use it later.
;
(define pick
  (lambda (n lat)
    (cond
      ((null? lat) (quote ()))
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
;
; define leftmost from chapter 05
;
(define leftmost
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))
;
; examples of leftmost
;
(leftmost (quote (((a) b) (c d))))
;Value: a

(leftmost (quote (((a) ()) () (e))))
;Value: a

;
; redefine leftmost
;
(define leftmost
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (car l))
      (else
        (cond
          ((atom? (leftmost (car l))) (leftmost (car l)))
          (else (leftmost (cdr l))))))))
;
; examples of leftmost
;
(leftmost (quote (((a) b) (c d))))
;Value: a

(leftmost (quote (((a) ()) () (e))))
;Value: a

(leftmost (quote (((() a) ()))))
;Value: a

;
; using let for leftmost
;
(define leftmost
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (car l))
      (else
        (let ((a (leftmost (car l))))
          (cond
            ((atom? a) a)
            (else (leftmost (cdr l)))))))))
;
; examples of leftmost
(leftmost (quote (((() a) ()))))
;Value: a

; define rember1*
;
(define rember1*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
        (cond
          ((eq? a (car l)) (cdr l))
          (else (cons (car l) (rember1* a (cdr l))))))
      (else
        (cond
          ((eqlist?
             (rember1* a (car l))
             (car l))
            (cons (car l) (rember1* a (cdr l))))
          (else (cons (rember1* a (car l)) (cdr l))))))))
;
; examples of rember1*
;
(rember1* (quote salad) (quote ((Swedish rye) (French (mustard salad turkey) salad))))
; Value : ((swedish rye) (french (mustard turkey) salad))

(rember1* (quote meat) (quote ((pasta meat) pasta (noodles meat sauce) meat tomatoes)))
;Value : ((pasta) pasta (noodles meat sauce) meat tomatoes)


; fixed rember1* using the Twelfth commandment
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
                (cond
                  ((eqlist?
                      (R (car l))
                      (car l))
                    (cons (car l) (R (cdr l))))
                  (else
                    (cons (R (car l)) (cdr l)))))))))
          (R l))))
;
; examples of rember1* with retcc
;
(rember1* (quote meat) (quote ((pasta meat) pasta (noodles meat sauce) meat tomatoes)))
;Value : ((pasta) pasta (noodles meat sauce) meat tomatoes)

; using let with rember1*
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
                (cond
                  ((eqlist? av (car l))
                   (cons (car l) (R (cdr l))))
                  (else
                    (cons av (cdr l))))))))))
          (R l))))
;
; --------------------------------------------------------------------------------.
; ; the Fifteenth commandment                                                     ;
; ;  (preliminary version)                                                        ;
; ; Use (let ...) to name the values of repeated expressions.                     ;
; ---------------------------------------------------------------------------------.
;

; define depth*
;
(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth* (cdr l)))
      (else
        (cond
          ((> (depth* (cdr l))
              (add1 (depth* (car l))))
            (depth* (cdr l)))
          (else (add1 (depth* (car l)))))))))
;
; examples of depth*
;
(depth* (quote ((pickled) peppers (peppers pickled))))
;Value: 2

(depth* (quote (margarine
                ((bitter butter)
                (makes)
                (batter (bitter)))
                butter)))
;Value: 4

(depth* (quote (c (b (a b) a) a)))
;Value: 3

; redefine depth* with let
;
(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth* (cdr l)))
      (else
        (let ((a (add1 (depth* (car l))))
              (d (depth* (cdr l))))
          (cond
            ((> d a)
              d)
            (else a)))))))
;
; examples of depth*
;
(depth* (quote (c (b (a b) a) a)))
;Value: 3

; --------------------------------------------------------------------------------.
; ; the Fifteenth commandment                                                     ;
; ;  (revised version)                                                            ;
; ; Use (let ...) to name the values of repeated expressions in a function        ;
; ; definination if they may be evaluated twice for one and the same use of the   ;
; ; function.                                                                     ;
; ---------------------------------------------------------------------------------.
;
;
; more enjoyable version of depth*
;
(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth* (cdr l)))
      (else
        (let ((a (add1 (depth* (car l))))
              (d (depth* (cdr l))))
          (if (> d a) d a))))))
;
; examples of depth*
;
(depth* (quote (c (b (a b) a) a)))
;Value: 3
;
; define max
;
(define max
  (lambda (n m)
    (if (> n m) n m)))
;
; define depth* with max
;
(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth* (cdr l)))
      (else
        (let ((a (add1 (depth* (car l))))
              (d (depth* (cdr l))))
          (max a d))))))
;
; alternative version
;
(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth* (cdr l)))
      (else
        (max (add1 (depth* (car l))) (depth* (cdr l)))))))
;
; examples of depth*
;
(depth* (quote (c (b (a b) a) a)))
;Value: 3

; more examples with letting
;
; redefine scramble from chapter 12
;
(define scramble
  (lambda (tup)
    (letrec
      ((P (lambda (tup rp)
            (cond
              ((null? tup) (quote ()))
              (else
                (cons (pick (car tup) (cons (car tup) rp))
                      (P (cdr tup) (cons (car tup) rp))))))))
      (P tup (quote ())))))
;
; redefine it again with let
;
(define scramble
  (lambda (tup)
    (letrec
      ((P (lambda (tup rp)
            (cond
              ((null? tup) (quote ()))
              (else
                (let ((rp (cons (car tup) rp)))
                  (cons (pick (car tup) rp)
                        (P (cdr tup) rp))))))))
      (P tup (quote ())))))
;
; examples
(scramble (quote (1 2 3 4 5 6 7 8 9)))
;Value : (1 1 1 1 1 1 1 1 1)

;
; redefine leftmost with letcc
;
(define leftmost
  (lambda (l)
    (call-with-current-continuation
      (lambda (skip)
        (lm l skip)))))
;
; define lm
;
(define lm
  (lambda (l out)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (out (car l)))
      (else (let ()
              (lm (car l) out)
              (lm (cdr l) out))))))
;
; examples for new version leftmost
;
(leftmost (quote (((a)) b (c))))
;Value: a

; hide lm with letrec
;
(define leftmost
  (lambda (l)
    (letrec
      ((lm (lambda (l out)
              (cond
                ((null? l) (quote ()))
                ((atom? (car l)) (out (car l)))
                (else (let ()
                        (lm (car l) out)
                        (lm (cdr l) out)))))))
      (call-with-current-continuation
        (lambda (skip)
          (lm l skip))))))
;
; remove (letrec.....) into the value part of (letcc....)
;
(define leftmost
  (lambda (l)
      (call-with-current-continuation
        (lambda (skip)
          (letrec
            ((lm (lambda (l out)
                    (cond
                      ((null? l) (quote ()))
                      ((atom? (car l)) (out (car l)))
                      (else (let ()
                              (lm (car l) out)
                              (lm (cdr l) out)))))))
              (lm l skip))))))
;
; examples for new version leftmost
;
(leftmost (quote (((a)) b (c))))
;Value: a

; remove argument out in lm.
;
(define leftmost
  (lambda (l)
      (call-with-current-continuation
        (lambda (skip)
          (letrec
            ((lm (lambda (l)
                    (cond
                      ((null? l) (quote ()))
                      ((atom? (car l)) (skip (car l)))
                      (else (let ()
                              (lm (car l))
                              (lm (cdr l))))))))
              (lm l))))))
;
;
; redefine rember1* with letcc
;
(define rember1*
  (lambda (a l)
    (if (atom? (call-with-current-continuation
                  (lambda (oh)
                    (rm a l oh))))
          l
          (rm a l (quote ())))))
;
;
; define rm
;
(define rm
  (lambda (a l oh)
    (cond
      ((null? l) (oh (quote no)))
      ((atom? (car l))
        (if (eq? a (car l))
            (cdr l)
            (cons (car l) (rm a (cdr l) oh))))
      (else
        (let ((new-car (call-with-current-continuation
                          (lambda (oh)
                            (rm a (car l) oh)))))
          (if (atom? new-car)
              (cons (car l) (rm a (cdr l) oh))
              (cons new-car (cdr l))))))))
;
; examples of rember1* with letcc
;
(rember1* (quote meat) (quote ((pasta meat) pasta (noodles meat sauce) meat tomatoes)))
;Value : ((pasta) pasta (noodles meat sauce) meat tomatoes)

; hide rm
;
(define rember1*
  (lambda (a l)
    (letrec
      ((rm (lambda (l oh)
              (cond
                ((null? l) (oh (quote no)))
                ((atom? (car l))
                  (if (eq? a (car l))
                      (cdr l)
                      (cons (car l) (rm (cdr l) oh))))
                (else
                  (let ((new-car (call-with-current-continuation
                                    (lambda (oh)
                                      (rm (car l) oh)))))
                    (if (atom? new-car)
                        (cons (car l) (rm (cdr l) oh))
                        (cons new-car (cdr l)))))))))

        (if (atom? (call-with-current-continuation
                      (lambda (oh)
                        (rm l oh))))
              l
              (rm l (quote ()))))))
;
;
; examples of rember1* with letcc
;
(rember1* (quote meat) (quote ((pasta meat) pasta (noodles meat sauce) meat tomatoes)))
;Value : ((pasta) pasta (noodles meat sauce) meat tomatoes)

; scheme not working with try
; the following codes do not work.
; --------------------------------------------------------------------------------.
; a new version with try
;
(define rember1*
  (lambda (a l)
    (try oh (rm a l oh) l)))
;
; define rm with try
;
(define rm
  (lambda (a l oh)
    (cond
      ((null? l) (oh (quote no)))
      ((atom? (car l))
        (if (eq? a (car l))
            (cdr l)
            (cons (car l) (rm a (cdr l) oh))))
      (else
        (try oh2
          (cons (rm a (car l) oh2) (cdr l))
          (cons (car l) (rm a (cdr l) oh)))))))
;
; --------------------------------------------------------------------------------.
