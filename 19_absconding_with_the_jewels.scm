; Chapter 19 Absconding with the Jewels
;
; define atom? from chapter 02, we will use it later.
;
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
;

; define sub1 from chapter 04, we will use it later.
;
(define sub1
  (lambda (n)
    (- n 1)))
;

; define deep from chapter 16
;
(define deep
  (lambda (m)
    (cond
      ((zero? m) (quote pizza))
      (else (cons (deep (sub1 m)) (quote ()))))))
;
;
; define six-layers
;
(define six-layers
  (lambda (p)
    (cons
      (cons
        (cons
          (cons
            (cons
              (cons p (quote ()))
              (quote ()))
            (quote ()))
          (quote ()))
        (quote ()))
      (quote ()))))
;
;
; define four-layers
;
(define four-layers
  (lambda (p)
    (cons
      (cons
        (cons
          (cons p (quote ()))
          (quote ()))
        (quote ()))
      (quote ()))))
;
; define toppings
(define toppings)

;
; define deepB
;
(define deepB
  (lambda (m)
    (cond
      ((zero? m)
       (call-with-current-continuation
         (lambda (jump)
          (set! toppings jump)
          (quote pizza))))
       (else (cons (deepB (sub1 m)) (quote ()))))))
;
;
; examples of deepB and layers
;
(deepB 6)
;Value: ((((((pizza))))))

(six-layers (quote mozzarella))
;Value: ((((((mozzarella))))))

; examples of toppings
;
(toppings (quote mozzarella))
;Value: ((((((mozzarella))))))

(toppings (quote pizza))
;Value: ((((((pizza))))))

(cons (toppings (quote cake)) (quote ()))
;Value: ((((((cake))))))

(cons
  (cons
    (cons (toppings (quote mozzarella))
      (quote ()))
    (quote ()))
  (quote ()))
;Value: ((((((mozzarella))))))

; as you see, the toppings forgets everything surrounding it and only adds exactly six layers of parentheses.
;
; let's try (deepB 4)
;
(deepB 4)
;Value: ((((pizza))))

; then toppings

(cons
  (cons
    (cons (toppings (quote mozzarella))
      (quote ()))
    (quote ()))
  (quote ()))
;Value: ((((mozzarella))))
;

; --------------------------------------------------------------------------------.
; ; the Twentieth commandment                                                     ;
; ; When thinking about a value created with (letcc....), write down the function ;
; ; that is equivalent but does not forget. Then, when you use it, remember to    ;
; ; forget.                                                                       ;
; ---------------------------------------------------------------------------------.
;

(cons (toppings (quote cake))
  (toppings (quote cake)))
;Value: ((((cake))))
;

(cons (toppings (quote cake))
  (cons (toppings (quote mozzarella))
    (cons (toppings (quote pizza))
      (quote ()))))
;Value: ((((pizza))))
;

; now, smile and say: long time no see. collector.
;
; define deep&co
;
(define deep&co
  (lambda (m k)
    (cond
      ((zero? m) (k (quote pizza)))
      (else
        (deep&co (sub1 m)
          (lambda (x)
            (k (cons x (quote ())))))))))
;
; examples of deep&co
;
(deep&co 0 (lambda (x) x))
;Value: pizza

(deep&co 6 (lambda (x) x))
;Value: ((((((pizza))))))

(deep&co 2 (lambda (x) x))
;Value: ((pizza))

; define two-layers
;
(define two-layers
  (lambda (p)
    (cons
      (cons p (quote ()))
      (quote ()))))
;
; the value of (deep&co 2 (lambda (x) x)) is the same as (two-layers (quote pizza))
; and
; the value of (deep&co 4 (lambda (x) x)) is the same as (four-layers (quote pizza))
; the value of (deep&co 6 (lambda (x) x)) is the same as (six-layers (quote pizza))

; define deep&coB with toppings
;
(define deep&coB
  (lambda (m k)
    (cond
      ((zero? m)
       (let ()
         (set! toppings k)
         (k (quote pizza))))
      (else
        (deep&coB (sub1 m)
          (lambda (x)
            (k (cons x (quote ())))))))))
;

; examples of deep&coB
;
(deep&coB 2 (lambda (x) x))
;Value: ((pizza))

(deep&coB 6 (lambda (x) x))
;Value: ((((((pizza))))))

(deep&coB 4 (lambda (x) x))
;Value: ((((pizza))))

; examples of toppings, now toppings is just like four-layers
;
(cons (toppings (quote cake)) (toppings (quote cake)))
;Value: (((((cake)))) (((cake))))

(cons (toppings (quote cake))
  (cons (toppings (quote mozzarella))
    (cons (toppings (quote pizza))
      (quote ()))))
;Value: (((((cake)))) ((((mozzarella)))) ((((pizza)))))

; define two-in-a-row? from chapter 11
;
(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (two-in-a-row-b? (car lat) (cdr lat))))))
;
; define two-in-a-row-b? from chapter 11
;
(define two-in-a-row-b?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a) (two-in-a-row-b? (car lat) (cdr lat)))))))
;
; define two-in-a-row? from chapter 12
;
(define two-in-a-row?
  (lambda (lat)
    (letrec
      ((W (lambda (a lat)
            (cond
              ((null? lat) #f)
              (else
                (let ((nxt (car lat)))
                  (or (eq? nxt a) (W nxt (cdr lat)))))))))
      (cond
        ((null? lat) #f)
        (else (W (car lat) (cdr lat)))))))
;
; examples of two-in-a-row?
;
(two-in-a-row? (quote (mozzarella cake mozzarella)))
;Value: #f

(two-in-a-row? (quote (mozzarella mozzarella pizza)))
;Value: #t

; define leave
(define leave)
; define walk
;
(define walk
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (leave (car l)))
      (else
        (let ()
          (walk (car l))
          (walk (cdr l)))))))
;
; we have seen something like this in leftmost.
;
; leftmost in chapter 14.
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
; define start-it
;
(define start-it
  (lambda (l)
    (call-with-current-continuation
      (lambda (here)
        (set! leave here)
        (walk l)))))
;
; examples of start-it
;
(start-it (quote ((potato) (chips (chips (with) fish)))))
;Value: potato

; define fill
(define fill)

; define waddle
;
(define waddle
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (let ()
         (call-with-current-continuation
           (lambda (rest)
             (set! fill rest)
             (leave (car l))))
          (waddle (cdr l))))
      (else
        (let ()
          (waddle (car l))
          (waddle (cdr l)))))))
;
; define start-it2
;
(define start-it2
  (lambda (l)
    (call-with-current-continuation
      (lambda (here)
        (set! leave here)
        (waddle l)))))
;
; examples of start-it2
;
(start-it2 (quote ((donuts) (cheerios (cheerios (spaghettios)) donuts))))
;Value: donuts

; define get-next
;
(define get-next
  (lambda (x)
    (call-with-current-continuation
      (lambda (here-again)
        (set! leave here-again)
        (fill (quote go))))))
;
; examples of get-next
;
(get-next (quote (() (cheerios (cheerios (spaghettios)) donuts))))
;Value: cheerios

; define get-first
;
(define get-first
  (lambda (l)
    (call-with-current-continuation
      (lambda (here)
        (set! leave here)
        (waddle l)
        (leave (quote ()))))))
;
; examples of get-first with get-next
;
(get-first (quote (donut)))
;Value: donut

(get-next (quote go))
;Value: ()


(get-first (quote (fish (chips))))
;Value: fish

(get-next (quote go))
;Value: chips

(get-next (quote go))
;Value: ()

(get-first (quote (fish (chips) chips)))
;Value: fish

(get-next (quote go))
;Value: chips

(get-next (quote go))
;Value: chips

; define two-in-a-row*? with get-first and get-next
;
(define two-in-a-row*?
  (lambda (l)
    (let ((fst (get-first l)))
      (if (atom? fst)
          (two-in-a-row-b*? fst)
          #f))))
;
; define two-in-a-row-b*?
;
(define two-in-a-row-b*?
  (lambda (a)
    (let ((n (get-next (quote go))))
      (if (atom? n)
          (or (eq? n a)
              (two-in-a-row-b*? n))
          #f))))
;
; using letrec to rewrite two-in-a-row*
;
(define two-in-a-row*?
  (letrec
    ((T? (lambda (a)
          (let ((n (get-next 0)))
            (if (atom? n)
                (or (eq? n a)
                    (T? n))
                #f))))
      (get-next
        (lambda (x)
          (call-with-current-continuation
            (lambda (here-again)
              (set! leave here-again)
              (fill (quote go))))))
      (fill (lambda (x) x))
      (waddle
        (lambda (l)
          (cond
            ((null? l) (quote ()))
            ((atom? (car l))
             (let ()
               (call-with-current-continuation
                 (lambda (rest)
                   (set! fill rest)
                   (leave (car l))))
                (waddle (cdr l))))
            (else
              (let ()
                (waddle (car l))
                (waddle (cdr l)))))))
        (leave (lambda (x) x)))
        (lambda (l)
          (let ((fst (call-with-current-continuation
                        (lambda (here)
                          (set! leave here)
                          (waddle l)
                          (leave (quote ()))))))
            (if (atom? fst)
                (T? fst)
                #f)))))
;
; examples of two-in-a-row*?
;
(two-in-a-row*? (quote ((mozzarella) (cake) mozzarella)))
;Value: #f

(two-in-a-row*? (quote ((potato) (chips ((with) fish) (fish)))))
;Value: #t

(two-in-a-row*? (quote ((potato) (chips ((with) fish) (chips)))))
;Value: #f

(two-in-a-row*? (quote ((potato) (chips (chips (with) fish)))))
;Value: #t

(two-in-a-row*? (quote (((food) ()) (((food))))))
;Value: #t

; letcc is so cool!
; ok, time for cake.
