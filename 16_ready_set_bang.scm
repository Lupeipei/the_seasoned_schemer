; Chapter 16 ready, set, bang!
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
; define member? from chapter 02, we will use it later.
;
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a) (member? a (cdr lat)))))))
;
; define atom? from chapter 02, we will use it later.
;
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
;
; define max from chapter 14, we will use it later.
;
(define max
  (lambda (n m)
    (if (> n m) n m)))
;

; define sweet-tooth
;
(define sweet-tooth
  (lambda (food)
    (cons food
      (cons (quote cake)
        (quote ())))))
;
; define last
(define last (quote angelfood))
;
; examples of sweet-tooth
;
(sweet-tooth (quote chocolate))
;Value : (chocolate cake)

(sweet-tooth (quote fruit))
;Value : (fruit cake)

((lambda () last))
;Value: angelfood

; define sweet-toothL
;
(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food
      (cons (quote cake)
        (quote ())))))
;
(sweet-toothL (quote chocolate))
;Value : (chocolate cake)

((lambda () last))
;Value: chocolate

(sweet-toothL (quote fruit))
;Value : (fruit cake)

((lambda () last))
;Value: fruit

(sweet-toothL (quote cheese))
; Value : (cheese cake)

(sweet-toothL (quote carrot))
;Value : (carrot cake)

; using ingredients to track all the ingredients that went into sweet-toothL
(define ingredients (quote ()))

; define sweet-toothR
(define sweet-toothR
  (lambda (food)
    (set! ingredients
      (cons food ingredients))
    (cons food
      (cons (quote cake)
        (quote ())))))
;
; examples of sweet-toothR
;
(sweet-toothR (quote chocolate))
; Value : (chocolate cake)

((lambda () ingredients))
;Value : (chocolate)

(sweet-toothR (quote fruit))
;Value : (fruit cake)

((lambda () ingredients))
;Value : (fruit chocolate)

(sweet-toothR (quote cheese))
;Value : (cheese cake)

((lambda () ingredients))
;Value : (cheese fruit chocolate)

(sweet-toothR (quote carrot))
;Value : (carrot cake)

((lambda () ingredients))
;Value : (carrot cheese fruit chocolate)

; define deep
;
(define deep
  (lambda (m)
    (cond
      ((zero? m) (quote pizza))
      (else (cons (deep (sub1 m)) (quote ()))))))
;
; examples of deep
;
(deep 3)
;Value : (((pizza)))

(deep 7)
;Value : (((((((pizza)))))))

(deep 0)
;Value: pizza

; define Rs, Ns, deepR to remember all the deep results and corresponding numbers
;
(define Rs (quote ()))
(define Ns (quote ()))
;
; define deepR
;
(define deepR
  (lambda (m)
    (let ((result (deep m)))
      (set! Ns (cons m Ns))
      (set! Rs (cons result Rs))
      result)))
;
; examples of deepR
;
(deepR 3)
;Value : (((pizza)))

(deepR 5)
;Value : (((((pizza)))))

((lambda () Rs))
;Value : ((((((pizza))))) (((pizza))))

((lambda () Ns))
;Value : (5 3)

(deepR 3)
;Value : (((pizza)))


((lambda () Rs))
;Value : ((((pizza))) (((((pizza))))) (((pizza))))

((lambda () Ns))
;Value : (3 5 3)

; --------------------------------------------------------------------------------.
; ; the Nineteenth commandment                                                   ;
; ; Use (set! ...) to remember valuable things between two distinct uses of a    ;
; ; function.                                                                    ;
; ---------------------------------------------------------------------------------.
;
; define find
;
(define find
  (lambda (n Ns Rs)
    (letrec
      ((A (lambda (ns rs)
            (cond
              ((= n (car ns)) (car rs))
              (else (A (cdr ns) (cdr rs)))))))
        (A Ns Rs))))
;
; exmaples of find
;
(find 3 Ns Rs)
;Value : (((pizza)))

; define deepM
;
(define deepM
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (deepR n))))
;
; reset Ns, Rs
(set! Ns (cdr Ns))
(set! Rs (cdr Rs))

((lambda () Rs))
;Value : ((((((pizza))))) (((pizza))))

((lambda () Ns))
;Value : (5 3)

; redefine deepM with deepR
;
(define deepM
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (let ((result (deep n)))
          (set! Ns (cons n Ns))
          (set! Rs (cons result Rs))
          result))))
;
(deepM 6)
;Value : ((((((pizza))))))
;

; redefine deep with deepM
;
(define deep
  (lambda (m)
    (cond
      ((zero? m) (quote pizza))
      (else (cons (deepM (sub1 m)) (quote ()))))))
;
(deepM 9)
;Value : (((((((((pizza)))))))))

((lambda () Ns))
;Value : (9 8 7 6 5 3)

; redefine deepM to obey the Sixteenth commandment
;
(define deepM
  (let ((Rs (quote ()))
        (Ns (quote ())))
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (let ((result (deep n)))
          (set! Ns (cons n Ns))
          (set! Rs (cons result Rs))
          result)))))
;
; examples of deepM
;
(deepM 16)
;Value : ((((((((((((((((pizza))))))))))))))))

; we know the Rs, Ns in deepM changed, but Rs, Ns outside deepM haven't changed
((lambda () Rs))
;Value : ((((((((((pizza))))))))) ((((((((pizza)))))))) (((((((pizza))))))) ((((((pizza)))))) (((((pizza))))) (((pizza))))

((lambda () Ns))
;Value : (9 8 7 6 5 3)

; refefine find so when Ns and Rs is empty list.
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
; redefine deepM with this new find
;
(define deepM
  (let ((Rs (quote ()))
        (Ns (quote ())))
  (lambda (n)
    (let ((exists (find n Ns Rs)))
      (if (atom? exists)
          (let ((result (deep n)))
            (set! Ns (cons n Ns))
            (set! Rs (cons result Rs))
            result)
            exists)))))
;
; examples of deepM
;
(deepM 2)
;Value : ((pizza))

; need a cup of Coke.
; we meet length again.
;
(define length
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
;
;
(define length (lambda (l) 0))

(set! length
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
;
; using let and set! to get another version of length
;
(define length
  (let ((h (lambda (l) 0)))
    (set! h
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (h (cdr l)))))))
    h))
;
; examples of this length
;
(length (quote (1)))
;Value: 1

(length (quote (1 2 3)))
; Value: 3

; --------------------------------------------------------------------------------.
; ; the Seventeenth commandment                                                   ;
; ;  (final version)                                                              ;
; ; Use (set! ...) for (let ((x ...))) only if there is at least one (lambda ..)  ;
; ; between it and the  (let ((x ...))) or if the new value for x is a function   ;
; ; refers to x.                                                                  ;
; ---------------------------------------------------------------------------------.

; define L
;
(define L
  (lambda (lengthL)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (lengthL (cdr l))))))))
;
; using L to redefine length
;
(define length
  (let ((h (lambda (l) 0)))
    (set! h
      (L (lambda (arg) (h arg))))
    h))
;
; just the same as before.

;
; examples of this length

(length (quote (1 2 3 4)))
;Value: 4

; define Yi
(define Yi
  (lambda (L)
    (let ((h (lambda (l) 0)))
      (set! h
        (L (lambda (arg) (h arg))))
      h)))
;
; we get Y-bang
;
(define Y-bang
  (lambda (f)
    (letrec
      ((h (f (lambda (arg) (h arg)))))
      h)))
;
(define length (Yi L))

(length (quote (1 2 3)))
;Value: 3

; we can also define length with Y

; Y combinator from chapter 09, old friends.
;
(define Y
  (lambda (le)
    ((lambda (f)
      (f f))
      (lambda (f)
        (le (lambda (x)
              ((f f) x)))))))
;

(define length (Y L))

(length (quote (1 2 3)))
;Value: 3
;

; define D
;
(define D
  (lambda (depth*)
    (lambda (s)
      (cond
        ((null? s) 1)
        ((atom? (car s)) (depth* (cdr s)))
        (else
          (max
            (add1 (depth* (car s)))
            (depth* (cdr s))))))))
;
(define depth* (Yi D))

; examples of depth*

(depth* (quote (c (b (a b) a) a)))
;Value: 3

(define depth* (Y-bang D))

(depth* (quote (c (b (a b) a) a)))
;Value: 3

(define depth* (Y D))

(depth* (quote (c (b (a b) a) a)))
;Value: 3

; difference between Yi and Y
;
; define biz
;

(define biz
  (let ((x 0))
    (lambda (f)
      (set! x (add1 x))
      (lambda (a)
        (if (= a x)
            0
            (f a))))))
;
; examples of biz
;
((Y biz) 5)
; Value: 0

((Yi biz) 5)
; no answer

((Y-bang biz) 5)
; no answer

; we know the x in (Yi biz) will always be 1. so ((Y biz) 1) must be equal ((Yi biz) 1), which is 0.
((Y biz) 1)
;Value: 0

((Yi biz) 1)
;Value: 0

((Y-bang biz) 1)
;Value: 0

; we can change biz so that ((Yi biz) 5) will have value.

; redefine biz
;
(define biz
  (lambda (f)
    (let ((x 0))
      (lambda (a)
        (set! x (add1 x))
        (if (= a x)
            0
            (f a))))))
;
; examples of biz

((Yi biz) 5)
;Value: 0

((Y-bang biz) 5)
;Value: 0

; enjoy!
