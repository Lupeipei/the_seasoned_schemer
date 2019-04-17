; Chapter 12 Take Cover
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
; define multirember from chapter 03
;
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))
;
; Y combinator from chapter 09
;
(define Y
  (lambda (le)
    ((lambda (f)
      (f f))
      (lambda (f)
        (le (lambda (x)
              ((f f) x)))))))
;
; redefine multirember with Y combinator
;
(define multirember
  (lambda (a lat)
    ((Y (lambda (mr)
          (lambda (lat)
            (cond
              ((null? lat) (quote ()))
              ((eq? a (car lat)) (mr (cdr lat)))
              (else (cons (car lat) (mr (cdr lat)))))))) lat)))
;
; test this new definination.
; examples of multirember
;
(multirember (quote tuna) (quote (shrimp salad tuna salad and tuna)))
;Value : (shrimp salad salad and)

; using letrec to define multirember
;
(define multirember
  (lambda (a lat)
    ((letrec
      ((mr (lambda (lat)
              (cond
                ((null? lat) (quote ()))
                ((eq? a (car lat)) (mr (cdr lat)))
                (else (cons (car lat) (mr (cdr lat)))))))) mr)
                lat)))
;
; here, (letrec ((mr ....) mr)) defines and returns a recursive function
; ((letrec ((mr ....) mr)) lat) applying the mr function to lat
;
; test this new definination.
; examples of multirember
;
(multirember (quote tuna) (quote (shrimp salad tuna salad and tuna)))
;Value : (shrimp salad salad and)

; define multirember again
;
(define multirember
  (lambda (a lat)
    (letrec
      ((mr (lambda (lat)
              (cond
                ((null? lat) (quote ()))
                ((eq? a (car lat)) (mr (cdr lat)))
                (else (cons (car lat) (mr (cdr lat))))))))
                (mr lat))))
; this time using (letrec ((mr ....)) (mr lat))
; it just actions as ((letrec ((mr ....) mr)) lat)
;
; test this new definination.
; examples of multirember
;
(multirember (quote tuna) (quote (shrimp salad tuna salad and tuna)))
;Value : (shrimp salad salad and)

; --------------------------------------------------------------------------------.
; ; the Twelfth commandment                                                      ;
; ; Use (letrec ...) to remove arguments that do not change for recursive        ;
; ; applications                                                                 ;
; ---------------------------------------------------------------------------------.
;

; define rember-f from chapter 08
;
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) a) (cdr l))
        (else (cons (car l) ((rember-f test?) a (cdr l))))))))
;
; we can define rember-eq? with rember-f
;
(define rember-eq? (rember-f (quote eq?)))
;
; define multirember-f
;
(define multirember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) a) ((multirember-f test?) a (cdr l)))
        (else (cons (car l) ((multirember-f test?) a (cdr l))))))))
;
; redefine multirember-f with letrec
;
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (letrec
        ((m-f
          (lambda (a lat)
            (cond
              ((null? lat) (quote ()))
              ((test? a (car lat)) (m-f a (cdr lat)))
              (else (cons (car lat) (m-f a (cdr lat))))))))
            m-f))))
;
; (letrec ((m-f ....) m-f)) defines and returns a recursive function
;
; define multirember with multirember-f when test? is eq?
;
(define multirember
  (letrec
    ((mr
      (lambda (a lat)
        (cond
          ((null? lat) (quote ()))
          ((eq? (car lat) a) (mr a (cdr lat)))
          (else (cons (car lat) (mr a (cdr lat)))))))) mr))
;
; test this new definination.
; examples of multirember
;
(multirember (quote salad) (quote (shrimp salad tuna salad and tuna)))
;Value : (shrimp tuna and tuna)

; replace mr with multirember
;
(define multirember
  (letrec
    ((multirember
      (lambda (a lat)
        (cond
          ((null? lat) (quote ()))
          ((eq? (car lat) a) (multirember a (cdr lat)))
          (else (cons (car lat) (multirember a (cdr lat)))))))) multirember))
;
; elimiate (letrec ....)
;

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))
;

; define member? from chapter 02
;
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a) (member? a (cdr lat)))))))
;
; examples of member?
;
(member? (quote ice) (quote (salad greens with pears brie cheese frozen yogurt)))
;Value: #f

;
; redefine member? with letrec
;
(define member?
  (lambda (a lat)
    ((letrec
      ((yes?
        (lambda (l)
          (cond
            ((null? l) #f)
            (else (or (eq? a (car l)) (yes? (cdr l))))))))
        yes?)
      lat)))
;
; test
(member? (quote ice) (quote (salad greens with pears brie cheese frozen yogurt)))
;Value: #f

;
; alternative definination
;
(define member?
  (lambda (a lat)
    (letrec
      ((yes?
        (lambda (l)
          (cond
            ((null? l) #f)
            (else (or (eq? a (car l)) (yes? (cdr l))))))))
        (yes? lat))))
;
; test
(member? (quote ice) (quote (salad greens with pears brie cheese frozen yogurt)))
;Value: #f

;
; define union from chapter 07
;
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))
;
; examples of union
;
(union (quote (tomatoes and macaroni casserole)) (quote (macaroni and cheese)))
;Value : (tomatoes casserole macaroni and cheese)

;
; using letrec to rewrite union
;
(define union
  (lambda (set1 set2)
    (letrec
      ((U
        (lambda (set)
          (cond
            ((null? set) set2)
            ((member? (car set) set2) (U (cdr set)))
            (else (cons (car set) (U (cdr set))))))))
          (U set1))))
;
; examples of union
;
(union (quote (tomatoes and macaroni casserole)) (quote (macaroni and cheese)))
;Value : (tomatoes casserole macaroni and cheese)

; define member? in union
;
(define union
  (lambda (set1 set2)
    (letrec
      ((U
        (lambda (set)
          (cond
            ((null? set) set2)
            ((member? (car set) set2) (U (cdr set)))
            (else (cons (car set) (U (cdr set)))))))
        (member?
          (lambda (a lat)
          (cond
            ((null? lat) #f)
            (else (or (eq? a (car lat)) (member? a (cdr lat))))))))
        (U set1))))
;
; examples of union
;
(union (quote (tomatoes and macaroni casserole)) (quote (macaroni and cheese)))
;Value : (tomatoes casserole macaroni and cheese)


; --------------------------------------------------------------------------------.
; ; the Thirteen commandment                                                      ;
; ; Use (letrec ...) to hide and to protect functions.                            ;
; ---------------------------------------------------------------------------------.

; replace member? with M in the definination of union.
;
(define union
  (lambda (set1 set2)
    (letrec
      ((U
        (lambda (set)
          (cond
            ((null? set) set2)
            ((M? (car set) set2) (U (cdr set)))
            (else (cons (car set) (U (cdr set)))))))
        (M?
          (lambda (a lat)
          (cond
            ((null? lat) #f)
            (else (or (eq? a (car lat)) (M? a (cdr lat))))))))
        (U set1))))
;
; refactor union so M? not ignores the Twelfth commandment.
;
(define union
  (lambda (set1 set2)
    (letrec
      ((U
        (lambda (set)
          (cond
            ((null? set) set2)
            ((M? (car set) set2) (U (cdr set)))
            (else (cons (car set) (U (cdr set)))))))
        (M?
          (lambda (a lat)
            (letrec
              ((N?
                (lambda (l)
                  (cond
                    ((null? l) #f)
                    (else (or (eq? a (car l)) (N? (cdr lat))))))))
                (N? lat)))))
        (U set1))))
;
; we define two-in-a-row? with two-in-a-row-b?
; define two-in-a-row-b? from chapter 11
;
(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) preceding) (two-in-a-row-b? (car lat) (cdr lat)))))))
;
; redefine two-in-a-row? from chapter 11
;
(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (two-in-a-row-b? (car lat) (cdr lat))))))
;
; now we use letrec to hide two-in-a-row-b?
;
(define two-in-a-row?
  (lambda (lat)
    (letrec
      ((W (lambda (a lat)
            (cond
              ((null? lat) #f)
              (else (or (eq? (car lat) a) (W (car lat) (cdr lat))))))))
      (cond
        ((null? lat) #f)
        (else (W (car lat) (cdr lat)))))))
;
; test
;
(two-in-a-row? (quote (b d e i i a g)))
; Value: #t

; alternative
;
(define two-in-a-row?
    (letrec
      ((W (lambda (a lat)
            (cond
              ((null? lat) #f)
              (else (or (eq? (car lat) a) (W (car lat) (cdr lat))))))))
      (lambda (lat)
        (cond
          ((null? lat) #f)
          (else (W (car lat) (cdr lat)))))))
;
; test
;
(two-in-a-row? (quote (b d e i i a g)))
; Value: #t

; define sum-of-prefixes  from chapter 11
;
(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))
;
; define sum-of-prefixes-b  from chapter 11
;
(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond
      ((null? tup) (quote ()))
      (else
        (cons (+ (car tup) sonssf)
                  (sum-of-prefixes-b (+ (car tup) sonssf) (cdr tup)))))))
;
; protect sum-of-prefixes-b
;
(define sum-of-prefixes
  (lambda (tup)
    (letrec
      ((S (lambda (sonssf tup)
            (cond
              ((null? tup) (quote ()))
              (else
                (cons (+ (car tup) sonssf)
                         (S (+ (car tup) sonssf) (cdr tup))))))))
      (S 0 tup))))
;
; test
(sum-of-prefixes (quote (2 1 9 17 0)))
;Value : (2 3 12 29 29)

;
; alternative
;
(define sum-of-prefixes
  (letrec
    ((S (lambda (sonssf tup)
          (cond
            ((null? tup) (quote ()))
            (else
              (cons (+ (car tup) sonssf)
                       (S (+ (car tup) sonssf) (cdr tup))))))))
    (lambda (tup)
      (S 0 tup))))
;
; test
(sum-of-prefixes (quote (2 1 9 17 0)))
;Value : (2 3 12 29 29)

;
; define scramble  from chapter 11
;
(define scramble
  (lambda (tup)
    (scramble-b tup (quote ()))))
;
; define scramble-b
;
(define scramble-b
  (lambda (tup rev-pre)
    (cond
      ((null? tup) (quote ()))
      (else
        (cons (pick (car tup) (cons (car tup) rev-pre))
              (scramble-b (cdr tup) (cons (car tup) rev-pre)))))))
;
; protect scramble-b
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
; test
(scramble (quote (1 2 3 4 5 6 7 8 9)))
;Value : (1 1 1 1 1 1 1 1 1)

; alternative
;
(define scramble
  (letrec
    ((P (lambda (tup rp)
          (cond
            ((null? tup) (quote ()))
            (else
              (cons (pick (car tup) (cons (car tup) rp))
                    (P (cdr tup) (cons (car tup) rp))))))))
    (lambda (tup)
      (P tup (quote ())))))
;
; test
(scramble (quote (1 2 3 4 5 6 7 8 9)))
;Value : (1 1 1 1 1 1 1 1 1)
