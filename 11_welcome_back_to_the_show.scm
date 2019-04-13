; Chapter 11 Welcome back to the show
;
; define atom? from chapter 02
;
(define atom?
  (lambda (l)
    (and (not (pair? l)) (not (null? l)))))
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
(member? (quote sardines) (quote (Italian sardines spaghetti parsley)))
;Value: #t

; define is-first?
;
(define is-first?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (eq? (car lat) a)))))
;
; define two-in-a-row with is-first?
;
(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (or (is-first? (car lat) (cdr lat)) (two-in-a-row? (cdr lat)))))))
;
; examples of two-in-a-row?
;
(two-in-a-row? (quote (Italian sardines spaghetti parsley)))
;Value: #f

(two-in-a-row? (quote (Italian sardines sardines spaghetti parsley)))
;Value: #t

(two-in-a-row? (quote (Italian sardines more sardines spaghetti)))
;Value: #f

; revised version of two-in-a-row?
;
(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else
        (is-first-b? (car lat) (cdr lat))))))
;
; define is-first-b?
;
(define is-first-b?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a) (two-in-a-row? lat))))))
;
; define two-in-a-row-b?
;
(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) preceding) (two-in-a-row-b? (car lat) (cdr lat)))))))
;
; redefine two-in-a-row?
;
(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (two-in-a-row-b? (car lat) (cdr lat))))))
;

; examples of redefine two-in-a-row?
;
(two-in-a-row? (quote (b d e i i a g)))
;Value: #t

; define sum-of-prefixes
;
(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))
;
; define sum-of-prefixes-b
;
(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond
      ((null? tup) (quote ()))
      (else
        (cons (+ (car tup) sonssf)
                  (sum-of-prefixes-b (+ (car tup) sonssf) (cdr tup)))))))
;
; examples of sum-of-prefixes
;
(sum-of-prefixes (quote (2 1 9 17 0)))
;Value : (2 3 12 29 29)

(sum-of-prefixes (quote (1 1 1 1 1)))
;Value : (1 2 3 4 5)

; --------------------------------------------------------------------------------.
; ; the eleventh commandment                                                      ;
; ; Use additional arguments when a function needs to know what other arguments   ;
; ; to the function have been like so far                                         ;
; ---------------------------------------------------------------------------------.

;
; define sub1 from chapter 04
;
(define sub1
  (lambda (n)
    (- n 1)))
;
; define pick from chapter 04
;
(define pick
  (lambda (n lat)
    (cond
      ((null? lat) (quote ()))
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
;
; define scramble
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
; examples of scramble
;
(scramble (quote (1 1 1 3 4 2 1 1 9 2)))
;Value : (1 1 1 1 1 4 1 1 1 9)

(scramble (quote (1 2 3 4 5 6 7 8 9)))
;Value : (1 1 1 1 1 1 1 1 1)

(scramble (quote (1 2 3 1 2 3 4 1 8 2 10)))
;Value : (1 1 1 1 1 1 1 1 2 8 2)
