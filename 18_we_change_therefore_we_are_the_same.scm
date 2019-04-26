; Chapter 18 We change, Therefore, we are the same !
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
; define counter from chapter 17, we will use it later.
;
(define counter)

; define set-counter from chapter 17, we will use it later.
;
(define set-counter)

; define consC from chapter 17, we will use it later.
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
; define lots
;
(define lots
  (lambda (m)
    (cond
      ((zero? m) (quote ()))
      (else (cons (quote egg) (lots (sub1 m)))))))
;
; examples of lots
;
(lots 3)
;Value : (egg egg egg)

(lots 5)
;Value : (egg egg egg egg egg)

(lots 12)
;Value : (egg egg egg egg egg egg egg egg egg egg egg egg)

; define lenkth
;
(define lenkth
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (lenkth (cdr l)))))))
;
; examples of lenkth
;
(lenkth (lots 3))
;Value: 3

(lenkth (lots 5))
;Value: 5

(lenkth (lots 15))
;Value: 15

; define add-at-end
;
(define add-at-end
  (lambda (l)
    (cond
      ((null? (cdr l))
       (consC (car l) (cons (quote egg) (quote ()))))
      (else (consC (car l) (add-at-end (cdr l)))))))
;
; examples of add-at-end
;
(add-at-end (lots 3))
;Value : (egg egg egg egg)

(counter)
;Value: 3

; redefine add-at-end with letrec and set-cdr!
;
(define add-at-end-too
  (lambda (l)
    (letrec
      ((A (lambda (ls)
            (cond
              ((null? (cdr ls))
               (set-cdr! ls (cons (quote egg) (quote ()))))
              (else (A (cdr ls)))))))
        (A l))))
;
;
(set-counter 0)
;
; examples of add-at-end
;
(add-at-end-too (lots 3))
;Value : (egg egg egg egg)

(counter)
;Value: 0

; define kons
;
(define kons
  (lambda (kar kdr)
    (lambda (selector)
      (selector kar kdr))))
;
; define kar
;
(define kar
  (lambda (c)
    (c (lambda (a d) a))))
;
; define kdr
;
(define kdr
  (lambda (c)
    (c (lambda (a d) d))))
;
;
;
; examples of kons
((kons (quote egg) (quote ())) cons)
;Value : (egg)

;
; define bons
;
(define bons
  (lambda (kar)
    (let ((kdr (quote ())))
      (lambda (selector)
        (selector
          (lambda (x) (set! kdr x))
          kar
          kdr)))))
;
; define kar
;
(define kar
  (lambda (c)
    (c (lambda (s a d) a))))
;
; define kdr
;
(define kdr
  (lambda (c)
    (c (lambda (s a d) d))))
;
; define set-kdr
;
(define set-kdr
  (lambda (c x)
    ((c (lambda (s a d) s)) x)))
;
; redefine kons with set-kdr adn bons
;
(define kons
  (lambda (a d)
    (let ((c (bons a)))
      (set-kdr c d)
      c)))
;
(kar (kons (quote a) (quote (1 2 3))))
;Value: a

(kdr (kons (quote a) (quote (1 2 3))))
;Value : (1 2 3)

; redefine lots with consC.
;
(define lots
  (lambda (m)
    (cond
      ((zero? m) (quote ()))
      (else (consC (quote egg) (lots (sub1 m)))))))
;
; add-at-end with consC
;
(define add-at-end
  (lambda (l)
    (cond
      ((null? (cdr l))
       (consC (car l) (consC (quote egg) (quote ()))))
      (else (consC (car l) (add-at-end (cdr l)))))))
;
; add-at-end-too with consC
;
(define add-at-end-too
  (lambda (l)
    (letrec
      ((A (lambda (ls)
            (cond
              ((null? (cdr ls))
               (set-cdr! ls (consC (quote egg) (quote ()))))
              (else (A (cdr ls)))))))
        (A l)
        l)))
;
; define dozen
;
(set-counter 0)

(define dozen (lots 12))

((lambda () dozen))
;Value : (egg egg egg egg egg egg egg egg egg egg egg egg)

(counter)
;Value: 12


; define bakers-dozen
;
(define bakers-dozen (add-at-end dozen))
;
((lambda () bakers-dozen))
;Value : (egg egg egg egg egg egg egg egg egg egg egg egg egg)

(counter)
;Value: 25

; define bakers-dozen-too
;
(define bakers-dozen-too
  (add-at-end-too dozen))
;
((lambda () bakers-dozen-too))
;Value : (egg egg egg egg egg egg egg egg egg egg egg egg egg)
;
(counter)
;Value: 26

; define bakers-dozen-again
;
(define bakers-dozen-again
  (add-at-end dozen))
;
(counter)
;Value: 40
;
; define eklist?
;
(define eklist?
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) (null? ls2))
      ((null? ls2) #f)
      (else
        (and (eq? (car ls1) (car ls2))
             (eklist? (cdr ls1) (cdr ls2)))))))
;
; using eklist? to check bakers-dozen and bakers-dozen-too

(eklist? bakers-dozen bakers-dozen-too)
;Value: #t

; when we talk about sameness, what we talk is two konses are the same if changing one changes the other
;
; define same?
;
(define same?
  (lambda (c1 c2)
    (let ((t1 (cdr c1))
          (t2 (cdr c2)))
      (set-cdr! c1 1)
      (set-cdr! c2 2)
      (let ((v (= (cdr c1) (cdr c2))))
        (set-cdr! c1 t1)
        (set-cdr! c2 t2)
        v))))
;
; the book says the bakers-dozen and the bakers-dozen-too are the same, but you know they are not.

(same? bakers-dozen bakers-dozen-too)
;Value: #f


(same? bakers-dozen bakers-dozen-again)
;Value: #f

(same? bakers-dozen-too bakers-dozen-again)
;Value: #f

; define last-kons
;
(define last-kons
  (lambda (ls)
    (cond
      ((null? (cdr ls)) ls)
      (else (last-kons (cdr ls))))))
;
; examples of last-kons
;
(last-kons (quote (hello scheme)))
;Value : (scheme)

(define long (lots 12))

; (set-kdr (last-kons long) long)
; no answer because (last-kons long) is (egg)

(lenkth long)
;Value: 12

; define finite-lenkth
;
(define finite-lenkth
  (lambda (p)
    (call-with-current-continuation
      (lambda (infinite)
        (letrec
          ((C (lambda (p q)
                (cond
                  ((same? p q)
                   (infinite #f))
                  ((null? q) 0)
                  ((null? (cdr q)) 1)
                  (else
                    (+ (C (sl p) (qk q)) 2)))))
              (qk (lambda (x) (cdr (cdr x))))
              (sl (lambda (x) (cdr x))))
            (cond
              ((null? p) 0)
              (else (add1 (C p (cdr p))))))))))
;
(finite-lenkth long)
;Value: 12
