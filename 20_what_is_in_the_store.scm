; Chapter 20 What's in the store?
;
; finally, we arrive here, the last chapter of this book.
;
; define sub1 from chapter 04, we will use it later.
;
(define sub1
  (lambda (n)
    (- n 1)))
; define lookup
;
(define lookup
  (lambda (table name)
    (table name)))
;
; define extend
;
(define extend
  (lambda (name1 value table)
    (lambda (name2)
      (cond
        ((eq? name2 name1) value)
        (else (table name2))))))
;
;
; define define?
;
(define define?
  (lambda (a)
    (cond
      ((atom? e) #f)
      ((atom? (car e))
       (eq? (car e) (quote define)))
      (else #f))))
;
; define the-empty-table
;
(define the-empty-table
  (lambda (name)
    (car (quote ()))))
;
; define global-table
;
(define global-table the-empty-table)
;
; define *define
;
(define *define
  (lambda (e)
    (set! global-table
      (extend
        (name-of e)
        (box
          (the-meaning
            (right-side-of e)))
         global-table))))
;
; define box
;
(define box
  (lambda (it)
    (lambda (sel)
      (sel it (lambda (new)
                (set! it new))))))
;
; define setbox
;
(define setbox
  (lambda (box new)
    (box (lambda (it set) (set new)))))
;
;
; define unbox
;
(define unbox
  (lambda (box)
    (box (lambda (it set) it))))
;
; define lookup-in-global-table
;
(define lookup-in-global-table
  (lambda (name)
    (lookup global-table name)))
;
; define the-meaning
;
(define the-meaning
  (lambda (e)
    (meaning e lookup-in-global-table)))
;
;
; define value from chapter 10
;
(define value
  (lambda (e)
    (meaning e (quote ()))))
;
; define meaning from chapter 10
;
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))
;

;
; define *quote from chapter 10
;
(define *quote
  (lambda (e table)
    (text-of e)))
;
; define *identifier
;
(define *identifier
  (lambda (e table)
    (unbox (lookup table e))))
;
; define set*
;
(define set*
  (lambda (e table)
    (setbox
      (lookup table (name-of e))
      (meaning (right-side-of e) table))))
;
;
; define *lambda
;
(define *lambda
  (lambda (e table)
    (lambda (args)
      (beglis (body-of e)
        (multi-extend
          (formals-of e)
          (box-all args)
          table)))))
;
; and here is beglis and box-all
;
; define beglis
;
(define beglis
  (lambda (es table)
    (cond
      ((null? (cdr es))
       (meaning (car es) table))
      (else ((lambda (val)
                (beglis (cdr es) table))
              (meaning (car es) table))))))
;
; here, I wonder whether it is ok that we can remove the expression of (meaning (car es) table) as it has never been used ?
;
; define box-all
;
(define box-all
  (lambda (vals)
    (cond
      ((null? vals) (quote ()))
      (else (cons (box (car vals))
              (box-all (cdr vals)))))))
;
; define multi-extend
;
(define multi-extend
  (lambda (names values table)
    (cond
      ((null? names) table)
      (else
        (extend (car names) (car values)
          multi-extend (cdr names) (cdr values) table)))))
;
; define odd?
;
(define odd?
  (lambda (n)
    (cond
      ((zero? n) #f)
      (else even? (sub1 n)))))
;
; define even?
;
(define even?
  (lambda (n)
    (cond
      ((zero? n) #t)
      (else (odd? (sub1 n))))))
;
;
; define *application
;
(define *application
  (lambda (e table)
    ((meaning (function-of e) table)
     (evlis (arguments-of e) table))))

; define evlis, a little different from chapter 10
;
(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote ()))
      (else
        ((lambda (val)
          (cons val
             (evlis (cdr args) table)))
          (meaning (car args) table))))))
;
;
; define :car
(define :car
  (lambda (args-in-a-list)
    (car (car args-in-a-list))))
;
; define a-prim for actions like :car that take one argument
;
(define a-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)))))
;
; define b-prim for actions like cons that take two arguments
;
(define b-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)
         (car (cdr (args-in-a-list)))))))
;
; rewrite *const with let
;
(define *const
  (let ((:cons (b-prim cons))
        (:car (a-prim car))
        (:cdr (a-prim cdr))
        (:null? (a-prim null?))
        (:eq? (b-prim eq?))
        (:atom? (a-prim atom?))
        (:zero? (a-prim zero?))
        (:add1 (a-prim add1))
        (:sub1 (a-prim sub1))
        (:number? (a-prim number?))
      (lambda (e table)
        (cond
          ((number? e) e)
          ((eq? e #t) #t)
          ((eq? e #f) #f)
          ((eq? e (quote cons)) :cons)
          ((eq? e (quote car)) :car)
          ((eq? e (quote cdr)) :cdr)
          ((eq? e (quote null?)) :null?)
          ((eq? e (quote eq?)) :eq?)
          ((eq? e (quote atom?)) :atom?)
          ((eq? e (quote zero?)) :zero?)
          ((eq? e (quote add1)) :add1)
          ((eq? e (quote sub1)) :sub1)
          ((eq? e (quote number?)) :number?)))))
;
; also you can define *const without let
;
(define *const
  ((lambda (:cons :car :cdr :null? :eq? :atom? :zero? :add1 :sub1 :number?)
    (lambda (e table)
      (cond
        ((number? e) e)
        ((eq? e #t) #t)
        ((eq? e #f) #f)
        ((eq? e (quote cons)) :cons)
        ((eq? e (quote car)) :car)
        ((eq? e (quote cdr)) :cdr)
        ((eq? e (quote null?)) :null?)
        ((eq? e (quote eq?)) :eq?)
        ((eq? e (quote atom?)) :atom?)
        ((eq? e (quote zero?)) :zero?)
        ((eq? e (quote add1)) :add1)
        ((eq? e (quote sub1)) :sub1)
        ((eq? e (quote number?)) :number?))))
    (b-prim cons)
    (a-prim car)
    (a-prim cdr)
    (a-prim null?)
    (b-prim eq?)
    (a-prim atom?)
    (a-prim zero?)
    (a-prim add1)
    (a-prim sub1)
    (a-prim number?)
    ))

; --------------------------------------------------------------------------------.
; ; the Fifteenth commandment                                                     ;
; ;  (final version)                                                              ;
; ; Use (let ...) to name the values of repeated expressions in a function        ;
; ; definition if they may be evaluated twice for one and the same use of the     ;
; ; function. And use (let ...) to name the values of expressions (without set!)  ;
; ; that are re-evaluated every time a function is used.                          ;
; ---------------------------------------------------------------------------------.

; define *cond from chapter 10
;
(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))
;
; define evcon from chapter 10
;
(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))
;
; now, with letcc
;
(define *letcc
  (lambda (e table)
    (call-with-current-continuation
      (lambda (skip)
        (beglis (ccbody-of e)
          (extend
            (name-of e)
            (box (a-prim skip))
            table))))))
;
; define abort
;
(define abort)
;
; the final version of value
;
(define value
  (lambda (e)
    (call-with-current-continuation
      (lambda (the-end)
        (set! abort the-end)
        (cond
          ((define? e) (*define e))
          (else (the-meaning e)))))))
;
; redefine the-empty-table
;
(define the-empty-table
  (lambda (name)
    (abort
      (cons (quote no-answer)
        (cons name (quote ()))))))
;
; define expression-to-action
;
(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))
;
; define atom-to-action
;
(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote member?)) *const)
      (else *identifier))))
;
;
; define list-to-action
;
(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
        (cond
          ((eq? (car e) (quote quote)) *quote)
          ((eq? (car e) (quote lambda)) *lambda)
          ((eq? (car e) (quote cond)) *cond)
          ((eq? (car e) (quote letcc)) *letcc)
          ((eq? (car e) (quote set!)) *set)
          (else *application)))
      (else *application))))
;
; definitions of text-of, formals-of, body-of....etc
;
(define text-of
  (lambda (x)
    (car (cdr x))))
;
(define formals-of text-of)

(define body-of
  (lambda (x)
    (cdr (cdr x))))
;
(define ccbody-of body-of)
;
(define name-of text-of)
;
(define right-side-of
  (lambda (x)
    (cond
      ((null? (cdr (cdr x))) 0)
      (else (car (cdr (cdr x)))))))
;
(define cond-lines-of cdr)
;
(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x (quote else)))
      (else #f))))
;
(define question-of car)
;
(define answer-of text-of)
;
(define function-of car)
;
(define arguments-of cdr)
;
(value 1)
