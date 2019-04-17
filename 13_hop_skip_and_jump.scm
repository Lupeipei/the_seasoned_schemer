; Chapter 13 Hop, Skip, and Jump
;
; define member? from chapter 02
;
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a) (member? a (cdr lat)))))))
;
; define intersect from chapter 07
;
(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))
;
; rewrite intersect with letrec
;
(define intersect
  (lambda (set1 set2)
    (letrec
      ((I (lambda (set)
            (cond
              ((null? set) (quote ()))
              ((member? (car set) set2) (cons (car set) (I (cdr set))))
              (else (I (cdr set)))))))
      (I set1))))
;
; test
(intersect (quote (tomatoes and macaroni)) (quote (macaroni and cheese)))
;Value : (and macaroni)

;
; define intersectall from chapter 07
;
(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectall (cdr l-set)))))))
;
; examples of intersectall
;
(intersectall (quote ((6 pears and) (3 peaches and 6 peppers) (8 pears and 6 plums) (and 6 prunes with some apples))))
;Value : (6 and)

; redefine intersectall with letrec
;
(define intersectall
  (lambda (lset)
    (letrec
      ((A (lambda (lset)
            (cond
              ((null? (cdr lset)) (car lset))
              (else (intersect (car lset) (A (cdr lset))))))))
      (cond
        ((null? lset) (quote ()))
        (else (A lset))))))
;
; examples of intersectall
;
(intersectall (quote ((3 mangoes and) (3 kiwis and) (3 hamburgers))))
;Value : (3)

(intersectall (quote ((3 steaks and) (no food and) (three baked potatoes) (3 diet hamburgers))))
;Value : ()

(intersectall (quote ((3 mangoes and) () (3 diet hamburgers))))
;Value : ()

; redefine intersectall with letcc

; Alonzo Church's version
;
(define intersectall
  (lambda (lset)
    (call-with-current-continuation
      (lambda (hop)
      (letrec
        ((A (lambda (lset)
              (cond
                ((null? (car lset)) (hop (quote ())))
                ((null? (cdr lset)) (car lset))
                (else (intersect (car lset) (A (cdr lset))))))))
      (cond
        ((null? lset) (quote ()))
        (else (A lset))))))))
;
; test
(intersectall (quote ((3 steaks and) (no food and) (three baked potatoes) (3 diet hamburgers))))
;Value : ()

; --------------------------------------------------------------------------------.
; ; the Fourteen commandment                                                      ;
; ; Use (letcc ...) to return values abruptly and promptly.                       ;
; ---------------------------------------------------------------------------------.

; new version of intersect
;
(define intersect
  (lambda (set1 set2)
    (letrec
      ((I (lambda (set)
            (cond
              ((null? set) (quote ()))
              ((member? (car set) set2) (cons (car set) (I (cdr set))))
              (else (I (cdr set)))))))
      (cond
        ((null? set2) (quote ()))
        (else (I set1))))))
;
; rewrite intersectall with new version of intersect
;
(define intersectall
  (lambda (lset)
    (call-with-current-continuation
      (lambda (hop)
      (letrec
        ((A (lambda (lset)
              (cond
                ((null? (car lset)) (hop (quote ())))
                ((null? (cdr lset)) (car lset))
                (else (I (car lset) (A (cdr lset)))))))
          (I (lambda (set1 set2)
               (letrec
                 ((J (lambda (set)
                       (cond
                         ((null? set) (quote ()))
                         ((member? (car set) set2) (cons (car set) (J (cdr set))))
                         (else (J (cdr set)))))))
                  (cond
                    ((null? set2) (hop (quote ())))
                    (else (J set1)))))))
      (cond
        ((null? lset) (quote ()))
        (else (A lset))))))))
;
; test
(intersectall (quote ((3 steaks and) (no food and) (three baked potatoes) (3 diet hamburgers))))
;Value : ()

; rewrite rember with letrec
;
(define rember
  (lambda (a lat)
    (letrec
      ((R (lambda (lat)
            (cond
              ((null? lat) (quote ()))
              ((eq? a (car lat)) (cdr lat))
              (else (cons (car lat) (R (cdr lat))))))))
      (R lat))))
;
;
; define rember-beyond-first
;
(define rember-beyond-first
  (lambda (a lat)
    (letrec
      ((R (lambda (lat)
            (cond
              ((null? lat) (quote ()))
              ((eq? a (car lat)) (quote ()))
              (else (cons (car lat) (R (cdr lat))))))))
      (R lat))))
;
; examples of rember-beyond-first
;
(rember-beyond-first
  (quote roots)
  (quote (noodles
          spaghetti spatzle bean-thread
          roots
          potatoes yam
          others
          rice)))
;
;Value : (noodles spaghetti spatzle bean-thread)
;
(rember-beyond-first
  (quote others)
  (quote (noodles
          spaghetti spatzle bean-thread
          roots
          potatoes yam
          others
          rice)))
;
;Value : (noodles spaghetti spatzle bean-thread roots potatoes yam)
;
(rember-beyond-first
  (quote sweetthing)
  (quote (noodles
          spaghetti spatzle bean-thread
          roots
          potatoes yam
          others
          rice)))
;
;Value : (noodles spaghetti spatzle bean-thread roots potatoes yam others rice)
;
(rember-beyond-first
  (quote desserts)
  (quote (cookies
          chocolate mints
          caramel delight ginger snaps
          desserts
          chocolate mousse
          vanilla ice cream
          German chocolate cake
          more desserts
          gingerbreadman chocolate
          chip brownies)))
;
;Value : (cookies chocolate mints caramel delight ginger snaps)
;
; define rember-upto-last
;
(define rember-upto-last
  (lambda (a lat)
    (call-with-current-continuation
      (lambda (skip)
        (letrec
          ((R (lambda (lat)
                (cond
                  ((null? lat) (quote ()))
                  ((eq? a (car lat)) (skip (R (cdr lat))))
                  (else (cons (car lat) (R (cdr lat))))))))
          (R lat))))))
;
; examples of rember-upto-last
;
(rember-upto-last
  (quote roots)
  (quote (noodles
          spaghetti spatzle bean-thread
          roots
          potatoes yam
          others
          rice)))
;
;Value : (potatoes yam others rice)
;
(rember-upto-last
  (quote sweetthing)
  (quote (noodles
          spaghetti spatzle bean-thread
          roots
          potatoes yam
          others
          rice)))
;
;Value : (noodles spaghetti spatzle bean-thread roots potatoes yam others rice)
;
(rember-upto-last
  (quote cookies)
  (quote (cookies
          chocolate mints
          caramel delight ginger snaps
          desserts
          chocolate mousse
          vanilla ice cream
          German chocolate cake
          more cookies
          gingerbreadman chocolate
          chip brownies)))
;
;Value : (gingerbreadman chocolate chip brownies)
