; Chapter 15 The difference between men and boys
;
(define x
  (cons (quote chicago)
    (cons (quote pizza)
      (quote ()))))
;
; x refers to (chicago pizza)
;
(set! x (quote gone))
; now x refers to gone

(set! x (quote skins))
; now x refers to skins

; define gourmet
;
(define gourmet
  (lambda (food)
    (cons food
      (cons x (quote ())))))
;
; examples of gourmet
;
(gourmet (quote onion))
; Value : (onion skins)

(set! x (quote rings))
; now x refers to rings

;
(gourmet (quote onion))
; Value : (onion rings)

;
; define gourmand
;
(define gourmand
  (lambda (food)
    (set! x food)
    (cons food
      (cons x (quote ())))))
;
; examples of gourmand
;
(gourmand (quote potato))
;Value: (potato potato)

(gourmand (quote rice))
;Value: (rice rice)

; define diner
;
(define diner
  (lambda (food)
    (cons (quote milkshake)
      (cons food (quote ())))))
;
; define dinerR
;
(define dinerR
  (lambda (food)
    (set! x food)
    (cons (quote milkshake)
      (cons food (quote ())))))
;
; examples of dinerR
;
(dinerR (quote onion))
; Value : (milkshake onion)

(dinerR (quote pecanpie))
;Value: (milkshake pecanpie)

; define omnivore
;
(define omnivore
  (let ((x (quote minestrone)))
    (lambda (food)
      (set! x food)
      (cons food
        (cons x (quote ()))))))
;
; examples of omnivore
;
(omnivore (quote bouillabaisse))
; Value : (bouillabaisse bouillabaisse)

; --------------------------------------------------------------------------------.
; ; the Sixteenth commandment                                                     ;
; ; Use (set! ...) only with names defined in (let...)s.                          ;
; ---------------------------------------------------------------------------------.
;
; define gobbler, just like omnivore except the function name
; can also using (define gobbler omnivore)
;
(define gobbler
  (let ((x (quote minestrone)))
    (lambda (food)
      (set! x food)
      (cons food
        (cons x (quote ()))))))
;
; examples of gobbler
;
(gobbler (quote gumbo))
;Value: (gumbo gumbo)

; define nibbler
;
(define nibbler
  (lambda (food)
    (let ((x (quote minestrone)))
      (set! x food)
      (cons food
        (cons x (quote ()))))))
;
; examples of nibbler
;
(nibbler (quote cheerio))

; --------------------------------------------------------------------------------.
; ; the Seventeenth commandment                                                   ;
; ;  (preliminary version)                                                        ;
; ; Use (set! ...) for (let ((x ...))) only if there is at least one (lambda ..)  ;
; ; between it and the  (let ((x ...))).                                          ;
; ---------------------------------------------------------------------------------.

; define food
;
(define food (quote none))

; define glutton
;
(define glutton
  (lambda (x)
    (set! food x)
      (cons (quote more)
        (cons x
          (cons (quote more)
            (cons x
              (quote ())))))))
;
; examples of glutton
;
(glutton (quote garlic))
;Value: (more garlic more garlic)

; define chez-nous
;
(define chez-nous
  (lambda ()
    (let ((a food))
      (set! food x)
      (set! x a)
    )))
;
; put x and food
(define put-x
  (lambda ()
   x))

(define put-food
 (lambda ()
  food))
;
(put-x)
;Value: pecanpie

(put-food)
;Value: garlic

(chez-nous)

(put-x)
;Value: garlic

(put-food)
;Value: pecanpie

; --------------------------------------------------------------------------------.
; ; the Eighteenth commandment                                                   ;
; ; Use (set! ...) only when the value that x refers to is no longer needed.     ;
; ---------------------------------------------------------------------------------.

; great! time for lunch.
