;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname module13) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Module 13 General Tree- Exercises

;; Ex. 1

(define (nest-lst-sum x)
  (cond [(empty? x) 0]
        [(number? x) x]
        [else (+ (nest-lst-sum (first x)) (nest-lst-sum (rest x)))])
  )

(check-expect (nest-lst-sum empty) 0)
(check-expect (nest-lst-sum '(1 1)) 2)
(check-expect (nest-lst-sum '(1 2 (3 4) () 7 ((1 4) 1))) 23)


(define (nl-max-depth x)
  (cond [(empty? x) 0]
        ;[(number? x) 1]
        [else (max (length x) (nl-max-depth (rest x)))])
  )

(check-expect (nl-max-depth '()) 0)
(check-expect (nl-max-depth '(())) 1)
(check-expect (nl-max-depth '(1)) 1)
(check-expect (nl-max-depth '(1 (1 2))) 2)


;; Ex. 2

(define-struct gnode (key children))
;; a GT (generalized tree) is a (make-gnode Nat (listof GT))

-
(define (reverse-gt gt)
  (cond [(empty? gt) empty]
        [(number? gt) gt]
        [else (make-gnode (gnode-key gt) (map reverse-gt (reverse (gnode-children gt))))]
  ))


(define tree (make-gnode 40 (list (make-gnode 28 '(9 32)) (make-gnode 70 '(52 102)))))
(check-expect (reverse-gt tree) empty)


;; Ex. 3

;(define (most-populated-level)
;  )
