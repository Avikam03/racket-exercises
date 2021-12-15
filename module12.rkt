;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname module12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Module 12 Mutual Recursion- Exercises

;; Ex. 1

(define (pebbles-alice n)
  (cond
    [(< n 1) "Bob wins"]
    [(= n 1) "Alice wins"]
    [else (pebbles-bob (sub1 n))])
  )

(define (pebbles-bob n)
  (cond
    [(< n 1) "Alice wins"]
    [(= n 1) "Bob wins"]
    [(odd? n) (pebbles-alice (sub1 n))]
    [else (pebbles-alice (- n 2))])
  )

(check-expect (pebbles-alice 1) "Alice wins")
(check-expect (pebbles-alice 2) "Bob wins")
(check-expect (pebbles-alice 3) "Bob wins")

