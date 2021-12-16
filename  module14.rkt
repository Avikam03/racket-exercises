;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname | module14|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Module 14 Local Definitions- Exercises

;; Ex. 1

(define (check-msg-length to from body min-len max-len)
  (local [(define len (+ (string-length to) (string-length from) (string-length body)))]
    (cond [(> len min-len)
          (cond [(< len max-len) len]
                [else 'too-long])]
          [else 'too-short])
    )
  )

(check-expect (check-msg-length "Ed" "Santa" "Xmas List" 3 14) 'too-long)
(check-expect (check-msg-length "Ed" "Santa" "Xmas List" 3 140) 16)
(check-expect (check-msg-length "Charlie" "Santa" "No Presents for Ed!" 140 280) 'too-short)


;; Ex. 2

(define (normalize lst)
  (local [(define sum (foldr + 0 lst))]
    (map (lambda (x) (/ x sum)) lst)
    )
  )

(check-expect (normalize (list 4 2 14)) (list 0.2 0.1 0.7))

;; Ex. 3

(define (list-squares n)
  (build-list n (lambda (x) (expt x 2)))
  )

(check-expect (list-squares 4) (list 0 1 4 9))

;; Ex. 4

(define (mult-table2 nr nc)
  (local [(define (rows-to r)
            (local [(define (cols-to c)
                      (cond [(>= c nc) empty]
                            [else (cons (* r c) (cols-to (add1 c)))]))]
            (cond [(>= r nr) empty]
                  [else (cons (cols-to 0) (rows-to (add1 r)))])))
          ]
    (rows-to 0)))

(check-expect (mult-table2 3 4)
(list (list 0 0 0 0)
      (list 0 1 2 3)
      (list 0 2 4 6)))


;; Ex. 5

(define (table-ccr nr nc)
  (local [(define (main r)
            (cond [(= nr r) empty]
                  [(< r nr) (cons (build-list nc (lambda (x) (* (expt x 2) r))) (main (add1 r)))]))]
  (main 0)))

(table-ccr 4 5)




