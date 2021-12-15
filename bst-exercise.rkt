;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bst-exercise) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Chapter Binary Trees

(define-struct node (key left right))

(define test-tree (make-node 5 (make-node 1 (make-node 3 '() '())
                                          (make-node 6 '() '()))
                             (make-node 1 (make-node 1 (make-node 4 '() '())
                                                     (make-node 6 '() '()))
                                        (make-node 3 '() '()))))


;; Ex. 1

(define (count-leaves t)
  (cond [(empty? t) 0]
        [(and (empty? (node-right t)) (empty? (node-left t))) 1]
        [else (+ (count-leaves (node-left t)) (count-leaves (node-right t)))])  
  )

(check-expect (count-leaves empty) 0)
(check-expect (count-leaves test-tree) 5)
(check-expect (count-leaves (make-node 10 empty empty)) 1)
(check-expect (count-leaves (make-node 10 (make-node 5 empty empty)
                                       empty)) 1)
(check-expect (count-leaves (make-node 10 (make-node 5 empty empty)
                                       (make-node 5 empty empty))) 2)


;; Ex. 2

(define (count-evens t)
  (cond [(empty? t) 0]
        [(= (modulo (node-key t) 2) 0) (+ 1 (count-evens (node-left t)) (count-evens (node-right t)))]
        [else (+ (count-evens (node-left t)) (count-evens (node-right t)))])
  )

(check-expect (count-evens empty) 0)
(check-expect (count-evens test-tree) 3)
(check-expect (count-evens (make-node 10 empty empty)) 1)
(check-expect (count-evens (make-node 10 (make-node 5 empty empty)
                                       empty)) 1)
(check-expect (count-evens (make-node 10 (make-node 5 empty empty)
                                       (make-node 5 empty empty))) 1)
(check-expect (count-evens (make-node 10 (make-node 8 empty empty)
                                       (make-node 6 empty empty))) 3)


;; Ex. 3

(define (reverse-tree t)
  (cond
    [(empty? t) empty]
    [else (make-node (node-key t)
                     (reverse-tree (node-right t))
                     (reverse-tree (node-left t)))]
    )
  )

(check-expect (reverse-tree empty) empty)
(check-expect
 (reverse-tree (make-node 1 (make-node 2 (make-node 5 empty empty) '()) (make-node 3 '() '())))
 (make-node 1 (make-node 3 '() '()) (make-node 2 '() (make-node 5 empty empty))))


;; Ex. 4

(define (contains? k tree)
  (cond
    [(empty? tree) false]
    [(= k (node-key tree)) true]
    [else (or (contains? k (node-left tree)) (contains? k (node-right tree)))]
    )
  )

(check-expect (contains? 2 empty) false)
(check-expect (contains? 4 test-tree) true)


;; bst
(define test-bst (make-node 5 (make-node 1 (make-node 0 '() '())
                                         (make-node 3 '() '()))
                            (make-node 6 '() (make-node 14 '() '()))))

;; Ex. 5

(define (count-smaller n t)
  (cond
    [(empty? t) 0]
    [(> n (node-key t)) (+ 1 (count-smaller n (node-left t))
                           (count-smaller n (node-right t)))]
    [else (+ (count-smaller n (node-left t))
                           (count-smaller n (node-right t)))]
    )
  )

(check-expect (count-smaller 0 test-bst) 0)
(check-expect (count-smaller 8 test-bst) 5)
(check-expect (count-smaller 100 test-bst) 6)

;; Ex. 6

#|
(define (bst-min t)
  (bst-min/acc t (node-key t))
  )

(define (bst-min/acc t acc)
  (cond
    [(empty? t) acc]
    [else (min (node-key t) (bst-min/acc (node-left t) acc)
               (bst-min/acc (node-right t) acc))]
    ))
|#

(define (bst-min t)
  (cond [(empty? (node-left t)) (node-key t)]
        [else (bst-min (node-left t))])
  )

(check-expect (bst-min (make-node 6 '() '())) 6)
(check-expect (bst-min test-bst) 0)


;; bst-max

#|
(define (bst-max t)
  (bst-max/acc t (node-key t))
  )

(define (bst-max/acc t acc)
  (cond
    [(empty? t) acc]
    [else (max (node-key t) (bst-max/acc (node-left t) acc)
               (bst-max/acc (node-right t) acc))]
    ))
|#

(define (bst-max t)
  (cond [(empty? (node-right t)) (node-key t)]
        [else (bst-max (node-right t))])
  )

(check-expect (bst-max (make-node 6 '() '())) 6)
(check-expect (bst-max test-bst) 14)


;; bst-add

(define (bst-add n t)
  (cond [(empty? t) (make-node n empty empty)]
        [(= n (node-key t)) t]
        [(< n (node-key t)) (make-node (node-key t)
                                       (bst-add n (node-left t))
                                       (node-right t))]
        [(> n (node-key t)) (make-node (node-key t)
                                       (node-left t)
                                       (bst-add n (node-right t)))]))

(check-expect (bst-add 1 (make-node 2 '() '()))
              (make-node 2 (make-node 1 '() '()) '()))
(check-expect (bst-add 3 empty) (make-node 3 '() '()))
(check-expect (bst-add 3 (make-node 3 '() '())) (make-node 3 '() '()))
(check-expect (bst-add 4 (make-node 2 '() '()))
              (make-node 2 '() (make-node 4 '() '())))


;; bst-from-list (hax)

(define (bst-from-list lon)
  (bst-from-list-helper (quicksort lon <))
  )

(define (bst-from-list-helper lon)
  (cond
    [(empty? lon) empty]
    [else (make-node (first lon) empty (bst-from-list-helper (rest lon)))]
    )
  )

(check-expect (bst-from-list (list 3 4 9 0 1 99 2 13))
              (make-node 0 empty
                 (make-node 1 empty
                 (make-node 2 empty
                 (make-node 3 empty
                 (make-node 4 empty
                 (make-node 9 empty
                 (make-node 13 empty
                 (make-node 99 empty
                empty)))))))))



;; search-bst

(define (search-bst t)
  
  )



