;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname samplefinal) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Question 4

;; part a
(define (make-dupes n elem)
  (build-list n (lambda (x) elem))
  )

(check-expect (make-dupes 10 5) (list 5 5 5 5 5 5 5 5 5 5))

;; part b)
(define (count-occurrences lst elem)
  (length (filter (lambda (x) (= elem x)) lst)))

(check-expect (count-occurrences (list 1 2 3 4 4 3 2 1 2 3 9 4 8 7 6 5) 2) 3)

;; part c)
(define (count-occurrences-v2 lst elem)
  (foldl (lambda (x sum) (cond [(= elem x) (add1 sum)]
                               [else sum])) 0 lst)
  )

(check-expect (count-occurrences-v2 (list 1 2 3 4 4 3 2 1 2 3 9 4 8 7 6 5) 2) 3)


;; Question 5

(define (longest-string los)
  (longest-string/acc (rest los) (first los) (string-length (first los)))
  )

(define (longest-string/acc los acc len)
  (cond [(empty? los) acc]
        [(= (string-length (first los)) len)
         (cond [(string<? (first los) acc) (longest-string/acc (rest los) (first los) (string-length (first los)))]
               [else (longest-string/acc (rest los) acc len)])]
        [(> (string-length (first los)) len) (longest-string/acc (rest los) (first los) (string-length (first los)))]
        [else (longest-string/acc (rest los) acc len)]))

(check-expect (longest-string '("app" "apple" "alpha" "beta" "gamma")) "alpha")


;; Question 7

(define-struct node (key val left right))

;; Part c)
(define (min-val t)
  (cond [(empty? t) t]
        [(empty? (node-left t)) (node-val t)]
        [else (min-val (node-left t))])
  )

(define A (make-node 5 5 (make-node 2 2 (make-node 1 1 empty empty) (make-node 3 3 empty empty))
                       (make-node 7 7 empty (make-node 9 9 empty empty))))

(check-expect (min-val A) 1)

;; Part d)
(define (keys-larger-than t k)
  (local [(define (helper t)
            (cond [(empty? t) empty]
                  [(< (node-key t) k) (keys-larger-than (node-left t) k)]
                  [(> (node-key t) k) (append (list (node-key t)) (keys-larger-than (node-left t) k) (keys-larger-than (node-right t) k))]
                  [else (keys-larger-than (node-right t) k)]))]
    (quicksort (helper t) <)))

(check-expect (keys-larger-than A 2) (list 3 5 7 9))

;; Part e)
;; This is legit hard lmao


;; Question 8

;; Part a)
;; (local [(define x 1)(define y 2)] x)

;; step 1
;; (define x_0 1)
;; (define y_0 2)
;; x_0

;; step 2
;; (define x_0 1)
;; (define y_0 2)
;; 1

;; final value
;; 1


;; Part b)
;; (rest (rest (foldr cons '(0) '(1 2))))

;; step 1
;; (rest (rest '(1 2 0)))

;; step 2
;; (rest '(2 0))

;; final value
;; 0


;; Part c)
;; (((lambda (x) (lambda (y) (+ x y))) 1) (+ 1 1))

;; step 1
;; ((lambda (y) (+ 2 y)) (+ 1 1)))

;; step 2
;; ((lambda (y) (+ 2 y)) 2))

;; final value
;; 3


;; Part d)
;; (filter (lambda (x) true) (map even? (build-list 3 add1)))

;; step 1
;; (filter (lambda (x) true) (map even? '(1 2 3)))

;; step 2
;; (filter (lambda (x) true) '(2))

;; step 3
;; true


;; Part e)
;; (build-list 3 (local [(define f 3)] f))

;; step 1
;; error

;; final value
;; error


;; Part f)
;; (cond [(local [(define x true)] x) x])

;; step 1
;; (cond [(local [(define x true)] true) x])

;; step 2
;; error: x is not defined

;; final value
;; error: x is not defined



;; Question 9

;; Part a)

;; without hofs (simple recursion)
(define (interleave pred? lst1 lst2)
  (cond [(empty? lst1) empty]
        [(pred? (first lst1) (first lst2)) (cons (first lst1) (interleave pred? (rest lst1) (rest lst2)))]
        [else (cons (first lst2) (interleave pred? (rest lst1) (rest lst2)))])
  )



(check-expect (interleave < '(1 2 3) '(3 2 1)) '(1 2 1))
(check-expect (interleave > '(1 2 3) '(3 2 1)) '(3 2 3))

;; without hofs (accumulative recursion
(define (interleave-v3 pred? lst1 lst2)
  (local [(define (interleave/acc lst1 lst2 acc)
            (cond [(empty? lst1) acc]
                  [(pred? (first lst1) (first lst2)) (interleave/acc (rest lst1) (rest lst2) (append acc (list (first lst1))))]
                  [else (interleave/acc (rest lst1) (rest lst2) (append acc (list (first lst2))))])
            )]
  (interleave/acc lst1 lst2 empty)))


;; using hofs
(define (interleave-v2 pred? lst1 lst2)
  (foldr (lambda (x y acc) (cond [(pred? x y) (append acc (list x))]
                                 [else (append acc (list y))])) empty lst1 lst2)
  )

(check-expect (interleave-v2 < '(1 2 3) '(3 2 1)) '(1 2 1))
(check-expect (interleave-v2 > '(1 2 3) '(3 2 1)) '(3 2 3))


;; Part b)

;; actual way
(define (keep-divisors lst1 lst2)
  (interleave (lambda (x y) (= 0 (modulo y x))) lst1 lst2)
  )

(check-expect (keep-divisors '(2 3 4 5) '(10 10 10 10)) '(2 10 10 5))


;; using hofs
(define (keep-divisors-v2 lst1 lst2)
  (foldl (lambda (x y acc) (cond [(= 0 (modulo y x)) (append acc (list x))]
                                 [else (append acc (list y))])) empty lst1 lst2))

;; Part c)

(define (replace-symbols lst1 lst2)
  (interleave (lambda (x y) (not (symbol? x))) lst1 lst2)
  )

(check-expect (replace-symbols '(1 "two" three) '(1 2 3)) '(1 "two" 3))

;; Question 10

;; Part a)

(define (sinks G)
  (cond [(empty? G) empty]
        [(empty? (second (first G))) (cons (first (first G)) (sinks (rest G)))]
        [else (sinks (rest G))])
  )

(define graph '((A (C D E))
                (B (E J))
                (C ())
                (D (F J))
                (E (K))
                (F (K H))
                (H ())
                (J (H))
                (K ())))

(check-expect (sinks graph) '(C H K))

;; Part b)
(define (in-neighbours n G)
  (map (lambda (x) (first x)) (filter (lambda (x) (member? n (second x))) G))
  )

(check-expect (in-neighbours 'H graph) '(F J))

;; Part c)
(define (source G)
  (filter (lambda (x) (empty? (in-neighbours x G))) (map (lambda (x) (first x)) G)))

(check-expect (source graph) '(A B))

;; Part d)

;; can only use hofs hmm

(define (reverse-graph G)
  (foldr (lambda (x y) (append y (list (list (first x) (in-neighbours (first x) G))))) empty G)
  )

(check-expect (reverse-graph graph)
              (list
               (list 'K (list 'E 'F))
               (list 'J (list 'B 'D))
               (list 'H (list 'F 'J))
               (list 'F (list 'D))
               (list 'E (list 'A 'B))
               (list 'D (list 'A))
               (list 'C (list 'A))
               (list 'B '())
               (list 'A '())))


;; Question 11

;; Part a)
(define (quick-select lon n)
  (local [(define (insert x lon)
            (cond [(empty? lon) (cons x empty)]
                  [(< x (first lon)) (cons x lon)]
                  [else (cons (first lon) (insert x (rest lon)))]))

          (define (sort lon new)
            (cond [(empty? lon) new]
                  [else (sort (rest lon) (insert (first lon) new))]))

          (define (helper sortedlist m)
            (cond
              [(empty? sortedlist) empty]
              [(= m n) (first sortedlist)]
              [else (helper (rest sortedlist) (add1 m))]))
          ]
    (helper (sort lon empty) 1)))
    

(check-expect (quick-select '(2 3.1 2 -5 0 4 0) 5) 2)


;; Question 12

;; Part a)








