;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname EEA) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; (EEA a b) produces a list which correspond the table constructed by applying
;;  The Extended Euclidean Algorithm.
;; EEA: Nat Nat -> (listof (list Nat Nat Nat Nat))

;; Examples:
(check-expect (EEA 8 4) (list (list 1 0 8 0)
                              (list 0 1 4 0)
                              (list 1 -2 0 2)))

(check-expect (EEA 42 5) (list (list 1 0 42 0)
                               (list 0 1 5 0)
                               (list 1 -8 2 8)
                               (list -2 17 1 2)
                               (list 5 -42 0 2)))

(check-expect (EEA 5 42) (list (list 1 0 42 0)
                               (list 0 1 5 0)
                               (list 1 -8 2 8)
                               (list -2 17 1 2)
                               (list 5 -42 0 2)))

(define (EEA a b)
  (local [;; (EEA-helper x_1 y_1 r_1 q_1 x_2 y_2 r_2 q_2) produces a list which
          ;;  correspond the table constructed by applying The Extended
          ;;  Euclidean Algorithm.
          ;; EEA-helper: Nat Nat Nat Nat Nat Nat Nat Nat ->
          ;;             (listof (list Nat Nat Nat Nat))
          
          (define (EEA-helper x_1 y_1 r_1 q_1 x_2 y_2 r_2 q_2)
            (cond
              [(= r_2 0) empty]
              [else  (cons (list (- x_1 (* x_2 (quotient r_1 r_2)))
                                 (- y_1 (* y_2 (quotient r_1 r_2)))
                                 (remainder r_1 r_2)
                                 (quotient r_1 r_2))
                           (EEA-helper x_2 y_2 r_2 q_2
                                       (- x_1 (* x_2 (quotient r_1 r_2)))
                                       (- y_1 (* y_2 (quotient r_1 r_2)))
                                       (remainder r_1 r_2)
                                       (quotient r_1 r_2)))]))]

    (cons  (list 1 0 (max a b) 0)
           (cons (list 0 1 (min a b) 0)
                 (EEA-helper 1 0 (max a b) 0 0 1 (min a b) 0)))))


;; (EEA-useful-line a b) produces the list of the numbers in the second to last
;;  line in the table of EEA.
;; EEA-useful-line: Nat Nat -> (list Nat Nat Nat Nat)

;; Examples:
(check-expect (EEA-useful-line 8 4) (list 0 1 4 0))
(check-expect (EEA-useful-line 42 5) (list -2 17 1 2))

(define (EEA-useful-line a b)
  (local [(define (second-to-last lst)
            (cond [(= 2 (length lst)) (first lst)]
                  [else (second-to-last (rest lst))]))]
    (second-to-last (EEA a b))))


;; (expression a b) produces a string of calculation formula, which is also a
;;  certificate of correctness of the EEA.
;; expression: Nat Nat -> Str

;; Examples:
(check-expect (expression 8 4) "8 * 0 + 4 * 1 = 4")
(check-expect (expression 2172 423) "2172 * 52 + 423 * -267 = 3")

(define (expression a b)
  (local
    [;; (coef-a a b) produces the coefficient of a in the calculation formula.
     ;; coef-a: Nat Nat -> Int
     (define (coef-a a b)
       (cond [(> a b) (first (EEA-useful-line a b))]
             [else (second (EEA-useful-line a b))]))

     ;; (coef-b a b) produces the coefficient of b in the calculation formula.
     ;; coef-b: Nat Nat -> Int
     (define (coef-b a b)
       (cond [(> a b) (second (EEA-useful-line a b))]
             [else (first (EEA-useful-line a b))]))

     ;; (gcd a b) produces the greatest common divisor of a and b
     ;; gcd: Nat Nat -> Nat
     (define (gcd a b)
       (third (EEA-useful-line a b)))]
    
    (string-append (number->string a)
                   " * "
                   (number->string (coef-a a b))
                   " + "
                   (number->string b)
                   " * "
                   (number->string (coef-b a b))
                   " = "
                   (number->string (gcd a b)))))

;; Tests:
(check-expect (expression 1 1) "1 * 1 + 1 * 0 = 1")
(check-expect (expression 1386 322) "1386 * 10 + 322 * -43 = 14")

