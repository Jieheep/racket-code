;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname a06q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Constants
(define empty-sum 0)
(define face-card 10)
(define score-to-bust 22)
(define best-score 21)
(define lost-wager -1)
(define high-ace-cutoff 10)
(define high-ace 11)
(define low-ace 1)
(define single-ace 1)

;; Helper Function
;; (sum-list lon) takes each value of the list lon and adds it together until
;; lon is empty.
;; sum-list: (listof Nat) -> Nat
;; Requires:
;; 1) lon does not contain any strings or symbols.
;; 2) The numbers in lon are > 1 and <= 9
;; Examples
(check-expect (sum-list (list 2 2 3)) 7)
(check-expect (sum-list (list 2 2 2)) 6)
(check-expect (sum-list (list 9)) 9)

(define (sum-list lon)
  (cond
    [(empty? lon) empty-sum]
    [else (+ (first lon)(sum-list (rest lon)))]))

;; Helper Function
;; (number-list hand) takes the list hand and extracts all the terms of the list that
;; are numbers and excludes any symbols or strings.
;; number-list: (listof (anyof Nat Sym)) -> (listof Nat)
;; Requires:
;; 1) hand is non-empty
;; 2) hand is a flat list
;; 3) The symbols in hand are either 'J, 'Q, 'K or 'A
;; 4) The numbers in hand are > 1 and <= 9
;; Examples
(check-expect (number-list (list 2 2 'A 'Q 'K))(list 2 2))
(check-expect (number-list (list 'A 'Q 'J 'K))(list))
(check-expect (number-list (list 4 6 8))(list 4 6 8))

(define (number-list hand)
  (cond
    [(empty? hand) empty]
    [(number? (first hand))(cons (first hand)(number-list (rest hand)))]
    [else (number-list (rest hand))]))

;; Helper Function
;; (number-of-aces hand) takes a list of symbols and numbers and extracts the number of
;; times the symbol 'A appears in the list.
;; number-of-aces: (listof (anyof Nat Sym)) -> Nat
;; Requires:
;; 1) hand is non-empty
;; 2) hand is a flat list
;; 3) The symbols in hand are either 'J, 'Q, 'K or 'A
;; 4) The numbers in hand are > 1 and <= 9
;; Examples
(check-expect (number-of-aces (list 2 2 2 'A 'A)) 2)
(check-expect (number-of-aces (list 'A 'A 'A 'A)) 4)
(check-expect (number-of-aces (list 2 2 3 3)) 0)

(define (number-of-aces hand)
  (cond
    [(empty? hand) empty-sum]
    [(equal? (first hand) 'A)(+ single-ace (number-of-aces (rest hand)))]
    [else (number-of-aces (rest hand))]))

;; Helper Function
;; (scores-from-JQK) takes a list of symbols and numbers, hand and adds 10 for every
;; symbols 'J, "Q or 'K that are in the list.
;; scores-from-JQK: (listof (anyof Sym Nat)) -> Nat
;; Requires:
;; 1) hand is non-empty
;; 2) hand is a flat list
;; 3) The symbols in hand are either 'J, 'Q, 'K or 'A
;; 4) The numbers in hand are > 1 and <= 9
;; Examples
(check-expect (scores-from-JQK (list 2 2 3 4 4)) 0)
(check-expect (scores-from-JQK (list 'Q 'K 'A)) 20)
(check-expect (scores-from-JQK (list 'Q 2 'K 4)) 20)

(define (scores-from-JQK hand)
  (cond
    [(empty? hand) empty-sum]
    [(or (equal? (first hand) 'J)(equal? (first hand) 'Q)(equal? (first hand) 'K))
     (+ face-card (scores-from-JQK (rest hand)))]
    [else (scores-from-JQK (rest hand))]))

;; Helper Function
;; (total-score hand) takes the list hand and calculates the value of the hand
;; in terms of a black jack score. The function will also choose whether to use
;; the aces as 1 or 11 depending on what brings them closer to 21, but not over.
;; total-score: (listof (anyof Sym Nat)) -> Nat
;; 1) hand is non-empty
;; 2) hand is a flat list
;; 3) The symbols in hand are either 'J, 'Q, 'K or 'A
;; 4) The numbers in hand are > 1 and <= 9
;; Examples
(check-expect (total-score (list 2 4 'A)) 17)
(check-expect (total-score (list 'A 'K)) 21)
(check-expect (total-score (list 'A 'A 'A)) 13)
(check-expect (total-score (list 5 5 9 2)) 21)

(define (total-score hand)
  (cond
    [(empty? hand) empty-sum]
    [(= (number-of-aces hand) empty-sum)
     (+ (sum-list (number-list hand))(scores-from-JQK hand))]
    [(and (= (number-of-aces hand) single-ace)
          (<= (+ (sum-list (number-list hand))(scores-from-JQK hand))
              high-ace-cutoff))
     (+ (+ high-ace (sum-list (number-list hand)))(scores-from-JQK hand))]
    [(and (= (number-of-aces hand) single-ace)
          (> (+ (sum-list (number-list hand))(scores-from-JQK hand))
             high-ace-cutoff))
     (+ (+ low-ace (sum-list (number-list hand)))(scores-from-JQK hand))]
    [(and (> (number-of-aces hand) single-ace)
          (< (+ (+ (sum-list (number-list hand)) high-ace
                   (* low-ace (- (number-of-aces hand) single-ace)))
                (scores-from-JQK hand)) score-to-bust))
     (+ (+ (sum-list (number-list hand)) high-ace
           (* low-ace (- (number-of-aces hand) single-ace)))
        (scores-from-JQK hand))]
    [(and (> (number-of-aces hand) single-ace)
          (> (+ (+ (sum-list (number-list hand)) high-ace
                   (* low-ace (- (number-of-aces hand) single-ace)))
                (scores-from-JQK hand)) best-score))
     (+ (+ (sum-list (number-list hand))
           (* low-ace (number-of-aces hand)))(scores-from-JQK hand))]))

;; (player-winnings player-hands house-hands wagers) takes the three lists of the
;; same length and produces an integer value based on the wager the player placed
;; and how much the player lost or won.
;; player-winnings:
;; (listof (list of (anyof Sym Nat))(listof (anyof Sym Nat))(listof Nat)) -> Num
;; 1) player-winnings player-hands wagers is non-empty
;; 2) player-winnings player-hands wagers is a flat list
;; 3) The symbols in player-winnings and player-hands are either 'J, 'Q, 'K or 'A
;; 4) The numbers in player-winnings and player-hands are > 1 and <= 9
;; 5) Num in wagers are >= 1
;; 6) All three lists have the same number of terms
;; Examples
(check-expect (player-winnings
               (list (list 2 4 'A) (list 2 6 2 3 5 2) (list 'A 'A 'A 7 'A))
               (list (list 'A 'K) (list 6 2 'K) (list 'K 'Q))
               (list 5 9 2)) 6)
(check-expect (player-winnings
               (list (list 'A 'A 'J 2 3 4)	
                     (list 7 7 'Q) 
                     (list 8 'K)
                     (list 'A 2 'K 6)) 
               (list (list 'K 9 2)	
                     (list 3 2 'J 9) 
                     (list 2 2 3 'A)
                     (list 4 9 5))	
               (list 17 22 11 19)) -31)

(define (player-winnings player-hands house-hands wagers)
  (cond
    [(empty? player-hands) empty-sum]
    [(and (> (total-score (first player-hands)) best-score)
          (> (total-score (first house-hands)) best-score))
     (+ (* lost-wager (first wagers))
        (player-winnings (rest player-hands)(rest house-hands)(rest wagers)))]
    [(and (> (total-score (first player-hands))(total-score (first house-hands)))
          (< (total-score (first player-hands)) score-to-bust))
     (+ (first wagers)
        (player-winnings (rest player-hands) (rest house-hands) (rest wagers)))]
    [(and (> (total-score (first player-hands))(total-score (first house-hands)))
          (> (total-score (first player-hands)) best-score))
     (+ (* lost-wager (first wagers))
        (player-winnings (rest player-hands) (rest house-hands) (rest wagers)))]
    [(and (<= (total-score (first player-hands))(total-score (first house-hands)))
          (< (total-score (first house-hands)) score-to-bust))
     (+ (* lost-wager (first wagers))
        (player-winnings (rest player-hands) (rest house-hands) (rest wagers)))]
    [(and (<= (total-score (first player-hands))(total-score (first house-hands)))
          (> (total-score (first house-hands)) best-score))
     (+ (first wagers)
        (player-winnings (rest player-hands) (rest house-hands) (rest wagers)))]))
    

;; Tests
(check-expect (player-winnings (list (list 7 7 7) (list 7 7 7) (list 7 7 7))
                               (list (list 'J 'K) (list 6 2 'K) (list 'K 'Q))
                               (list 1 1 1)) 3)
(check-expect (player-winnings (list (list 'A 'K) (list 7 7) (list 7 7 7))
                               (list (list 'J 'K) (list 6 2 'K) (list 'K 'Q))
                               (list 1 1 1)) 1)
(check-expect (player-winnings (list (list 7 7 7) (list 2 7 5) (list 2 7 7))
                               (list (list 'J 'K) (list 6 2 'K) (list 'K 'Q))
                               (list 1 1 1)) -1)
(check-expect (player-winnings (list (list 7 2 7) (list 'A 6) (list 5 7 'K))
                               (list (list 'J 'K) (list 6 2 'K) (list 'K 'Q))
                               (list 1 1 1)) -3)
(check-expect (player-winnings (list (list  7 9) (list 7 7 9))
                               (list (list 'J 'K) (list 9 9 'K))
                               (list 1 1)) -2)
(check-expect (player-winnings (list (list 7 7 7))
                               (list (list 'J 'K))
                               (list 10 )) 10)
(check-expect (player-winnings (list (list 'A 'A 'A))
                               (list (list 'A 'K))
                               (list 10)) -10)
j
