;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a09q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; A Polygon is a (listof Posn)
;; Requires:
;;   Polygon is non-empty
;;   Posn are listed in counterclockwise or clockwise order

;; Constants
(define v1 (make-posn 0.5 0))
(define v2 (make-posn 1 2))
(define v3 (make-posn -1 1))
(define v4 (make-posn -0.5 -1))
(define v5 (make-posn 0 0))
(define v6 (make-posn 1 -1))
(define polygon1 (list v1))
(define polygon2 (list v1 v2))
(define polygon3 (list v1 v2 v3))
(define polygon4 (list v1 v2 v3 v4))
(define polygon5 (list v1 v2 v3 v4 v5))
(define polygon6 (list v1 v2 v3 v4 v5 v6))
(define lon0 (list))
(define lon1 (list 1))
(define lon2 (list 1 2))

;; Helper Function
;; (last-term pgon) consumes a Polygon and produces the last term in
;;  the list.
;; last-term: Polygon -> Posn
;; Requires:
;;   Polygon is non-empty
;; Examples
(check-expect (last-term polygon1) v1)
(check-expect (last-term polygon2) v2)
(check-expect (last-term polygon3) v3)

(define (last-term pgon)
  (cond
    [(empty? (rest pgon))(first pgon)]
    [else (last-term (rest pgon))]))

;; Helper Function
;; (sum-list lon) consumes a list of Num and produces the sum of all the
;;  numbers in the list.
;; sum-list: (listof Num) -> Num
;; Examples
(check-expect (sum-list lon0) 0)
(check-expect (sum-list lon1) 1)
(check-expect (sum-list lon2) 3)

(define (sum-list lon)
  (cond
    [(empty? lon) 0]
    [else (+ (first lon)(sum-list (rest lon)))]))

;; (polygon-area pgon) consumes a Polygon and produces the area of the Polygon.
;; polygon-area: Polygon -> Num
;; Requires:
;;  Polygon is non-empty
;; Examples
(check-expect (polygon-area polygon1) 0)
(check-expect (polygon-area polygon2) 0)
(check-expect (polygon-area polygon3) 1.75)

(define (polygon-area pgon)
  (/ (abs
      (+ (local
           [(define (last-to-origin pgon)
              (- (* (posn-x (last-term pgon))(posn-y (first pgon)))
                 (* (posn-x (first pgon))(posn-y (last-term pgon)))))]
           (last-to-origin pgon))
         (local
           [(define (first-sums pgon)
              (sum-list
               (cond
                 [(empty? (rest pgon)) empty]
                 [else
                  (cons
                   (- (* (posn-x (first pgon))(posn-y (second pgon)))
                      (* (posn-x (second pgon))(posn-y (first pgon))))
                   (cons (first-sums (rest pgon)) empty))])))]
           (first-sums pgon)))) 2))

;; Tests
(check-expect (polygon-area polygon4) 3)
(check-expect (polygon-area polygon5) 2.75)
(check-expect (polygon-area polygon6) 3)
