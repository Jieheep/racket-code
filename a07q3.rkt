;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname a07q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Helper for Constants
;; A Point is a (list Num Num)
;; make-point: Num Num -> Point
(define (make-point x y) (list x y))
;; point-x: Point -> Num
(define (point-x pt) (first pt))
;; point-y: Point -> Num
(define (point-y pt) (second pt))

;; Constants
(define in-range 0.0001)
(define negative-bound -1)
(define D 0.5)
(define D2 2)
(define D3 4)
(define D4 6)
(define D5 5)
(define theta1 (/ pi 3))
(define theta2 pi)
(define theta3 (/ pi 6))
(define theta4 (/ pi 2))
(define theta5 0)
(define theta6 (* 2 pi))
(define theta7 (/ pi 4))
(define rect2-P1 (make-point 0 0))
(define rect2-P2 (make-point 0 1))
(define rect2-P3 (make-point 1 1)) 
(define rect2-P4 (make-point 1 0)) 
(define rect1-P1 (make-point 0 0))
(define rect1-P2 (make-point 0 0.5))
(define rect1-P3 (make-point 1 0.5)) 
(define rect1-P4 (make-point 1 0)) 
(define rect3-P1 (make-point 0 0))
(define rect3-P2 (make-point 0 1))
(define rect3-P3 (make-point 1 1)) 
(define rect3-P4 (make-point 1 0)) 
(define rect4-P1 (make-point 0 0))
(define rect4-P2 (make-point 2 0))
(define rect4-P3 (make-point 2 4)) 
(define rect4-P4 (make-point 0 4))
(define rect5-P1 (make-point 4 0))
(define rect5-P2 (make-point 2 0))
(define rect5-P3 (make-point 2 4)) 
(define rect5-P4 (make-point 4 4))
(define rect6-P1 (make-point 4 2))
(define rect6-P2 (make-point 2 2))
(define rect6-P3 (make-point 2 4)) 
(define rect6-P4 (make-point 4 4))
(define rect7-P1 (make-point 4 -2))
(define rect7-P2 (make-point 2 -2))
(define rect7-P3 (make-point 2 -4)) 
(define rect7-P4 (make-point 4 -4))
(define rect8-P1 (make-point -4 -2))
(define rect8-P2 (make-point -2 -2))
(define rect8-P3 (make-point -2 -4)) 
(define rect8-P4 (make-point -4 -4))
(define rect9-P1 (make-point -4 2))
(define rect9-P2 (make-point -2 2))
(define rect9-P3 (make-point -2 4)) 
(define rect9-P4 (make-point -4 4))
(define rect10-P1 (make-point 0 0))
(define rect10-P2 (make-point 4 0))
(define rect10-P3 (make-point 4 2)) 
(define rect10-P4 (make-point 0 2))

;; (rotated-rect-inside-lines? p1 p2 p3 p4 theta D) consumes 4 coordinate points
;;   in the form of lists as well as an angle theta and a value D and calculates
;;   whether or not the rectangle formed by the 4 points when rotated about the
;;   angle theta will have x values that fall within the values of D. If they do
;;   the function produces true and if not, the function produces false.
;; rotated-rect-inside-lines?: Point Point Point Point Num Num -> Bool
;; Requires:
;; 1) theta is in radians
;; 2) p1 p2 p3 p4 are all points of a rectangle
;; 3) D is positive
;; Examples
(check-expect
 (rotated-rect-inside-lines? rect1-P1 rect1-P2 rect1-P3 rect1-P4 theta1 D)
 true)
(check-expect
 (rotated-rect-inside-lines? rect2-P1 rect2-P2 rect2-P3 rect2-P4 theta2 D)
 false)
(check-expect
 (rotated-rect-inside-lines? rect3-P1 rect3-P2 rect3-P3 rect3-P4 theta3 D)
 false)

(define (rotated-rect-inside-lines? p1 p2 p3 p4 theta D)
  (local [;;(new-x-values point theta) consumes a list point and using the
          ;;   equations of translations of a point, the x-coordinate and theta
          ;;   it will produce a new x-coordinate of the point when translated.
          ;; new-x-values: (listof Num) Num -> Num
          ;; Requires:
          ;; 1) theta is in radians
          (define (new-x-values point theta)
            (- (* (cos theta)(point-x point))(* (sin theta)(point-y point))))]
    (cond
      [(or (<= (new-x-values p1 theta)(- (* negative-bound D) in-range))
           (>= (new-x-values p1 theta)(+ D in-range))) false]
      [(or (<= (new-x-values p2 theta)(- (* negative-bound D) in-range))
           (>= (new-x-values p2 theta)(+ D in-range))) false]
      [(or (<= (new-x-values p3 theta)(- (* negative-bound D) in-range))
           (>= (new-x-values p3 theta)(+ D in-range))) false]
      [(or (<= (new-x-values p4 theta)(- (* negative-bound D) in-range))
           (>= (new-x-values p4 theta)(+ D in-range))) false]
      [else true])))

;; Tests
(check-expect
 (rotated-rect-inside-lines? rect4-P1 rect4-P2 rect4-P3 rect4-P4 theta4 D2)
 false)
(check-expect
 (rotated-rect-inside-lines? rect4-P1 rect4-P2 rect4-P3 rect4-P4 theta5 D2)
 true)
(check-expect
 (rotated-rect-inside-lines? rect4-P1 rect4-P2 rect4-P3 rect4-P4 theta6 D2)
 true)
(check-expect
 (rotated-rect-inside-lines? rect4-P1 rect4-P2 rect4-P3 rect4-P4 theta6 D3)
 true)
(check-expect
 (rotated-rect-inside-lines? rect4-P1 rect4-P2 rect4-P3 rect4-P4 theta4 D3)
 true)
(check-expect
 (rotated-rect-inside-lines? rect5-P1 rect5-P2 rect5-P3 rect5-P4 theta4 D2)
 false)
(check-expect
 (rotated-rect-inside-lines? rect5-P1 rect5-P2 rect5-P3 rect5-P4 theta4 D4)
 true)
(check-expect
 (rotated-rect-inside-lines? rect6-P1 rect6-P2 rect6-P3 rect6-P4 theta4 D5)
 true)
(check-expect
 (rotated-rect-inside-lines? rect6-P1 rect6-P2 rect6-P3 rect6-P4 theta4 D2)
 false)
(check-expect
 (rotated-rect-inside-lines? rect7-P1 rect7-P2 rect7-P3 rect7-P4 theta2 D2)
 false)
(check-expect
 (rotated-rect-inside-lines? rect7-P1 rect7-P2 rect7-P3 rect7-P4 theta2 D5)
 true)
(check-expect
 (rotated-rect-inside-lines? rect8-P1 rect8-P2 rect8-P3 rect8-P4 theta2 D5)
 true)
(check-expect
 (rotated-rect-inside-lines? rect8-P1 rect8-P2 rect8-P3 rect8-P4 theta2 D2)
 false)
(check-expect
 (rotated-rect-inside-lines? rect9-P1 rect9-P2 rect9-P3 rect9-P4 theta2 D2)
 false)
(check-expect
 (rotated-rect-inside-lines? rect9-P1 rect9-P2 rect9-P3 rect9-P4 theta2 D5)
 true)
(check-expect
 (rotated-rect-inside-lines? rect10-P1 rect10-P2 rect10-P3 rect10-P4 theta7 D2)
 false)
