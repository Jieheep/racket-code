;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname a06q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Constants
(define substring-start 0)
(define substring-end 1)
(define negative-cutoff 0)

;;(generate-password los loi) consumes a list of strings los and a list of
;; integers loi and produces a string based on the corresponding position
;; of the integer to its string. If there are not enough integers then the
;; first term of the remaining strings are used.
;; generate-password (listof Str) (lisof Num) -> Str
;; Requires: los is non-empty
;; Examples
(check-expect (generate-password (list "abc" "def" "ghi")(list 0 1 2)) "aei")
(check-expect (generate-password (list "abc" "def" "ghi")(list)) "adg")

(define (generate-password los loi)
  (cond
    [(empty? los) ""]
    [(empty? loi)
     (string-append (substring (first los) substring-start substring-end)
                    (generate-password (rest los) loi))]
    [(>= (first loi)(string-length (first los)))(generate-password los (rest loi))]
    [(< (first loi) negative-cutoff)(generate-password los (rest loi))]
    [else (string-append (substring (first los) (first loi)
                                    (+ substring-end (first loi)))
                         (generate-password (rest los)(rest loi)))]))

;; Examples
(check-expect (generate-password (list "abc" "dogs" "!?!")(list 0 3 2)) "as!")
(check-expect (generate-password (list "abc")(list 5 0)) "a")
(check-expect (generate-password (list "abc" "dogs" "xyz" "help")(list 0 3))
              "asxh")
(check-expect (generate-password (list "abc" "hello" "xyz")(list 0 3 0 4 2 3 4))
              "alx")
(check-expect (generate-password
               (list "abc" "125" "xyz" "hello" "lawnmower")(list 0 2 3 1 4 4 4 5 5))
              "a5yom")
(check-expect (generate-password (list "help" "me" "now" "!!!") (list 0 1)) "hen!")
(check-expect (generate-password (list "does" "this" "work?") (list -2 1 7)) "otw")