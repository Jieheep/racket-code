;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a03q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Constants
(define FI-lower-cutoff 5)
(define FI-upper-cutoff 8)
(define Account-lower-cutoff 8)
(define Bank-of-Montreal-code 001)
(define Scotiabank-code 002)
(define Royal-Bank-code 003)
(define Bank-of-Canada-code 177)
(define PC-Financial-code 326)
(define Canadian-Tire-Bank-code 338)

;; Helper Function
;; (branch_number account_number) produces a substring of the string account_number
;; based on the first 5 digits of the string.
;; branch_number: Str -> Str
;; Examples:
(check-expect (branch_number "123456123456") "12345")

(define (branch_number account_number)
  (substring account_number 0 5))

;; Helper Function
;; (financial_institution account_number) produces a string corresponding with
;; the financial institution
;; and the next 3 digits of the string after the account_number.
;; If there is no corresponding financial institution, then the function will produce a
;; new bank entitled "Bank" followed by the three digits following the Account Number.
;; financial_institution: Str -> Str
;; Examples:
(check-expect (financial_institution "000001770000") "Bank of Canada")
(check-expect (financial_institution "000003380000") "Canadian Tire Bank")
(check-expect (financial_institution "000001230000") "Bank 123")

(define (financial_institution account_number)
  (cond
    [(= (string->number (substring account_number FI-lower-cutoff FI-upper-cutoff))
        Bank-of-Montreal-code) "Bank of Montreal"]
    [(= (string->number (substring account_number FI-lower-cutoff FI-upper-cutoff))
        Scotiabank-code) "Scotiabank"]
    [(= (string->number (substring account_number FI-lower-cutoff FI-upper-cutoff))
        Royal-Bank-code) "Royal Bank"]
    [(= (string->number (substring account_number FI-lower-cutoff FI-upper-cutoff))
        Bank-of-Canada-code) "Bank of Canada"]
    [(= (string->number (substring account_number FI-lower-cutoff FI-upper-cutoff))
        PC-Financial-code) "PC Financial"]
    [(= (string->number (substring account_number FI-lower-cutoff FI-upper-cutoff))
        Canadian-Tire-Bank-code) "Canadian Tire Bank"]
    [else (string-append "Bank" " "
                         (substring account_number FI-lower-cutoff FI-upper-cutoff))]))

;; Helper Function
;; (account account_number) produces a substring based on the string account_number
;; from the 9th digit
;; inclusive until the end of the string.
;; account: Str -> Str
;; Examples
(check-expect (account "00000000123456") "123456")

(define (account account_number)
  (substring account_number Account-lower-cutoff))

;; (banking account_number) produces a new string which complies all the substrings
;; financial_institution, "Branch" and branch_number, ":" and "Account" and account
;; produced from the string account_number.
;; account_number: Str -> Str
;; Requires:
;; string length of account_number > 14 and string length of account_number < 21
;; Examples:
(check-expect (banking "12345001123456") "Bank of Montreal (Branch 12345): Account 123456")
(check-expect (banking "00000000000000") "Bank 000 (Branch 00000): Account 000000")

(define (banking account_number)
  (string-append
   (financial_institution account_number)" " "(" "Branch" " "
   (branch_number account_number) ")" ":" " " "Account" " "
   (account account_number)))

;; Tests
(check-expect (banking "121218882314398123")
              "Bank 888 (Branch 12121): Account 2314398123")
(check-expect (banking "017003262314398")
              "PC Financial (Branch 01700): Account 2314398")
(check-expect (banking "017000012314398")
              "Bank of Montreal (Branch 01700): Account 2314398")
(check-expect (banking "017000022314398")
              "Scotiabank (Branch 01700): Account 2314398")
(check-expect (banking "017000032314398")
              "Royal Bank (Branch 01700): Account 2314398")
(check-expect (banking "017001772314398")
              "Bank of Canada (Branch 01700): Account 2314398")
(check-expect (banking "017003382314398")
              "Canadian Tire Bank (Branch 01700): Account 2314398")

(banking "1234512345123")
