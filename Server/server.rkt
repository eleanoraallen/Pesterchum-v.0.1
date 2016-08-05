;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname server) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#lang racket
(require 2htdp/batch-io)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions

;; a universe is a (make-universe list-of-iworlds list-of-strings)
(define-struct u (l l2))
(define DEFAULT (make-u empty empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main functions

;; handle-new : universe, iworld --> bundle
;; takes a universe-state, and an iworld that has just joined and outputs a bundle containing
;; mail to every iworld to the effect that (iworld-name iworld) has joined the chat
(define (handle-new u i)
  (make-u (cons i (u-l u)) (u-l2 u)))

;; handle-msg : universe, iworld, sexp --> bundle
(define (handle-msg u i s)
  (if
   (and (cons? s) (symbol? (first s)))
   (make-u (u-l u) (if (unique? s (u-l2 u)) (cons s (u-l2 u)) (reverse (update s (u-l2 u) empty))))
   (make-bundle u (send-to-recipients (u-l u) i s) empty)))

;; update: list, list, list --> list
(define (update l l2 l3)
  (cond
    [(empty? l2) l3]
    [(cons? l2)
     (if (symbol=? (first l) (first (first l2)))
         (update l (rest l2) (cons l l3))
         (update l (rest l2) (cons (first l2) l3)))]))

;; tock: universe --> bundle
;; sends out a list of current iworld names to every connected iworld
(define (tock u) (make-bundle u (send-to-all (u-l u) (u-l2 u)) empty))

;; disconnect
(define (disconnect u i)
  (make-u (remove-iworld (u-l u) i) empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; send-to-all : loiw sexp --> lom
;; makes a lom w/ message sexp adressed to all iworlds in loiw
(define (send-to-all l s)
  (cond
    [(empty? l) empty]
    [(cons? l) (cons (make-mail (first l) s) (send-to-all (rest l) s))]))

;; derive-initials : string -> string
;; outputs the initials (first letter, capital leter) of a given string
(define (derive-initials s) (string-append (capitalize (first (explode s))) (find-capital (rest (explode s)))))

;; capitalize 1string --> 1string 
;; capitalizes a given letter
(define (capitalize s)
  (cond
    [(string=? s "a") "A"]
    [(string=? s "b") "B"]
    [(string=? s "c") "C"]
    [(string=? s "d") "D"]
    [(string=? s "e") "E"]
    [(string=? s "f") "F"]
    [(string=? s "g") "G"]
    [(string=? s "h") "H"]
    [(string=? s "i") "I"]
    [(string=? s "j") "J"]
    [(string=? s "k") "K"]
    [(string=? s "l") "L"]
    [(string=? s "m") "M"]
    [(string=? s "n") "N"]
    [(string=? s "o") "O"]
    [(string=? s "p") "P"]
    [(string=? s "q") "Q"]
    [(string=? s "r") "R"]
    [(string=? s "s") "S"]
    [(string=? s "t") "T"]
    [(string=? s "u") "U"]
    [(string=? s "v") "V"]
    [(string=? s "w") "W"]
    [(string=? s "x") "X"]
    [(string=? s "y") "Y"]
    [(string=? s "z") "Z"]
    [else s]))

;; find-capital : los -> string
;; outputs a capital letter in a list of strings
(define (find-capital l)
  (cond
    [(empty? l) ""]
    [(cons? l) (if (or (string=? (first l) "A")
                       (string=? (first l) "B")
                       (string=? (first l) "C")
                       (string=? (first l) "D")
                       (string=? (first l) "E")
                       (string=? (first l) "F")
                       (string=? (first l) "G")
                       (string=? (first l) "H")
                       (string=? (first l) "I")
                       (string=? (first l) "J")
                       (string=? (first l) "K")
                       (string=? (first l) "L")
                       (string=? (first l) "M")
                       (string=? (first l) "N")
                       (string=? (first l) "O")
                       (string=? (first l) "P")
                       (string=? (first l) "Q")
                       (string=? (first l) "R")
                       (string=? (first l) "S")
                       (string=? (first l) "T")
                       (string=? (first l) "U")
                       (string=? (first l) "V")
                       (string=? (first l) "W")
                       (string=? (first l) "X")
                       (string=? (first l) "Y")
                       (string=? (first l) "Z")
                       (string=? (first l) "!"))
                   (first l)
                   (find-capital (rest l)))]))

;; send-to-recipients: loiw, iworld, sexp
;; makes list of mail adressed to all in loiw except for the given iworld w/ message s
(define (send-to-recipients l i s)
  (cond
    [(empty? l) empty]
    [(cons? l)
     (if (iworld=? i (first l))
         (send-to-recipients (rest l) i s)
         (cons (make-mail (first l) s) (send-to-recipients (rest l) i s)))]))

;; remove-iworld : list-of-iworlds, iworld --> list-of-iworlds
;; removes a given iworld from the list
(define (remove-iworld l i)
  (cond
    [(empty? l) l]
    [(cons? l) (if (iworld=? (first l) i)
                   (remove-iworld (rest l) i)
                   (cons (first l) (remove-iworld (rest l) i)))]))

;; unique? list, los --> bool
(define (unique? s l)
  (cond
    [(empty? l) true]
    [(cons? l)
     (and (not (symbol=? (first s) (first (first l))))
          (unique? s (rest l)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Universe
(universe DEFAULT
          [port 9000]
          [on-new handle-new]
          [on-msg handle-msg]
          [on-tick tock]
          [on-disconnect disconnect])