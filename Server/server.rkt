#lang racket
(require 2htdp/batch-io)
(require 2htdp/universe)

;; -----------------------------------------------------------------------------------------
;; DEFINITIONS
;; -----------------------------------------------------------------------------------------

;; a universe is a list-of-universe-lists
(define DEFAULT empty)

;; a universe-list is one of:
;; - '(iworld)
;; - '(iworld username status)

;; a message is a '(string string string content) where:
;; the first string is the username of the sender of the message
;; the second string is one of:
;;   - the username of the recipient of the message
;;   - "server" if the message is intended not to be sent to anyone else
;; the third string denotes the purpouse of the message and is one of:
;;  - "login"          : the user attempts to login (server only)
;;  - "login-data"     : data sent to user after attempted login
;;  - "new-user"       : creates a new user (server only)
;;  - "pester"         : a text message intended for another user
;;  - "request"        : a friend request from one user to another
;;  - "request-accept" : user excepts friend request (server-only)
;;  - "friends"        : list of users online and their status'
;;  - "block"          : user blocks other user (server only)
;;  - "login-fail"     : failed login attempt
;;  - "create-fail"    : failed to create user
;;  - "change-status"  : user changes status
;; content is an is the contence of a message which is:
;;  - for login content will be password
;;  - for login-data content will be '(text-color friends pesters requests)
;;  - for new-user content will be '(text-color password)
;;  - for pester content will be '(string text-color)
;;  - for request the content will be empty
;;  - for request-accept the content will be friends-username
;;  - for friends the content will be 'a list of '(username status online?)
;;  - for block content is the username of the friend user wants to block
;;  - for login-fail content is empty
;;  - for create-fail content is empty
;;  - for change status content is a symbol that is the user's status

;; a package is on of:
;; - a message
;; - a list of messages

;; a user-data is a list of strings that is:
;; - '(username text-color password-block friends)

;; a list-of-friends is a list of
;; - (list username, status, new-messages?)

;; a username is a string that represents a user
;; a status is a symbol that represents a user's status
;; a new-messages? is a bool that is true iff there are unviewd messeges

;; All the characters
(define VALID-CHARACTERS
  (list "`" "G" "#" "w" "}" ":" "f" "b" "V" "?" "Y" "l" "6" "P"
        "O" "*" "x" "e" "T" "2" "H" "/" "X" "W" "&" "g" "{" ")"
        "u" "n" "M" "E" "A" "B" "$" "R" "[" "~" ";" "0" "!"
        "N" "C" "D" "I" "." "F" "(" "K" "@" "Q" "=" "j" "o" "%"
        "λ" "a" "]" "r" "h" "_" "m" "p" "z" "c" "4" "i" "t" "v"
        "9" "+" "5" "L" "y" "U" "q" "S" "-" "Z" "8" "^" "," "k"
        "d" "3" "'" "7" "J" "1" "s"))

;; -----------------------------------------------------------------------------------------
;; handle-new
;; -----------------------------------------------------------------------------------------

;; handle-new : universe, iworld --> universe
;; adds iworld to universe when new user starts program
(define (handle-new u w) (cons (list w) u))

;; -----------------------------------------------------------------------------------------
;; disconnect
;; -----------------------------------------------------------------------------------------

;; disconnect : universe- , iworld --> universe
;; removes iworld from universe when user logs out
(define (disconnect u i) (filter (lambda (l) (not (iworld=? (first l) i))) u))

;; -----------------------------------------------------------------------------------------
;; handle-msg
;; -----------------------------------------------------------------------------------------

;; handle-msg : universe, iworld, message --> universe
;; takes a universe, the iworld that sent a message, and that message and updates universe
(define (handle-msg u i m)
  (cond
    ;; pester & timestamp
    [(= (length m) 2)
     (cond
       [(online? (second (first m)) u)
        (make-bundle u (list (make-mail (username->iworld (second (first m)) u) (first m))
                             (make-mail (username->iworld (second (first m)) u) (second m))) empty)]
       [else (write-file
              "pending_messages.txt"
              (string-append
               (read-file "pending_messages.txt")
               " \n"
               (string-append (first (first m)) " " (second (first m)) " " (third (first m)) " "
                              (first (fourth (first m))) " " (second (fourth (first m))) " \n"
                              (first (second m)) " " (second (second m)) " " (third (second m)) " "
                              (first (fourth (second m))) " " (second (fourth (second m)))))) u])]
    ;; login
    [(string=? (third m) "login")
     (cond
       [(or (unique-username? (first m)) (not (password-success? (get-user-data (first m)) (fourth m))))
        (make-bundle u (list (make-mail i (list "server" "user" "login-fail" empty))) empty)]
       [(password-success? (get-user-data (first m)) (fourth m))
        (make-bundle (log-user-in u i (first m)) (list (make-mail i (make-login-data (first m) u))) empty)])]
    ;; new-user
    [(string=? (third m) "new-user")
     (cond
       [(and (unique-username? (first m)) (not (string=? (first m) "pesterchumDev")))
        (add-new-user-data (first m) (first (fourth m)) (second (fourth m))) u]
       [else (make-bundle u (list (make-mail i (list "server" "user" "create-fail" empty))) empty)])]
    ;; pester
    [(string=? (third m) "pester")
     (cond
       [(online? (second m) u) (make-bundle u (list (make-mail (username->iworld (second m) u) m)) empty)]
       [else (write-file
              "pending_messages.txt"
              (string-append
               (read-file "pending_messages.txt")
               " \n"
               (string-append (first m) " " (second m) " " (third m) " "
                              (first (fourth m)) " " (second (fourth m))))) u])]
    ;; request
    [(string=? (third m) "request")
     (cond
       [(online? (second m) u) (make-bundle u (list (make-mail (username->iworld (second m) u) m)) empty)]
       [(unique-username? (second m)) u]
       [else (write-file
              "pending_messages.txt"
              (string-append
               (read-file "pending_messages.txt")
               " \n"
               (string-append (first m) " " (second m) " " (third m)))) u])]
    ;; request-accept
    [(string=? (third m) "request-accept") (add-friends (first m) (fourth m)) u]
    ;; block
    [(string=? (third m) "block") (remove-friends (first m) (fourth m)) u]
    ;; change-status
    [(string=? (third m) "change-status")
     (map (lambda (x) (if (iworld=? i (first x)) (list i (first m) (fourth m)) x)) u)]
    [else u]))

;; add-friends : username, username --> write-file
;; makes two users friends by editing their entries in the user_data file
(define (add-friends u1 u2)
  (local
    [(define (edit-user-data-lolos l)
       (cond
         [(empty? l) empty]
         [(empty? (first l)) (edit-user-data-lolos (rest l))]
         [(string=? u1 (first (first l)))
          (cons
           (append
            (list (first (first l)) (second (first l)) (third (first l)))
            (sort (cons u2 (rest (rest (rest (first l))))) string<?))
           (edit-user-data-lolos (rest l)))]
         [(string=? u2 (first (first l)))
          (cons
           (append
            (list (first (first l)) (second (first l)) (third (first l)))
            (sort (cons u1 (rest (rest (rest (first l))))) string<?))
           (edit-user-data-lolos (rest l)))]
         [else (cons (first l) (edit-user-data-lolos (rest l)))]))]
    (write-file
     "user_data.txt" (los->string (map words->line (edit-user-data-lolos (read-words/line "user_data.txt")))))))

;; remove-friends : username, username --> write-file
;; makes two not friends by editing their entries in the user_data file
(define (remove-friends u1 u2)
  (local
    [(define (edit-user-data-lolos l)
       (cond
         [(empty? l) empty]
         [(empty? (first l)) (edit-user-data-lolos (rest l))]
         [(string=? u1 (first (first l)))
          (cons
           (append (list (first (first l)) (second (first l)) (third (first l)))
                   (filter (lambda (x) (not (string=? x u2))) (rest (rest (rest (first l))))))
           (edit-user-data-lolos (rest l)))]
         [(string=? u2 (first (first l)))
          (cons
           (append (list (first (first l)) (second (first l)) (third (first l)))
                   (filter (lambda (x) (not (string=? x u1))) (rest (rest (rest (first l))))))
           (edit-user-data-lolos (rest l)))]
         [else (cons (first l) (edit-user-data-lolos (rest l)))]))]
    (write-file
     "user_data.txt"
     (los->string (map words->line (edit-user-data-lolos (read-words/line "user_data.txt")))))))

;; words->line : los --> string
;; appends strings in a los w/ spaces between them
(define (words->line l)
  (cond
    [(empty? l) ""]
    [(empty? (rest l)) (first l)]
    [else (string-append (first l) " " (words->line (rest l)))]))

;; online? : username, universe --> bool
;; true iff user w/ given username is logged in
(define (online? s u)
  (cond
    [(empty? u) false]
    [(< (length (first u)) 3) (online? s (rest u))]
    [else (or (string=? (second (first u)) s)
              (online? s (rest u)))]))

;; username->iworld : username, universe --> iworld
;; gets iworld that coresponds to a given username
(define (username->iworld s u)
  (cond
    [(string=? s (second (first u))) (first (first u))]
    [else (username->iworld s (rest u))]))

;; unique-username? : string --> bool
;; true iff username not found in user_data file
(define (unique-username? s)
  (local
    [(define (get-usernames l)
       (cond
         [(empty? l) empty]
         [(empty? (first l)) (get-usernames (rest l))]
         [else (cons (first (first l))
                     (get-usernames (rest l)))]))]
    (= (length (filter (lambda (x) (string=? s x))
                       (get-usernames (read-words/line "user_data.txt")))) 0)))

;; password-sucess? : user-data, string --> bool
;; true iff string decripts user-data passwordblock
(define (password-success? d p)
  (and (> (string-length p) 1)
       
       (string=? (decrypt (substring (third d) 0 (string-length p)) p) p)
       (string=? (decrypt (substring (third d) (string-length p) (if (< (string-length p) 10)
                                                                     (+ (string-length p) 1)
                                                                     (+ (string-length p) 2))) p)
                 (number->string (string-length p)))))

;; get-user-data : username --> user-data
;; takes username and returns user-data from "user_data" file
(define (get-user-data s)
  (local [(define (find-user l)
            (if (string=? (first (first l)) s)
                (first l) (find-user (rest l))))]
    (find-user (read-words/line "user_data.txt"))))

;; make-login-data : username, universe --> message
;; returns login-data message for given user
(define (make-login-data s u) 
  (local
    [(define DATA (get-user-data s))]
    (list "server" s "login-data"
          (list
           (second DATA)
           (get-friends (rest (rest (rest DATA))) s u)
           (append (reverse (get-pesters s)) (dev-notice))
           (get-requests s)))))

;; get-friends : list-of-strings, username, universe --> list-of-friends
;; takes list of friends usernames the users username and the current universe
;; and outputs updated friend-list
(define (get-friends l s u)
  (local
    [(define (get-friend-status n f)
       (cond
         [(empty? f) 'o]
         [(and (> (length (first f)) 1) (string=? n (second (first f)))) (third (first f))]
         [else (get-friend-status n (rest f))]))]
    (cond
      [(empty? l) empty]
      [else (cons
             (list (first l)
                   (get-friend-status (first l) u)
                   (> (length (filter (lambda (x) (string=? (first x) (first l))) (get-pesters s))) 0))
             (get-friends (rest l) s u))])))

;; get-pesters : string --> list-of-pesters
;; takes username and returns a list of all pesters sent to that user from pending_messages file
(define (get-pesters s)
  (map (lambda (x) (list (first x) (second x)
                         "pester"
                         (list (words->line
                                (rest (rest (rest (reverse (rest (reverse x)))))))
                               (first (reverse x)))))
       (filter (lambda (m) (string=? (third m) "pester"))
               (get-messages (read-words/line "pending_messages.txt") s))))

;; get-requests : string --> list-of-requests
;; takes username and returns a list of all messages sent to that user from pending_messages
(define (get-requests s)
  (filter (lambda (m) (string=? (third m) "request"))
          (get-messages (read-words/line "pending_messages.txt") s)))

;; get-messages : list-messages, string --> list-of-messages
;; gets all messages sent to a given username
(define (get-messages l s)
  (cond
    [(empty? l) empty]
    [(empty? (first l)) (get-messages (rest l) s)]
    [else (if (string=? (second (first l)) s)
              (cons (first l) (get-messages (rest l) s))
              (get-messages (rest l) s))]))

;; log-user-in : universe, iworld, username --> universe
;; updates universe to reflect that a user has logged in
(define (log-user-in u i s)
  (if (iworld=? (first (first u)) i)
      (cons (list i s 'c)
            (rest u))
      (cons (first u)
            (log-user-in (rest u) i s))))

;; decrypt : string, string --> string
;; uses second string to try and decrypt first string
(define (decrypt s k)
  (cond
    [(string=? s "") ""]
    [else
     (string-append
      (list-ref VALID-CHARACTERS
                (if (> (length (filter (lambda (x) (string=? x (substring k 0 1))) VALID-CHARACTERS)) 0)         
                    (modulo (- (get-list-ref-number (substring s 0 1) VALID-CHARACTERS 0)
                               (get-list-ref-number (substring k 0 1) VALID-CHARACTERS 0))
                            (length VALID-CHARACTERS)) 41))
      (decrypt (substring s 1) (string-append (substring k 1) (substring k 0 1))))]))

;; encrypt : string, string --> string
;; uses second string to encrypt first string
(define (encrypt s k)
  (cond
    [(string=? s "") ""]
    [else
     (string-append
      (list-ref VALID-CHARACTERS
                (if (> (length (filter (lambda (x) (string=? x (substring k 0 1))) VALID-CHARACTERS)) 0)
                    (modulo (+ (get-list-ref-number (substring s 0 1) VALID-CHARACTERS 0)
                               (get-list-ref-number (substring k 0 1) VALID-CHARACTERS 0))
                            (length VALID-CHARACTERS)) 41))
      (encrypt (substring s 1) (string-append (substring k 1) (substring k 0 1))))]))

;; get-list-ref-number : character, list, number
;; returns number + the position of a given string in a list of strings
(define (get-list-ref-number s l n)
  (cond
    [(and (empty? l) (not (empty? s))) false]
    [(string=? s (first l)) n]
    [else (get-list-ref-number s (rest l) (+ n 1))]))

;; los->string : los --> string
;; writes list-of-strings as a string w/ line breaks in between lists
(define (los->string l)
  (cond
    [(empty? l) ""]
    [else (string-append (first l) "\n" (los->string (rest l)))]))

;; add-new-user-data : username, text-color, password --> write-files
;; adds new-user to user_data file
(define (add-new-user-data s t p)
  (write-file "user_data.txt"
              (string-append (read-file "user_data.txt") "\n"
                             s " " t " " (encrypt (string-append p (number->string (string-length p)) (random-string (- 50
                                                                                                                        (if (< (string-length p) 10)
                                                                                                                            (+ (string-length p) 1)
                                                                                                                            (+ (string-length p) 2))))) p))))
;; random-string : int --> string
;; string of random characters int characters long
(define (random-string i)
  (cond
    [(< i 1) ""]
    [else (string-append (list-ref VALID-CHARACTERS
                                   (random (length VALID-CHARACTERS)))
                         (random-string (- i 1)))]))

;; dev-notice : list-of-lists-of-strings --> list-of-pesters
;; takes list of strings and returns list of pesters that is used to display dev notices
(define (dev-notice)
  (local
    [(define (compile-notice l)
       (cond
         [(empty? l) empty]
         [else
          (cons (list "pesterchumDev" "allUsers"
                      "pester" (list (first l) "black"))
                (compile-notice (rest l)))]))]
    (if (empty? (read-lines "developer_notice.txt"))
        empty
        (compile-notice (read-lines "developer_notice.txt")))))

;; -----------------------------------------------------------------------------------------
;; Tock
;; -----------------------------------------------------------------------------------------

;; tock : universe --> bundle
;; takes a universe and sends updated friend data to all logged in users
(define (tock u)
  (local
    [(define (make-friend-mail l)
       (cond
         [(empty? l) empty]
         [else (if (< (length (first l)) 2)
                   (make-friend-mail (rest l))
                   (cons
                    (make-mail (first (first l))
                               (list "server" (second (first l)) "friends"
                                     (get-friends (rest (rest (rest (get-user-data (second (first l)))))) (second (first l)) u)))
                    (make-friend-mail (rest l))))]))]
    (map (lambda (x) (if (> (length x) 1) (remove-pending-messages (second x)) x)) u)
    (make-bundle u (make-friend-mail u) empty)))

;; remove-pending-messages : string --> write-file
;; removes all messages sent to a given username from the pending_messages file
(define (remove-pending-messages s)
  (if
   (or (empty? (read-words/line "pending_messages.txt"))
       (empty? (filter (lambda (x)
                         (and (> (length x) 0)
                              (not (string=? (second x) s))))
                       (read-words/line "pending_messages.txt"))))
   (write-file "pending_messages.txt" "")
   (write-file
    "pending_messages.txt"
    (los->string (map words->line (filter (lambda (x)
                                            (and (> (length x) 0)
                                                 (not (string=? (second x) s))))
                                          (read-words/line "pending_messages.txt")))))))

;; -----------------------------------------------------------------------------------------
;; Universe
;; -----------------------------------------------------------------------------------------
(universe DEFAULT
          [port 9000]
          [on-new handle-new]
          [on-msg handle-msg]
          [on-tick tock]
          [on-disconnect disconnect])


