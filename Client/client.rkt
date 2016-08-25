#lang racket

(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

;; Start and cease pestering messages

;; -----------------------------------------------------------------------------------------
;; DEFINITIONS
;; -----------------------------------------------------------------------------------------

;; maximum number of pesters from one person user can store
(define MAXPESTERS 27)

;; maxamum number of requests a user can view at a time
(define MAXREQUESTS 18)

;; maximum number of friends user can view at a time
(define VIEWINT 12)

;; a world is one of:
;; - user-login
;; - user-create
;; - user

;; a user-login is a (make-login string string int) where:
;; the first string is the username field
;; the second string is the password field
;; the int is one of:
;;  - 0 : not editing any field
;;  - 1 : editing username field
;;  - 2 : editing password field
(define-struct user-login (username password editing))
(define TEST-USER-LOGIN (make-user-login "testUser" "Dragon" 0))
(define STARTING-WORLD (make-user-login "" "" 0))

;; a user-create is a (make-user-create string string string string) where:
;; the first string is the username field
;; the second string is the text color field
;; the third string is the password field
;; the fourth string is the repeat password field
;; the int is one of:
;;  - 0 : not editing any field
;;  - 1 : editing username field
;;  - 2 : editing text-color field
;;  - 3 : editing password1 field
;;  - 4 : editing password 2 field
(define-struct user-create (username text-color password1 password2 editing))
(define TEST-USER-CREATE (make-user-create "testUser" "blue" "Dragon" "Dragon" 1))

;; a user is a (make-user string symbol string LOLOSS%B LOS string string
;; the first string is the user's username
;; the symbol is the user's status that is one of:
;;   - 'c  : chummy
;;   - 'b  : bully
;;   - 'p  : palsy
;;   - 'pe : peppy
;;   - 'ch : chipper
;;   - 'r  : rancorous
;;   - 'o  : offline
;; the second string is the user's text color
;; the list-of-strings-symbols-and-bools is a list of friends info '(username status new-pesters?)
;; the int shows which group of friends user is viewing, eg:
;; - 1                 : showing first up to VIEWINT friends in friendlist
;; - 2 (if applicable) : showing second up to VIEWINT friends in friendlist etc.
;; the list-of-pesters is a list of pesters the user has recieved sence login
;; the list-of-strings is a list of usernames that are friend requests the user has recieved
;; the third string represents the tab the user is looking at and is one of:
;;   - "none"                : no tab open
;;   - "requests"            : friend requests
;;   - "[friend's username]" : conversation with that friend

;; the fourth string is the text the user is typing
(define-struct user (username status text-color friends viewing pesters requests tab text))
(define TEST-USER
  (make-user "testUser" 'c "blue"
             (list (list "testusersFriend" 'o false)
                   (list "testusersotherFriend" 'c true))
             1 (append
                '(("pesterchumDev"
                   "allUsers" "pester"
                   ("To whom it may concern," "black"))
                  ("pesterchumDev" "allUsers" "pester"
                                   ("" "black"))
                  ("pesterchumDev"
                   "allUsers"
                   "pester"
                   ("Hello." "black")))
                (list
                 (list "testusersFriend" "testUser" "pester"
                       (list "hello," "red"))
                 (list "testUser" "testusersFriend" "pester"
                       (list "hello back" "blue"))
                 (list "testusersFriend" "testUser" "pester"
                       (list "testing longer text message" "red"))))
             (list
              (list "wantstobetusFriend" "testUser" "request")
              (list "alsowantstobetusFriend" "testUser" "request"))
             "requests" "test text is made for testing"))

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
;;  - "change-status"  : user changes status (server only)
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

;; pestertext : string int color --> image
;; takes a string, int, and color and outputs a coresponding text image
(define (pestertext t s c) (text/font t s c "Courier New" 'default 'normal 'bold false))

;; Status Buttons:
(define CHUMMY (overlay/align "left" "middle"
                              (beside (rectangle 27 0 'solid 'gold)
                                      (pestertext " CHUMMY" 14 'black))
                              (bitmap/file "button1.png")))
(define BULLY (overlay/align "left" "middle"
                             (beside (rectangle 27 0 'solid 'gold)
                                     (pestertext " BULLY" 14 'black))
                             (bitmap/file "button1.png")))
(define PALSY (overlay/align "left" "middle"
                             (beside (rectangle 27 0 'solid 'gold)
                                     (pestertext " PALSY" 14 'black))
                             (bitmap/file "button1.png")))
(define PEPPY (overlay/align "left" "middle"
                             (beside (rectangle 27 0 'solid 'gold)
                                     (pestertext " PEPPY" 14 'black))
                             (bitmap/file "button1.png")))
(define CHIPPER (overlay/align "left" "middle"
                               (beside (rectangle 22 0 'solid 'gold)
                                       (pestertext " CHIPPER" 14 'black))
                               (bitmap/file "button1.png")))
(define RANCOROUS (overlay/align "left" "middle"
                                 (beside (rectangle 21 0 'solid 'red)
                                         (pestertext " RANCOROUS" 11 'black))
                                 (bitmap/file "button2.png")))

;; Status Button Highlight
(define HIGHLIGHT (overlay (rectangle 93 38 'outline 'black) (rectangle 95 40 'outline 'black)))

;; Markers
(define OFFLINEMARKER (circle 8 'solid 'gray))
(define CHUMMYMARKER (circle 8 'solid 'yellow))
(define BULLYMARKER (circle 8 'solid 'yellow))
(define PALSYMARKER (circle 8 'solid 'yellow))
(define PEPPYMARKER (circle 8 'solid 'yellow))
(define CHIPPERMARKER (circle 8 'solid 'yellow))
(define RANCOROROUSMARKER (circle 8 'solid 'red))
(define NEWMESSAGEMARKER (circle 8 'solid 'blue))

;; All the characters
(define VALID-CHARACTERS
  (list "`" "G" "#" "w" "}" ":" "f" "b" "V" "?" "Y" "l" "6" "P"
        "O" "*" "x" "e" "T" "2" "H" "/" "X" "W" "&" "g" "{" ")"
        "u" "n" "M" "E" "A" " " "B" "$" "R" "[" "~" ";" "0" "!"
        "N" "C" "D" "I" "." "F" "(" "K" "@" "Q" "=" "j" "o" "%"
        "λ" "a" "]" "r" "h" "_" "m" "p" "z" "c" "4" "i" "t" "v"
        "9" "+" "5" "L" "y" "U" "q" "S" "-" "Z" "8" "^" "," "k"
        "d" "3" "'" "7" "J" "1" "s"))

;; -----------------------------------------------------------------------------------------
;; Render
;; -----------------------------------------------------------------------------------------

;; render : world --> image
;; renders world as an image
(define (render w)
  (cond
    [(user? w) (beside (render-sidebar w)
                       (rectangle 10 600 'solid 'gold)
                       (render-tab w))]
    [(user-login? w)
     (overlay/align
      "middle" "top"
      (above (rectangle 0 30 'solid 'pink)
             (pestertext "PESTERCHUM 0.1" 85 'white)) 
      (overlay
       (above (if (and (string=? (user-login-username w) "")
                       (not (= (user-login-editing w) 1)))
                  (overlay/align "left" "middle"
                                 (pestertext " userName" 14 'gray)
                                 (rectangle 350 30 'solid 'white))
                  (overlay/align
                   "left" "middle"
                   (beside (pestertext (string-append " " (user-login-username w))
                                       14 'black)
                           (if (eq? (user-login-editing w) 1)
                               (rectangle 2 17 'solid 'black)
                               (square 0 'solid 'pink)))
                   (rectangle 350 30 'solid 'white)))
              (rectangle 0 20 'solid 'pink)
              (if (and
                   (string=? (user-login-password w) "")
                   (not (= (user-login-editing w) 2)))
                  (overlay/align
                   "left" "middle"
                   (pestertext " password" 14 'gray)
                   (rectangle 350 30 'solid 'white))
                  (overlay/align
                   "left" "middle"
                   (beside (rectangle 8 0 'solid 'pink)
                           (pestertext
                            (hidden-string
                             (string-length (user-login-password w)))
                            14 'black)
                           (if (eq? (user-login-editing w) 2)
                               (rectangle 2 17 'solid 'black)
                               (square 0 'solid 'pink)))
                   (rectangle 350 30 'solid 'white)))
              (rectangle 0 30 'solid 'pink)
              (beside (overlay
                       (pestertext "CREATE ACCOUNT!" 14 'black)
                       (rectangle 146 26 'solid 'yellow)
                       (rectangle 150 30 'solid 'tan))
                      (rectangle 50 0 'solid 'pink)
                      (overlay
                       (pestertext "LOGIN!" 16 'black)
                       (rectangle 146 26 'solid 'yellow)
                       (rectangle 150 30 'solid 'tan))))
       (rectangle 925 600 'solid 'gold)))]
    [(user-create? w)
     (overlay
      (above (if (and (string=? (user-create-username w) "")
                      (not (= (user-create-editing w) 1)))
                 (overlay/align "left" "middle"
                                (pestertext " userName" 14 'gray)
                                (rectangle 350 30 'solid 'white))
                 (overlay/align "left" "middle"
                                (beside
                                 (pestertext
                                  (string-append " " (user-create-username w))
                                  14 (user-create-text-color w))
                                 (if (eq? (user-create-editing w) 1)
                                     (rectangle 2 17 'solid 'black)
                                     (square 0 'solid 'pink)))
                                (rectangle 350 30 'solid 'white)))
             (rectangle 0 20 'solid 'pink)
             (if (and (string=? (user-create-text-color w) "")
                      (not (= (user-create-editing w) 2)))
                 (overlay/align "left" "middle"
                                (pestertext " text color" 14 'gray)
                                (rectangle 350 30 'solid 'white))
                 (overlay/align "left" "middle"
                                (beside
                                 (pestertext
                                  (string-append " " (user-create-text-color w))
                                  14 (user-create-text-color w))
                                 (if (eq? (user-create-editing w) 2)
                                     (rectangle 2 17 'solid 'black)
                                     (square 0 'solid 'pink)))
                                (rectangle 350 30 'solid 'white)))
             (rectangle 0 20 'solid 'pink)
             (if (and (string=? (user-create-password1 w) "")
                      (not (= (user-create-editing w) 3)))
                 (overlay/align "left" "middle"
                                (pestertext " password" 14 'gray)
                                (rectangle 350 30 'solid 'white))
                 (overlay/align "left" "middle"
                                (beside (rectangle 8 0 'solid 'pink)
                                        (pestertext (hidden-string
                                                     (string-length
                                                      (user-create-password1 w)))
                                                    14 (user-create-text-color w))
                                        (if (eq? (user-create-editing w) 3)
                                            (rectangle 2 17 'solid 'black)
                                            (square 0 'solid 'pink)))
                                (rectangle 350 30 'solid 'white)))
             (rectangle 0 20 'solid 'pink)
             (if (and (string=? (user-create-password2 w) "")
                      (not (= (user-create-editing w) 4)))
                 (overlay/align "left" "middle"
                                (pestertext " re-enter password" 14 'gray)
                                (rectangle 350 30 'solid 'white))
                 (overlay/align "left" "middle"
                                (beside (rectangle 8 0 'solid 'pink)
                                        (pestertext (hidden-string
                                                     (string-length
                                                      (user-create-password2 w)))
                                                    14 (user-create-text-color w))
                                        (if (eq? (user-create-editing w) 4)
                                            (rectangle 2 17 'solid 'black)
                                            (square 0 'solid 'pink)))
                                (rectangle 350 30 'solid 'white)))
             (rectangle 0 30 'solid 'pink)
             (beside (overlay
                      (pestertext "BACK TO LOGIN!" 14 'black)
                      (rectangle 146 26 'solid 'yellow)
                      (rectangle 150 30 'solid 'tan))
                     (rectangle 50 0 'solid 'pink)
                     (overlay
                      (pestertext "CREATE ACCOUNT!" 14 'black)
                      (rectangle 146 26 'solid 'yellow)
                      (rectangle 150 30 'solid 'tan))))
      (rectangle 925 600 'solid 'gold))]
    [else (error 'unexpected_worldstate)]))

;; hidden-string : int --> string
;; produces a string of int "*"s
(define (hidden-string n)
  (cond
    [(= n 0) ""]
    [else (string-append "*" (hidden-string (- n 1)))]))

;; render-tab : world --> image
;; renders the tab portion of the program window
(define (render-tab w)
  (cond
    ;; none tab can be used as notice board
    [(eq? (user-tab w) "none")
     (local
       [(define (render-notice l)
          (cond
            [(empty? l) (square 0 'solid 'pink)]
            [else (above/align "left" (pestertext (first (fourth (first l))) 15 'black)
                               (render-notice (rest l)))]))]
       (if (not (empty? (filter (lambda (p) (string=? (first p) "pesterchumDev"))
                                (user-pesters w))))
           (overlay
            (above (pestertext "A Message from the Developer:" 30 'black)
                   (rectangle 0 30 'solid 'pink)
                   (render-notice (filter (lambda (p) (string=? (first p) "pesterchumDev"))
                                          (user-pesters w))))
            (square 600 'solid 'gold))
           (square 600 'solid 'gold)))]
    [(eq? (user-tab w) "requests")
     (overlay (above
               (overlay/align
                "left" "top"
                (render-requests (first-n (user-requests w) MAXREQUESTS))
                (rectangle 580 540 'solid 'black))
               (rectangle 0 10 'solid 'pink)
               (beside (overlay/align
                        "left" "middle"
                        (beside (pestertext (string-append " " (user-text w)) 12 'white)
                                (rectangle 2 17 'solid 'white))
                        (rectangle 420 30 'solid 'black))
                       (rectangle 10 0 'solid 'pink)
                       (overlay (pestertext "SEND CHUM REQUEST!" 12 'black)
                                (rectangle 146 26 'solid 'yellow)
                                (rectangle 150 30 'solid 'tan))))
              (square 600 'solid 'gold))]
    [else
     (overlay (above (overlay/align
                      "left" "top"
                      (render-pesters
                       (reverse (filter (lambda (x) (or (string=? (second x) (user-tab w))
                                                        (string=? (first x) (user-tab w))))
                                        (user-pesters w))))
                      (rectangle 580 540 'solid 'white))
                     (rectangle 0 10 'solid 'pink)
                     (overlay/align
                      "left" "middle"
                      (beside (pestertext
                               (string-append " " (user-text w))
                               12 (user-text-color w))
                              (rectangle 2 17 'solid (user-text-color w)))
                      (rectangle 580 30 'solid 'white)))
              (square 600 'solid 'gold))]))

;; first-n : list, int --> list
;; returns up to the first int elements in a given list
(define (first-n l i)
  (cond
    [(or (empty? l) (= i 0)) empty]
    [else (cons (first l) (first-n (rest l) (- i 1)))]))

;; render-requests : list-of-requests --> image
;; renders list-of-requests as an image
(define (render-requests l)
  (cond
    [(empty? l) (square 0 'solid 'pink)]
    [else
     (above
      (overlay/align
       "left" "middle"
       (pestertext (string-append " " (first (first l))) 12 'white)
       (overlay/align "right" "middle"
                      (beside
                       (overlay
                        (pestertext "✓" 25 'black)
                        (rectangle 50 20 'solid 'green))
                       (rectangle 10 0 'solid 'pink)
                       (overlay
                        (pestertext "X" 20 'black)
                        (rectangle 50 20 'solid 'red))
                       (rectangle 10 0 'solid 'red))
                      (overlay/align "middle" "top"
                                     (rectangle 580 28 'solid 'black)
                                     (rectangle 580 30 'solid 'gray))))
      (render-requests (rest l)))]))

;; render-pesters : list-of-pesters --> image
;; renders list of pesters as an image
(define (render-pesters l)
  (cond
    [(empty? l) (square 0 'solid 'pink)]
    [else (above
           (overlay/align
            "left" "middle"
            (pestertext (string-append
                         " " (get-initials (first (first l)))
                         ": " (first (fourth (first l))))
                        12 (second (fourth (first l))))
            (rectangle 580 20 'solid 'white))
           (render-pesters (rest l)))]))

;; get-initials : string --> string
;; returns the first letter and first capital letter that is not the first letter
(define (get-initials s)
  (string-append (string-upcase (substring s 0 1))
                 (first-capital (substring s 1 (string-length s)))))

;; first-capital : string --> string
;; gets first capital letter in a string if it exists
(define (first-capital s)
  (cond
    [(string=? s "") ""]
    [else (if (string=? (substring s 0 1) (string-upcase (substring s 0 1)))
              (substring s 0 1) (first-capital (substring s 1 (string-length s))))]))

;; render-sidebar : world --> image
;; renders sidebar portion of the program window
(define (render-sidebar w)
  (overlay
   (above
    (pestertext "PESTERCHUM 0.1" 35 'white)
    (rectangle 0 5 'solid 'pink)
    (beside (rectangle 8 0 'solid 'pink)
            (above/align
             "left"
             (beside (pestertext " CHUMROLL:" 16 'black)
                     (rectangle 180 0 'solid 'pink)
                     (if (> (length (user-friends w)) VIEWINT)
                         (beside (triangle 10 'solid 'black)
                                 (rectangle 0 1 'solid 'pink)
                                 (rotate 180 (triangle 10 'solid 'black)))
                         (square 0 'solid 'pink)))
             (rectangle 0 5 'solid 'pink)
             (above (overlay/align
                     "middle" "top"
                     (render-friends (get-group (user-friends w)
                                                (user-viewing w))
                                     (user-tab w))
                     (rectangle 300 300 'solid 'black))
                    (rectangle 0 7 'solid 'pink)
                    (beside
                     (overlay (pestertext "ADD CHUM!" 14 'black)
                              (if (> (length (user-requests w)) 0)
                                  (rectangle 90 20 'solid 'green)
                                  (rectangle 90 20 'solid 'yellow))
                              (rectangle 94 24 'solid 'tan))
                     (rectangle 10 0 'solid 'pink)
                     (overlay (pestertext "BLOCK!" 14 'black)
                              (rectangle 90 20 'solid 'red)
                              (rectangle 94 24 'solid 'tan))
                     (rectangle 10 0 'solid 'pink)
                     (overlay (pestertext "PESTER!" 14 'black)
                              (rectangle 90 20 'solid 'yellow)
                              (rectangle 94 24 'solid 'tan))))
             (rectangle 0 12 'solid 'pink)
             (pestertext " MYCHUMHANDLE:" 16 'black)
             (rectangle 0 5 'solid 'pink)
             (overlay/align
              "left" "middle"
              (beside (rectangle 10 0 'solid 'pink)
                      (cond
                        [(eq? (user-status w) 'c) CHUMMYMARKER]
                        [(eq? (user-status w) 'b) BULLYMARKER]
                        [(eq? (user-status w) 'p) PALSYMARKER]
                        [(eq? (user-status w) 'pe) PEPPYMARKER]
                        [(eq? (user-status w) 'ch) CHIPPERMARKER]
                        [(eq? (user-status w) 'r) RANCOROROUSMARKER]
                        [else (error 'unexpected_user_status)])
                      (rectangle 8 0 'solid 'pink)
                      (pestertext (user-username w) 14 'white))
              (rectangle 300 25 'solid 'black))
             (rectangle 0 13 'solid 'pink)
             (pestertext " MOOD:" 16 'black)
             (rectangle 0 2 'solid 'pink)
             (render-status-buttons (user-status w)))))
   (rectangle 315 600 'solid 'gold)))

;; render-friends : LOSS&B string --> image
;; renders friends list (LOSS&B) as image for sidebar
;; highlights friend as specified by tab if necessary
(define (render-friends l t)
  (cond
    [(empty? l) (square 0 'solid 'pink)]
    [(cons? l)
     (above
      (overlay/align
       "left" "middle"
       (beside (rectangle 5 0 'solid 'pink)
               (cond
                 [(eq? (second (first l)) 'o) OFFLINEMARKER]
                 [(eq? (second (first l)) 'c) CHUMMYMARKER]
                 [(eq? (second (first l)) 'b) BULLYMARKER]
                 [(eq? (second (first l)) 'p) PALSYMARKER]
                 [(eq? (second (first l)) 'pe) PEPPYMARKER]
                 [(eq? (second (first l)) 'ch) CHIPPERMARKER]
                 [(eq? (second (first l)) 'r) RANCOROROUSMARKER]
                 [else (error 'unexpected_friend_status)])
               (rectangle 5 0 'solid 'pink)
               (pestertext (first (first l)) 14
                           (if (eq? (second (first l)) 'o) 'gray 'white)))
       (overlay/align
        "right" "middle"
        (beside (if (third (first l)) NEWMESSAGEMARKER
                    (square 0 'solid 'pink)) (rectangle 5 0 'solid 'pink))
        (if (eq? (first (first l)) t) (rectangle 300 25 'solid (make-color 80 80 80))
            (rectangle 300 25 'solid 'black))))
      (render-friends (rest l) t))]))

;; get-group : list int --> list
;; returns the intth group of VIEWINT items in list
(define (get-group l i)
  (remove-from-end (remove-from-front l (* (- i 1) VIEWINT))
                   (- (length (remove-from-front l (* (- i 1) VIEWINT))) 25)))

;; remove-from-front : list int --> list
;; removes int elements from the front of list
(define (remove-from-front l i)
  (cond
    [(= i 0) l]
    [else (remove-from-front (rest l) (- i 1))]))

;; remove-from-end : list int --> list
;; removes int elements from the end of list
(define (remove-from-end l i)
  (cond
    [(<= i 0) l]
    [else (remove-from-end (reverse (rest (reverse l))) (- i 1))]))

;; render-status-buttons : symbol --> image
;; renders status button grid with higlight on status indicated by symbol
(define (render-status-buttons s)
  (cond
    [(symbol=? s 'c)
     (above
      (beside (overlay HIGHLIGHT CHUMMY) (rectangle 10 0 'solid' pink)
              BULLY (rectangle 10 0 'solid 'pink) PALSY)
      (rectangle 0 10 'solid 'pink)
      (beside PEPPY (rectangle 10 0 'solid 'pink) CHIPPER
              (rectangle 10 0 'solid' pink) RANCOROUS))]
    [(symbol=? s 'b)
     (above (beside CHUMMY (rectangle 10 0 'solid' pink)
                    (overlay HIGHLIGHT BULLY) (rectangle 10 0 'solid 'pink) PALSY)
            (rectangle 0 10 'solid 'pink)
            (beside PEPPY (rectangle 10 0 'solid 'pink)
                    CHIPPER (rectangle 10 0 'solid' pink) RANCOROUS))]
    [(symbol=? s 'p)
     (above (beside CHUMMY (rectangle 10 0 'solid' pink) BULLY
                    (rectangle 10 0 'solid 'pink) (overlay HIGHLIGHT PALSY))
            (rectangle 0 10 'solid 'pink)
            (beside PEPPY (rectangle 10 0 'solid 'pink) CHIPPER
                    (rectangle 10 0 'solid' pink) RANCOROUS))]
    [(symbol=? s 'pe)
     (above (beside CHUMMY (rectangle 10 0 'solid' pink)
                    BULLY (rectangle 10 0 'solid 'pink) PALSY)
            (rectangle 0 10 'solid 'pink)
            (beside (overlay HIGHLIGHT PEPPY) (rectangle 10 0 'solid 'pink)
                    CHIPPER (rectangle 10 0 'solid' pink) RANCOROUS))]
    [(symbol=? s 'ch)
     (above (beside CHUMMY (rectangle 10 0 'solid' pink) BULLY
                    (rectangle 10 0 'solid 'pink) PALSY)
            (rectangle 0 10 'solid 'pink)
            (beside PEPPY (rectangle 10 0 'solid 'pink) (overlay HIGHLIGHT CHIPPER)
                    (rectangle 10 0 'solid' pink) RANCOROUS))]
    [(symbol=? s 'r)
     (above (beside CHUMMY (rectangle 10 0 'solid' pink) BULLY
                    (rectangle 10 0 'solid 'pink) PALSY)
            (rectangle 0 10 'solid 'pink)
            (beside PEPPY (rectangle 10 0 'solid 'pink) CHIPPER
                    (rectangle 10 0 'solid' pink) (overlay HIGHLIGHT RANCOROUS)))]
    [else (above (beside CHUMMY (rectangle 10 0 'solid' pink) BULLY
                         (rectangle 10 0 'solid 'pink) PALSY)
                 (rectangle 0 10 'solid 'pink)
                 (beside PEPPY (rectangle 10 0 'solid 'pink) CHIPPER
                         (rectangle 10 0 'solid' pink) RANCOROUS))]))

;; -----------------------------------------------------------------------------------------
;; Handle-Key
;; -----------------------------------------------------------------------------------------

;; handle-key : world keyevent --> world
;; handles key input and updates the world if necessary
(define (handle-key w k)
  (cond
    [(or (key=? k "shift") (key=? k "start") (key=? k "cancel")
         (key=? k "clear") (key=? k "menu") (key=? k "pause")
         (key=? k "capital") (key=? k "prior") (key=? k "next")
         (key=? k "end") (key=? k "home") (key=? k "select")
         (key=? k "print") (key=? k "execute") (key=? k "snapshot")
         (key=? k "insert") (key=? k "help") (key=? k "f1")
         (key=? k "f2") (key=? k "f3") (key=? k "f4") (key=? k "f5")
         (key=? k "f6") (key=? k "f7") (key=? k "f8") (key=? k "f9")
         (key=? k "f10") (key=? k "f11") (key=? k "f12")
         (key=? k "numlock") (key=? k "scroll") (key=? k "wheel-left")
         (key=? k "wheel-right") (key=? k "control") (key=? k "rcontrol")
         (key=? k "rshift") (key=? k "\t")) w]
    [(user? w)
     (cond
       [(key=? k "escape") w]
       [(or (key=? k "left") (key=? k "right")
            (key=? k "up") (key=? k "down") 
            (key=? k "wheel-up") (key=? k "wheel-down"))
        (make-user (user-username w) (user-status w) (user-text-color w) (user-friends w)
                   (update-viewing (user-viewing w) (length (user-friends w)))
                   (user-pesters w) (user-requests w) (user-tab w) (user-text w))]
       [(key=? k "\b")
        (make-user (user-username w) (user-status w) (user-text-color w) (user-friends w)
                   (user-viewing w) (user-pesters w) (user-requests w) (user-tab w)
                   (if (string=? (user-text w) "")
                       "" (substring (user-text w) 0 (- (string-length (user-text w)) 1))))]
       [(key=? k "\r")
        (cond
          [(or (string=? (user-text w) "") (eq? (user-tab w) "none")) w]
          [(eq? (user-tab w) "requests")
           (make-package
            (make-user (user-username w) (user-status w) (user-text-color w) (user-friends w)
                       (user-viewing w) (user-pesters w) (user-requests w) (user-tab w) "")
            (list (user-username w) (user-text w) "request" empty))]
          [else
           (make-package
            (make-user (user-username w) (user-status w) (user-text-color w)
                       (user-friends w) (user-viewing w)
                       (if (>= (length (filter (lambda (x)
                                                 (or (eq? (second x) (user-tab w))
                                                     (eq? (first x) (user-tab w))))
                                               (user-pesters w))) MAXPESTERS)
                           (cons
                            (list (user-username w) (user-tab w) "pester"
                                  (list (user-text w) (user-text-color w)))
                            (append (reverse
                                     (rest
                                      (reverse
                                       (filter (lambda (x) (or (eq? (second x) (user-tab w))
                                                               (eq? (first x) (user-tab w))))
                                               (user-pesters w)))))
                                    (filter (lambda (x) (not (or (eq? (second x) (user-tab w))
                                                                 (eq? (first x) (user-tab w)))))
                                            (user-pesters w))))
                           (cons (list (user-username w) (user-tab w) "pester"
                                       (list (user-text w) (user-text-color w))) (user-pesters w)))
                       (user-requests w) (user-tab w) "")
            (list (user-username w) (user-tab w) "pester"
                  (list (user-text w) (user-text-color w))))])]
       [(= (length (filter (lambda (x) (string=? x k)) VALID-CHARACTERS)) 0) w]
       [else (if (or (>= (image-width (pestertext (string-append "  " (user-text w)) 12 'white)) 575)
                     (and (eq? (user-tab w) "requests")
                          (>= (image-width (pestertext (string-append "  " (user-text w)) 12 'white)) 415))
                     (eq? (user-tab w) "none"))
                 w (make-user (user-username w) (user-status w)
                              (user-text-color w) (user-friends w)
                              (user-viewing w) (user-pesters w) (user-requests w)
                              (user-tab w) (string-append (user-text w) k)))])]
    [(user-login? w)
     (cond
       [(or (key=? k "up") (key=? k "down") (key=? k "left") (key=? k "right")
            (key=? k "wheel-up") (key=? k "wheel-down"))
        (make-user-login (user-login-username w)
                         (user-login-password w)
                         (if (eq? (user-login-editing w) 1) 2 1))]
       [(key=? k "\r") (make-package (make-user-login "" "" 1)
                                     (list (user-login-username w)
                                           "server" "login" (user-login-password w)))]
       [(key=? k "\b")
        (if (eq? (user-login-editing w) 2)
            (make-user-login (user-login-username w)
                             (if (string=? (user-login-password w) "")
                                 "" (substring (user-login-password w) 0
                                               (- (string-length (user-login-password w)) 1))) 2)
            (make-user-login (if (string=? (user-login-username w) "")
                                 "" (substring (user-login-username w) 0
                                               (- (string-length (user-login-username w)) 1)))
                             (user-login-password w) 1))]
       [(= (length (filter (lambda (x) (string=? x k)) VALID-CHARACTERS)) 0) w]
       [else (cond
               [(and (eq? (user-login-editing w) 1)
                     (not (> (image-width
                              (pestertext
                               (string-append "  "
                                              (user-login-username w)) 14 'white)) 346)))
                (make-user-login (string-append (user-login-username w) k)
                                 (user-login-password w) 1)]
               [(and (eq? (user-login-editing w) 2)
                     (not (> (image-width (pestertext
                                           (string-append "  " (user-login-password w))
                                           14 'white)) 346)))
                (make-user-login (user-login-username w)
                                 (string-append (user-login-password w) k) 2)]
               [else w])])]
    [(user-create? w)
     (cond
       [(or (key=? k "up") (key=? k "down") (key=? k "left") (key=? k "right")
            (key=? k "wheel-up") (key=? k "wheel-down") (key=? k " "))
        (make-user-create (user-create-username w) (user-create-text-color w)
                          (user-create-password1 w) (user-create-password2 w)
                          (if (or (eq? (user-create-editing w) 4)
                                  (eq? (user-create-editing w) 0))
                              1 (+ (user-create-editing w) 1)))]
       [(key=? k "\b")
        (cond
          [(eq? (user-create-editing w) 1)
           (make-user-create (if (string=? (user-create-username w) "")
                                 "" (substring (user-create-username w)
                                               0 (- (string-length (user-create-username w)) 1)))
                             (user-create-text-color w) (user-create-password1 w)
                             (user-create-password2 w) 1)]
          [(eq? (user-create-editing w) 2)
           (make-user-create (user-create-username w)
                             (if (string=? (user-create-text-color w) "")
                                 "" (substring (user-create-text-color w)
                                               0 (- (string-length (user-create-text-color w)) 1)))
                             (user-create-password1 w) (user-create-password2 w) 2)]
          [(eq? (user-create-editing w) 3)
           (make-user-create (user-create-username w) (user-create-text-color w)
                             (if (string=? (user-create-password1 w) "")
                                 "" (substring (user-create-password1 w)
                                               0 (- (string-length (user-create-password1 w)) 1)))
                             (user-create-password2 w) 3)]
          [(eq? (user-create-editing w) 4)
           (make-user-create (user-create-username w) (user-create-text-color w) (user-create-password1 w)
                             (if (string=? (user-create-password2 w) "")
                                 "" (substring (user-create-password2 w)
                                               0 (- (string-length (user-create-password2 w)) 1))) 4)]
          [else w])]
       [(key=? k "\r") (if (and (string=? (user-create-password1 w) (user-create-password2 w))
                                (>= (string-length (user-create-username w)) 2)
                                (>= (string-length (user-create-password1 w)) 2))
                           (make-package (make-user-login "" "" 1)
                                         (list (user-create-username w)
                                               "server" "new-user"
                                               (list (user-create-text-color w)
                                                     (user-create-password1 w))))
                           (make-user-create (user-create-username w)
                                             (user-create-text-color w) "" "" 3))]
       [(= (length (filter (lambda (x) (string=? x k)) VALID-CHARACTERS)) 0) w]
       [else (cond
               [(and (not (> (image-width (pestertext (string-append "  " (user-create-username w))
                                                      14 'white)) 346))
                     (eq? (user-create-editing w) 1))
                (make-user-create (string-append (user-create-username w) k) (user-create-text-color w)
                                  (user-create-password1 w) (user-create-password2 w) (user-create-editing w))]
               [(and (not (> (image-width (pestertext (string-append "  " (user-create-text-color w))
                                                      14 'white)) 346))
                     (eq? (user-create-editing w) 2))
                (make-user-create (user-create-username w) (string-append (user-create-text-color w) k)
                                  (user-create-password1 w) (user-create-password2 w)
                                  (user-create-editing w))]
               [(and (not (> (image-width (pestertext (string-append "  " (user-create-password1 w))
                                                      14 'white)) 346))
                     (eq? (user-create-editing w) 3))
                (make-user-create (user-create-username w) (user-create-text-color w)
                                  (string-append (user-create-password1 w) k)
                                  (user-create-password2 w) (user-create-editing w))]
               [(and (not (> (image-width (pestertext (string-append "  " (user-create-password2 w))
                                                      14 'white)) 346))
                     (eq? (user-create-editing w) 4))
                (make-user-create (user-create-username w) (user-create-text-color w)
                                  (user-create-password1 w) (string-append (user-create-password2 w) k)
                                  (user-create-editing w))]
               [else w])])]
    [else (error 'unexpected_worldstate)]))

;; update-viewing : int int --> int
;; takes one int the int that shows which group of VIEWINT friends the user is viewing and one int that is the
;; lenght of the user's friendlist and outputs and updates the first int to show the next friend group if necessary
(define (update-viewing v f) (if (>= (* v VIEWINT) f) 1 (+ f 1)))

;; -----------------------------------------------------------------------------------------
;; Recieve Message
;; -----------------------------------------------------------------------------------------

;; recieve-message : world, message --> world
;; Updates world according to the message recieved
(define (receive-message w m)
  (cond
    [(string=? (third m) "pester")
     (make-user (user-username w)
                (user-status w)
                (user-text-color w)
                (if (not (string=? (user-tab w) (first m)))
                    (sort-new-messages (user-friends w) (list m))
                    (user-friends w))
                (user-viewing w)
                (if (>= (length (filter (lambda (x) (or (string=? (second x) (first m))
                                                        (string=? (first x) (first m))))
                                        (user-pesters w))) MAXPESTERS)
                    (cons m
                          (append
                           (reverse
                            (rest (reverse (filter (lambda (x) (or (string=? (second x) (first m))
                                                                   (string=? (first x) (first m))))
                                                   (user-pesters w)))))
                           (filter (lambda (x) (not (string=? (first x) (first m)))) (user-pesters w))))
                    (cons m (user-pesters w)))
                (user-requests w)
                (user-tab w)
                (user-text w))]
    [(string=? (third m) "login-data")
     (make-user (second m) 'c (first (fourth m))
                (sort-new-messages (second (fourth m))
                                   (third (fourth m)))
                1 (third (fourth m))
                (fourth (fourth m)) "none" "")]
    [(string=? (third m) "request")
     (make-user (user-username w)
                (user-status w)
                (user-text-color w)
                (user-friends w)
                (user-viewing w)
                (user-pesters w)
                (cons m (user-requests w))
                (user-tab w)
                (user-text w))]
    [(string=? (third m) "friends")
     (make-user (user-username w)
                (user-status w)
                (user-text-color w)
                (update-friends (user-friends w) (fourth m))
                (user-viewing w)
                (user-pesters w)
                (user-requests w)
                (user-tab w)
                (user-text w))]
    [(string=? (third m) "login-fail") (make-user-login "Login failed!" "" 0)]
    [(string=? (third m) "create-fail") (make-user-login "Account creation failed!" "" 0)]
    [else (error 'unexpected_message)]))


;; sort-new-messages : LOSS&B LOP --> LOSS&B
;; sets corisponding values of bools in LOSS&B to true
;; iff list of pesters has messages from those users
(define (sort-new-messages f p)
  (cond
    [(empty? f) empty]
    [(cons? f)
     (if (> (length (filter (lambda (x) (string=? (first x) (first (first f)))) p)) 0)
         (cons (list (first (first f)) (second (first f)) true)
               (sort-new-messages (rest f) p))
         (cons (first f) (sort-new-messages (rest f) p)))]))

;; update-friends : LOSS&B LOSS&B --> LOSS&B
;; updates friend list with friend data from server
(define (update-friends friends server)
  (map (lambda (sf)
         (if (> (length (filter (lambda (x) (string=? (first sf) (first x))) friends)) 0)
             (list
              (first sf)
              (second sf)
              (third (first (filter (lambda (x) (string=? (first sf) (first x))) friends))))
             sf))
       server))

;; handle-mouse : world, mouse-x, mouse-y, mouse-event --> world
;; handles mouse input
(define (handle-mouse w x y m)
  (cond
    [(not (eq? m "button-down")) w]
    [(user? w) (handle-user-mouse w x y)]
    [(user-login? w)
     (cond
       ;; select username field
       [(< y 270)
        (make-user-login (user-login-username w)
                         (user-login-password w)
                         1)]
       ;; select password field
       [(and (> y 270) (< y 330))
        (make-user-login (user-login-username w)
                         (user-login-password w)
                         2)]
       ;; create account & login buttons
       [(and (> y 340) (< y 370))
        (if (and (> x 290) (< x 440))
            (make-user-create "" "" "" "" 0)
            (if (and (> x 490) (< x 640))
                (make-package
                 (make-user-login "" "" 0)
                 (list (user-login-username w) "server" "login" (user-login-password w)))
                w))]
       [else w])]
    [(user-create? w)
     (cond
       ;; edit username field
       [(< y 220) (make-user-create (user-create-username w) (user-create-text-color w)
                                    (user-create-password1 w) (user-create-password2 w) 1)]
       ;; edit text-color field
       [(< y 270) (make-user-create (user-create-username w) (user-create-text-color w)
                                    (user-create-password1 w) (user-create-password2 w) 2)]
       ;; edit password1 field
       [(< y 320) (make-user-create (user-create-username w) (user-create-text-color w)
                                    (user-create-password1 w) (user-create-password2 w) 3)]
       ;; edit password2 field
       [(< y 365) (make-user-create (user-create-username w) (user-create-text-color w)
                                    (user-create-password1 w) (user-create-password2 w) 4)]
       ;; back to login and create account buttons
       [(and (> y 390) (< y 420))
        (if (and (> x 290) (< x 440))
            (make-user-login "" "" 0)
            (if (and (> x 490) (< x 640))
                (make-package
                 (make-user-login "" "" 0)
                 (list (user-create-username w) "server" "new-user"
                       (list (user-create-text-color w) (user-create-password1 w))))
                w))]
       [else w])]  
    [else (error 'unexpected_worldstate!)]))

;; handle-user-mouse : user, int, int, mouse-event --> world
;; applies mouse input to user
(define (handle-user-mouse w x y)
  (cond
    ;; status buttons
    [(and (> y 500) (< x 310))
     (make-package
      (make-user
       (user-username w)
       (cond
         [(< x 110) (if (< y 550) 'c 'pe)]
         [(< x 220) (if (< y 550) 'b 'ch)]
         [else (if (< y 550) 'p 'r)])
       (user-text-color w) (user-friends w) (user-viewing w)
       (user-pesters w) (user-requests w) (user-tab w) (user-text w))
      (list (user-username w) "server" "change-status"
            (cond
              [(< x 110) (if (< y 550) 'c 'pe)]
              [(< x 220) (if (< y 550) 'b 'ch)]
              [else (if (< y 550) 'p 'r)])))]
    ;; add chum, block, pester buttons
    [(and (> y 390) (< y 410) (< x 310))
     (cond
       ;; add chum button
       [(and (> x 10) (< x 110))
        (if (string=? (user-tab w) "requests")
            w
            (make-user (user-username w) (user-status w) (user-text-color w)
                       (user-friends w) (user-viewing w) (user-pesters w)
                       (user-requests w) "requests" ""))]
       ;; block button
       [(and (> x 110) (< x 210))
        (if (or (string=? (user-tab w) "requests") (string=? (user-tab w) "none"))
            w
            (make-package
             (make-user (user-username w) (user-status w) (user-text-color w)
                        (user-friends w) (user-viewing w) (user-pesters w)
                        (user-requests w) "none" "")
             (list (user-username w) "server" "block" (user-tab w))))]
       ;; pester button
       [else
        (if (or (string=? (user-text w) "") (string=? (user-tab w) "requests") (string=? (user-tab w) "none"))
            w
            (make-package
             (make-user (user-username w) (user-status w) (user-text-color w)
                        (user-friends w) (user-viewing w)
                        (if (>= (length (filter (lambda (x)
                                                  (or (string=? (second x) (user-tab w))
                                                      (string=? (first x) (user-tab w))))
                                                (user-pesters w))) MAXPESTERS)
                            (cons
                             (list (user-username w) (user-tab w) "pester"
                                   (list (user-text w) (user-text-color w)))
                             (append
                              (reverse
                               (rest
                                (reverse
                                 (filter (lambda (x) (or (string=? (second x) (user-tab w))
                                                         (string=? (first x) (user-tab w))))
                                         (user-pesters w)))))
                              (filter (lambda (x) (not (or (string=? (second x) (user-tab w))
                                                           (string=? (first x) (user-tab w)))))
                                      (user-pesters w))))
                            (cons
                             (list (user-username w) (user-tab w) "pester"
                                   (list (user-text w) (user-text-color w)))
                             (user-pesters w)))
                        (user-requests w) (user-tab w) "")
             (list (user-username w) (user-tab w) "pester"
                   (list (user-text w) (user-text-color w)))))])]
    ;; send request button
    [(and (> x 760) (> y 560) (string=? (user-tab w) "requests") (not (string=? (user-text w) "")))
     (make-package
      (make-user (user-username w) (user-status w) (user-text-color w) (user-friends w)
                 (user-viewing w) (user-pesters w) (user-requests w) (user-tab w) "")
      (list (user-username w) (user-text w) "request" empty))]
    ;; accept & decline requests
    [(and (string=? (user-tab w) "requests") (> x 795) (< x 905) (> y 10) (< y 550))
     (local
       [(define (request-action x y l)
          (cond
            [(empty? l) (list false "none")]
            [else (if (>= y 30)
                      (request-action x (- y 30) (rest l))
                      (list (if (< x 850) true false) (first (first l))))]))]
       (cond
         [(string=? (second (request-action x (- y 10) (user-requests w))) "none") w]
         [(first (request-action x (- y 10) (user-requests w)))
          (make-package
           (make-user (user-username w) (user-status w) (user-text-color w)
                      (user-friends w) (user-viewing w) (user-pesters w)
                      (filter (lambda (z)
                                (not (eq? (first z)
                                          (second (request-action x (- y 10) (user-requests w))))))
                              (user-requests w)) (user-tab w) (user-text w))
           (list (user-username w) "server" "request-accept"
                 (second (request-action x (- y 10) (user-requests w)))))]
         [else (make-user
                (user-username w) (user-status w) (user-text-color w)
                (user-friends w) (user-viewing w) (user-pesters w)
                (filter (lambda (z) (not (eq? (first z)
                                              (second (request-action x (- y 10) (user-requests w))))))
                        (user-requests w)) (user-tab w) (user-text w))]))]
    ;; select friend from friendlist
    [(and (> x 10) (< x 310) (> y 76) (< y 378))
     (local
       [(define (select-friend i l)
          (cond
            [(empty? l) "none"]
            [else (if (< i 25)
                      (first (first l))
                      (select-friend (- i 25) (rest l)))]))]
       (make-user (user-username w) (user-status w) (user-text-color w)
                  (map (lambda (f)
                         (if (string=? (first f)
                                       (select-friend (- y 78)
                                                      (get-group
                                                       (user-friends w)
                                                       (user-viewing w))))
                             (list (first f) (second f) false)
                             f)) (user-friends w))
                  (user-viewing w) (user-pesters w)
                  (user-requests w) (select-friend
                                     (- y 78)
                                     (get-group (user-friends w) (user-viewing w)))
                  ""))]
    ;; cycle friends
    [(and (> (length (user-friends w)) VIEWINT) (> y 50) (< x 78) (> x 270) (< x 310))
     (make-user (user-username w) (user-status w) (user-text-color w) (user-friends w)
                (update-viewing (user-viewing w) (length (user-friends w)))
                (user-pesters w) (user-requests w) (user-tab w) (user-text w))]
    [else w]))


;; impose-grid : image, int --> image
;; imposes a grid of int x int boxes over a given image
(define (impose-grid i n)
  (local
    [(define (get-rows x y n)
       (cond
         [(<= y 0) (square 0 'solid 'pink)]
         [else
          (above (rectangle x 1 'solid 'red)
                 (rectangle 0 (- n 1) 'solid 'pink)
                 (get-rows x (- y n) n))]))
     (define (get-columns x y n)
       (cond
         [(<= x 0) (square 0 'solid 'pink)]
         [else
          (beside (rectangle 1 y 'solid 'red)
                  (rectangle (- n 1) 0 'solid 'pink)
                  (get-columns (- x n) y n))]))]
    (overlay/align
     "left" "top"
     (get-rows (image-width i) (image-height i) n)
     (overlay/align
      "left" "top"
      (get-columns (image-width i) (image-height i) n) i))))

;; black-out : image, int, int --> image
;; blacks out everything to the left of the first int and everything above the second
(define (black-out i x y)
  (overlay/align
   "left" "top"
   (rectangle x (image-height i) 'solid 'red)
   (overlay/align
    "left" "top"
    (rectangle (image-width i) y 'solid 'red)
    i)))

;; -----------------------------------------------------------------------------------------
;; Main
;; -----------------------------------------------------------------------------------------

;; main
(define (main w)
  (big-bang w
            [name "Pesterchum v.0.1"]
            [register (read-file "server_ip.txt")]
            [port 9000]
            [on-receive receive-message]
            [to-draw render]
            [on-key handle-key]
            [on-mouse handle-mouse]))

(main STARTING-WORLD)
