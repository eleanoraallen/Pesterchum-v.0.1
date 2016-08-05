#lang racket

(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

;; -----------------------------------------------------------------------------------------
;; DEFINITIONS
;; -----------------------------------------------------------------------------------------

;; maximum number of pesters from one person user can store
(define MAXPESTERS 10)

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
;;  - 1 : editing username field
;;  - 2 : editing password field
(define-struct user-login (username password editing))
(define TEST-USER-LOGIN (make-user-login "testUser" "Dragon" 1))

;; a user-create is a (make-user-create string string string string) where:
;; the first string is the username field
;; the second string is the text color field
;; the third string is the password field
;; the fourth string is the repeat password field
;; the int is one of:
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
(define TEST-USER (make-user "testUser" 'c "blue"
                             (list (list "testusersFriend" 'o false)
                                   (list "testusersotherFriend" 'c true))
                             1 empty empty "none" ""))

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
;; content is an is the contence of a message which is:
;;  - for login content will be password
;;  - for login-data content will be '(text-color friends pesters requests)
;;  - for new-user content will be '(text-color password)
;;  - for pester content will be '(string text-color)
;;  - for request the content will be empty
;;  - for request-accept the content will be friends-username
;;  - for friends the content will be 'a list of '(username status)

;; pestertext : string int color --> image
;; takes a string, int, and color and outputs a coresponding text image
(define (pestertext t s c) (text/font t s c "Courier New" 'default 'normal 'bold false))

;; Status Buttons:
(define CHUMMY (overlay/align "left" "middle" (beside (rectangle 27 0 'solid 'gold) (pestertext " CHUMMY" 14 'black)) (bitmap/file "button1.png")))
(define BULLY (overlay/align "left" "middle" (beside (rectangle 27 0 'solid 'gold) (pestertext " BULLY" 14 'black)) (bitmap/file "button1.png")))
(define PALSY (overlay/align "left" "middle" (beside (rectangle 27 0 'solid 'gold) (pestertext " PALSY" 14 'black)) (bitmap/file "button1.png")))
(define PEPPY (overlay/align "left" "middle" (beside (rectangle 27 0 'solid 'gold) (pestertext " PEPPY" 14 'black)) (bitmap/file "button1.png")))
(define CHIPPER (overlay/align "left" "middle" (beside (rectangle 22 0 'solid 'gold) (pestertext " CHIPPER" 14 'black)) (bitmap/file "button1.png")))
(define RANCOROUS (overlay/align "left" "middle" (beside (rectangle 21 0 'solid 'red) (pestertext " RANCOROUS" 11 'black)) (bitmap/file "button2.png")))

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

;; -----------------------------------------------------------------------------------------
;; Render
;; -----------------------------------------------------------------------------------------

;; render : world --> image
;; renders world as an image
(define (render w)
  (cond
    [(user? w) (beside (render-sidebar w) (rectangle 10 600 'solid 'gold) (render-tab w))]
    [else (error 'unexpected_worldstate)]))

;; render-tab : world --> image
;; renders the tab portion of the program window
(define (render-tab w) (overlay (above (rectangle 580 540 'solid 'white) (rectangle 0 10 'solid 'pink) (rectangle 580 30 'solid 'white))
                                (square 600 'solid 'gold)))

;; render-sidebar : world --> image
;; renders sidebar portion of the program window
(define (render-sidebar w)
  (overlay
   (above (pestertext "PESTERCHUM 0.1" 35 'white)
          (rectangle 0 5 'solid 'pink)
          (beside (rectangle 8 0 'solid 'pink)
                  (above/align "left"
                       (pestertext " CHUMROLL:" 16 'black)
                       (rectangle 0 5 'solid 'pink)
                       (above (overlay/align "middle" "top"
                                      (render-friends (get-group (user-friends w) (user-viewing w)) (user-tab w))
                                      (rectangle 300 300 'solid 'black))
                       (rectangle 0 7 'solid 'pink)
                       (beside
                        (overlay (pestertext "ADD CHUM!" 14 'black) (rectangle 90 20 'solid 'yellow) (rectangle 94 24 'solid 'tan))
                        (rectangle 10 0 'solid 'pink)
                        (overlay (pestertext "BLOCK!" 14 'black) (rectangle 90 20 'solid 'red) (rectangle 94 24 'solid 'tan))
                        (rectangle 10 0 'solid 'pink)
                       (overlay (pestertext "PESTER!" 14 'black) (rectangle 90 20 'solid 'yellow) (rectangle 94 24 'solid 'tan))))
                       (rectangle 0 12 'solid 'pink)
                       (pestertext " MYCHUMHANDLE:" 16 'black)
                       (rectangle 0 5 'solid 'pink)
                       (overlay/align "left" "middle"
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
;; renders friends list (LOSS&B) as image for sidebar and highlights friend as specified by tab if necessary
(define (render-friends l t)
  (cond
    [(empty? l) (square 0 'solid 'pink)]
    [(cons? l)
     (above (overlay/align "left" "middle"
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
                                   (pestertext (first (first l)) 14 (if (eq? (second (first l)) 'o) 'gray 'white)))
                           (overlay/align "right" "middle"
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
    [(symbol=? s 'c) (above (beside (overlay HIGHLIGHT CHUMMY) (rectangle 10 0 'solid' pink) BULLY (rectangle 10 0 'solid 'pink) PALSY)
                            (rectangle 0 10 'solid 'pink)
                            (beside PEPPY (rectangle 10 0 'solid 'pink) CHIPPER (rectangle 10 0 'solid' pink) RANCOROUS))]
    [(symbol=? s 'b) (above (beside CHUMMY (rectangle 10 0 'solid' pink) (overlay HIGHLIGHT BULLY) (rectangle 10 0 'solid 'pink) PALSY)
                            (rectangle 0 10 'solid 'pink)
                            (beside PEPPY (rectangle 10 0 'solid 'pink) CHIPPER (rectangle 10 0 'solid' pink) RANCOROUS))]
    [(symbol=? s 'p) (above (beside CHUMMY (rectangle 10 0 'solid' pink) BULLY (rectangle 10 0 'solid 'pink) (overlay HIGHLIGHT PALSY))
                            (rectangle 0 10 'solid 'pink)
                            (beside PEPPY (rectangle 10 0 'solid 'pink) CHIPPER (rectangle 10 0 'solid' pink) RANCOROUS))]
    [(symbol=? s 'pe) (above (beside CHUMMY (rectangle 10 0 'solid' pink) BULLY (rectangle 10 0 'solid 'pink) PALSY)
                             (rectangle 0 10 'solid 'pink)
                             (beside (overlay HIGHLIGHT PEPPY) (rectangle 10 0 'solid 'pink) CHIPPER (rectangle 10 0 'solid' pink) RANCOROUS))]
    [(symbol=? s 'ch) (above (beside CHUMMY (rectangle 10 0 'solid' pink) BULLY (rectangle 10 0 'solid 'pink) PALSY)
                             (rectangle 0 10 'solid 'pink)
                             (beside PEPPY (rectangle 10 0 'solid 'pink) (overlay HIGHLIGHT CHIPPER) (rectangle 10 0 'solid' pink) RANCOROUS))]
    [(symbol=? s 'r) (above (beside CHUMMY (rectangle 10 0 'solid' pink) BULLY (rectangle 10 0 'solid 'pink) PALSY)
                            (rectangle 0 10 'solid 'pink)
                            (beside PEPPY (rectangle 10 0 'solid 'pink) CHIPPER (rectangle 10 0 'solid' pink) (overlay HIGHLIGHT RANCOROUS)))]
    [else  (above (beside CHUMMY (rectangle 10 0 'solid' pink) BULLY (rectangle 10 0 'solid 'pink) PALSY)
                  (rectangle 0 10 'solid 'pink)
                  (beside PEPPY (rectangle 10 0 'solid 'pink) CHIPPER (rectangle 10 0 'solid' pink) RANCOROUS))]))

;; -----------------------------------------------------------------------------------------
;; Handle-Key
;; -----------------------------------------------------------------------------------------

;; handle-key : world keyevent --> world
;; handles key input and updates the world if necessary
(define (handle-key w k)
  (cond
    [(or (key=? k "shift") (key=? k "start") (key=? k "cancel") (key=? k "clear")
         (key=? k "menu") (key=? k "pause") (key=? k "capital") (key=? k "prior")
         (key=? k "next") (key=? k "end") (key=? k "home") (key=? k "select")
         (key=? k "print") (key=? k "execute") (key=? k "snapshot") (key=? k "insert")
         (key=? k "help") (key=? k "f1") (key=? k "f2") (key=? k "f3")
         (key=? k "f4") (key=? k "f5") (key=? k "f6") (key=? k "f7") (key=? k "f8")
         (key=? k "f9") (key=? k "f10") (key=? k "f11") (key=? k "f12")
         (key=? k "numlock") (key=? k "scroll") (key=? k "wheel-left") (key=? k "wheel-right")
         (key=? k "control") (key=? k "rcontrol") (key=? k "rshift") (key=? k "\t")) w]
    [(user? w)
     (cond
       [(key=? k "escape") w]
       [(or (key=? k "left") (key=? k "right") (key=? k "up") (key=? k "down") 
            (key=? k "wheel-up") (key=? k "wheel-down"))
        (make-user (user-username w) (user-status w) (user-text-color w) (user-friends w)
                   (update-viewing (user-viewing w) (length (user-friends w)))
                   (user-pesters w) (user-requests w) (user-tab w) (user-text w))]
       [(key=? k "\b") (make-user (user-username w) (user-status w) (user-text-color w) (user-friends w)
                                  (user-viewing w) (user-pesters w) (user-requests w) (user-tab w)
                                  (if (eq? (user-text w) "")
                                      "" (substring (user-text w) 0 (- (length (user-text w)) 1))))]
       [(key=? k "\r")
        (cond
          [(eq? (user-tab w) "none") w]
          [(eq? (user-tab w) "requests")
           (make-package
            (make-user (user-username w) (user-status w) (user-text-color w) (user-friends w)
                       (user-viewing w) (user-pesters w) (user-requests w) (user-tab w) "")
            '((user-username w) (user-text w) "request" empty))]
          [else (make-package (make-user (user-username w) (user-status w) (user-text-color w) (user-friends w)
                                         (user-viewing w) (user-pesters w) (user-requests w) (user-tab w) "")
                              '((user-username w) (user-tab w) "pester" '((user-text w) (user-text-color w))))])]
       [else (if (eq? (user-tab w) "none")
                 w (make-user (user-username w) (user-status w) (user-text-color w) (user-friends w)
                              (user-viewing w) (user-pesters w) (user-requests w) (user-tab w) (string-append (user-text w) k)))])]
    [(user-login? w)
     (cond
       [(or (key=? k "up") (key=? k "down") (key=? k "left") (key=? k "right")
            (key=? k "wheel-up") (key=? k "wheel-down"))
        (make-user-login (user-login-username w)
                         (user-login-password w)
                         (if (eq? (user-login-editing w) 1) 2 1))]
       [(key=? k "escape") (make-user-create "" "black" "" "" 1)]
       [(key=? k "\r") (make-package (make-user-login "" "" 1)
                                     '((user-login-username w) "server" "login" (user-login-password w)))]
       [(key=? k "\b") (if (eq? (user-login-editing w) 1)
                           (make-user-login (if (eq? (user-login-username w) "")
                                                "" (substring (user-login-username w) 0 (- (string-length (user-login-username w)) 1)))
                                            (user-login-password w) 1)
                           (make-user-login (user-login-username w)
                                            (if (eq? (user-login-username w) "")
                                                "" (substring (user-login-password w) 0 (- (string-length (user-login-password w)) 1)))
                                            2))]
       [else (if (eq? (user-login-editing w) 1)
                 (make-user-login (string-append (user-login-username w) k) (user-login-password w) 1)
                 (make-user-login (user-login-username w) (string-append (user-login-password w) k) 2))])]
    [(user-create? w)
     (cond
       [(or (key=? k "up") (key=? k "down") (key=? k "left") (key=? k "right")
            (key=? k "wheel-up") (key=? k "wheel-down"))
        (make-user-create (user-create-username w) (user-create-text-color w)
                          (user-create-password1 w) (user-create-password2 w)
                          (if (eq? (user-create-editing w) 4) 1 (+ (user-create-editing w) 1)))]
       [(key=? k "escape") (make-user-login "" "" 1)]
       [(key=? k "\b")
        (cond
          [(eq? (user-create-editing w) 1)
           (make-user-create (if (eq? (user-create-username w) "")
                                 "" (substring (user-create-username w) 0 (- (string-length (user-create-username w)) 1)))
                             (user-create-text-color w) (user-create-password1 w) (user-create-password2 w) 1)]
          [(eq? (user-create-editing w) 2)
           (make-user-create (user-create-username w)
                             (if (eq? (user-create-text-color w) "")
                                 "" (substring (user-create-text-color w) 0 (- (string-length (user-create-text-color w)) 1)))
                             (user-create-password1 w) (user-create-password2 w) 2)]
          [(eq? (user-create-editing w) 3)
           (make-user-create (user-create-username w) (user-create-text-color w)
                             (if (eq? (user-create-password1 w) "")
                                 "" (substring (user-create-password1 w) 0 (- (string-length (user-create-password1 w)) 1)))
                             (user-create-password2 w) 3)]
          [(eq? (user-create-editing w) 4)
           (make-user-create (user-create-username w) (user-create-text-color w) (user-create-password1 w)
                             (if (eq? (user-create-password2 w) "")
                                 "" (substring (user-create-password2 w) 0 (- (string-length (user-create-password2 w)) 1))) 4)]
          [else (error 'unexpected_editing_field)])]
       [(key=? k "\r") (if (eq? (user-create-password1 w) (user-create-password2 w))
                           (make-package (make-user-login "" "" 1)
                                         '((user-create-username w) "server" "new-user" '((user-create-text-color w) (user-create-password1 w))))
                           (make-user-create (user-create-username w) (user-create-text-color w) "" "" 3))]
       [else w])]
    [else (cond
            [(or (< (image-width (pestertext (user-create-username w) 14 'white)) 246)
                 (< (image-width (pestertext (user-create-text-color w) 14 'white)) 246)
                 (< (image-width (pestertext (user-create-password1 w) 14 'white)) 246)
                 (< (image-width (pestertext (user-create-password2 w) 14 'white)) 246)) w]
            [(eq? (user-create-editing w) 1)
             (make-user-create (string-append (user-create-username w) k) (user-create-text-color w)
                               (user-create-password1 w) (user-create-password2 w))]
            [(eq? (user-create-editing w) 2)
             (make-user-create (user-create-username w) (string-append (user-create-text-color w) k)
                               (user-create-password1 w) (user-create-password2 w))]
            [(eq? (user-create-editing w) 3)
             (make-user-create (user-create-username w) (user-create-text-color w)
                               (string-append (user-create-password1 w) k) (user-create-password2 w))]
            [(eq? (user-create-editing w) 4)
             (make-user-create (user-create-username w) (user-create-text-color w)
                               (user-create-password1 w) (string-append (user-create-password2 w) k))]
            [else (error 'unexpected_editing_value)])]))

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
    [(eq? (third m) "pester") (make-user (user-username w)
                                         (user-status w)
                                         (user-text-color w)
                                         (if (not (eq? (user-tab w) (first m)))
                                             (sort-new-messages (user-friends w) '(m))
                                             (user-friends w))
                                         (user-viewing w)
                                         (if (>= (length (filter (lambda (x) (eq? (first x) (first m))) (user-pesters w))) MAXPESTERS)
                                             (cons m
                                                   (append
                                                    (reverse (rest (reverse (filter (lambda (x) (eq? (first x) (first m))) (user-pesters w)))))
                                                    (filter (lambda (x) (not (eq? (first x) (first m)))) (user-pesters w))))
                                             (cons m (user-pesters w)))
                                         (user-requests w)
                                         (user-tab w)
                                         (user-text w))]
    [(eq? (third m) "login-data") (make-user (second m) 'c (first (fourth m))
                                             (sort-new-messages (second (fourth m))
                                                                (third (fourth m)))
                                             1 (third (fourth m))
                                             (fourth (fourth m)) "none" "")]
    [(eq? (third m) "request") (make-user (user-username w)
                                          (user-status w)
                                          (user-text color w)
                                          (user-friends w)
                                          (user-viewing w)
                                          (user-pesters w)
                                          (cons m (user-pesters w))
                                          (user-tab w)
                                          (user-text w))]
    [(eq? (third m) "friends") (make-user (user-username w)
                                          (user-status w)
                                          (user-text color w)
                                          (update-friends (user-friends w) m)
                                          (user-viewing w)
                                          (user-pesters w)
                                          (user-pesters w)
                                          (user-tab w)
                                          (user-text w))]
    [else (error 'unexpected_message)]))


;; sort-new-messages : LOSS&B LOP --> LOSS&B
;; sets corisponding values of bools in LOSS&B to true
;; iff list of pesters has messages from those users
(define (sort-new-messages f p)
  (cond
    [(empty? f) empty]
    [(cons? f)
     (if (> (length (filter (lambda (x) (eq? (first x) (first (first f)))) p)) 0)
         (cons (list (first (first f)) (second (first f)) true)
               (sort-new-messages (rest f) p))
         (cons (first f) (sort-new-messages (rest f) p)))]))

;; update-friends : LOSS&B LOSS&B --> LOSS&B
;; updates friend list with friend data from server
(define (update-friends f s)
  (cond
    [(empty? f) empty]
    [(cons? f) (cons
                (if (third (first f))
                    (list (first (first s)) (second (first s)) true)
                    (first s))
                (update-friends (rest f) (rest s)))]))

;; -----------------------------------------------------------------------------------------
;; Main
;; -----------------------------------------------------------------------------------------

#|
;; main
(define (main w)
  (big-bang w
            [name "Pesterchum v.0.1"]
            [register (read-file "server_ip.txt")]
            [port 9000]
            [on-receive receive-message]
            [to-draw render]
            [on-key handle-key]
            [on-mouse handle-mouse] |#

