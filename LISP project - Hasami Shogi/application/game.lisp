(require "./libraries/libs.lisp")
(require "./game-logic/logics.lisp")

;;;;;;;;;;; Table functions

;creates initialized table for the game
(defun create-table (n)
    (create-table-rec n n))

;tests if input is out of bounds 
(defun valid-bounds (table i j)
    (or 
        (or (< i (length table)) (>= i 0)) 
        (or (< j (length table)) (>= j 0))))

;gets an element from the i-th row and j-th column of the table 
(defun get-table-element (table i j)
    (get-list-element (get-list-element table i) j))

;sets value of an element from the i-th row and j-th column of the table
(defun set-table-element (table el i j)
    (set-list-element table (set-list-element (get-list-element table i) el j) i))

;;;;;;;;;;; Gameplay functions

;changes game state if the move is valid
(defun set-next-state (table figure fromI fromJ toI toJ)
    (if (is-valid-move table figure fromI fromJ toI toJ)
        (set-table-element (set-table-element table figure toI toJ) '- fromI fromJ) table))

;tests if the move is valid
(defun is-valid-move (table figure fromI fromJ toI toJ)
    (and 
        (or (equal fromI toI) (equal fromJ toJ))
        (equal '- (car (get-table-element table toI toJ)))
        (equal figure (car (get-table-element table fromI fromJ)))
        (valid-bounds table fromI fromJ)
        (valid-bounds table toI toJ)))

;user plays the move, user enters the coordinates using standard input
(defun play-move-user (table figure)
    (set-next-state table figure (char-to-index (read-char)) (1- (read)) (char-to-index (read-char)) (1- (read))))

;decides who plays next
(defun plays-next (player)
    (logxor player 1))

;decides which figures can be played next
(defun next-figure (figure)
    (if (equal figure 'X) 'O 'X))