(require "./libraries/libs.lisp")
(require "./game-logic/logics.lisp")

;;;;;;;;;;; Table functions

;creates initialized table for the game
(defun create-table (n)
    (create-table-rec n n))

;tests if input is out of bounds 
(defun valid-bounds (table i j)
    (and (< i (length table)) (>= i 0)) (< j (length table)) (>= j 0))

;gets an element from the i-th row and j-th column of the table 
(defun get-table-element (table i j)
    (if (valid-bounds table i j)
        (get-list-element (get-list-element table i) j)))

;sets value of an element from the i-th row and j-th column of the table
(defun set-table-element (table el i j)
    (if (valid-bounds table i j)
        (set-list-element table (set-list-element (get-list-element table i) el j) i) table))

;;;;;;;;;;; Gameplay functions

;changes game state if the move is valid
(defun set-next-state (table figure fromI fromJ toI toJ)
    (if (is-valid-move table figure fromI fromJ toI toJ)
        (set-table-element (set-table-element table figure toI toJ) '- fromI fromJ) table))

;creates the list of all avaiable moves
(defun list-of-avaiable-moves-for-figure (table figure fromI fromJ)
    (append 
        (avaiable-moves-left table figure fromI fromJ)
        (avaiable-moves-right table figure fromI fromJ)
        (avaiable-moves-top table figure fromI fromJ)
        (avaiable-moves-bottom table figure fromI fromJ)))

;creates the list of all avaiable moves to the left of the figure
(defun avaiable-moves-left (table figure fromI fromJ)
    (append 
        (avaiable-moves-left-rec table fromI fromJ)
        (if (and 
            (equal (car (get-table-element table fromI (- fromJ 1))) (next-figure figure))
            (equal (car (get-table-element table fromI (- fromJ 2))) '-))
            (list (list fromI (- fromJ 2))))))

;creates the list of all avaiable moves to the right of the figure
(defun avaiable-moves-right (table figure fromI fromJ)
    (append 
        (avaiable-moves-right-rec table fromI fromJ)
        (if (and 
            (equal (car (get-table-element table fromI (+ fromJ 1))) (next-figure figure))
            (equal (car (get-table-element table fromI (+ fromJ 2))) '-))
            (list (list fromI (+ fromJ 2))))))

;creates the list of all avaiable moves above the figure
(defun avaiable-moves-top (table figure fromI fromJ)
    (append 
        (avaiable-moves-top-rec table fromI fromJ)
        (if (and 
            (equal (car (get-table-element table (- fromI 1) fromJ)) (next-figure figure))
            (equal (car (get-table-element table (- fromI 2) fromJ)) '-))
            (list (list (- fromI 2) fromJ)))))

;creates the list of all avaiable moves below the figure
(defun avaiable-moves-bottom (table figure fromI fromJ)
    (append 
        (avaiable-moves-bottom-rec table fromI fromJ)
        (if (and 
            (equal (car (get-table-element table (+ fromI 1) fromJ)) (next-figure figure))
            (equal (car (get-table-element table (+ fromI 2) fromJ)) '-))
            (list (list (+ fromI 2) fromJ)))))

;tests if the move is valid
(defun is-valid-move (table figure fromI fromJ toI toJ)
    (and 
        (valid-bounds table fromI fromJ) (valid-bounds table toI toJ)
        (sublist-exists (list toI toJ) (list-of-avaiable-moves-for-figure table figure fromI fromJ))
        (equal figure (car (get-table-element table fromI fromJ)))))

;user plays the move, user enters the coordinates using standard input
(defun play-move (table figure)
    (set-next-state table figure (char-to-index (read-char)) (1- (read)) (char-to-index (read-char)) (1- (read))))

;decides who plays next
(defun plays-next (player)
    (logxor player 1))

;decides which figures can be played next
(defun next-figure (figure)
    (if (equal figure 'X) 'O 'X))