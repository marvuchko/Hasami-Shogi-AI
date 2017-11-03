(require "./libs.lisp")
(require "./logics.lisp")

;;; Table functions

(defun create-table (n)
    (create-table-rec n n))

(defun valid-bounds (table i j)
    (or (or (< i (length table)) (>= i 0)) (or (< j (length table)) (>= j 0))))

(defun get-table-element (table i j)
        (get-list-element (get-list-element table i) j))

(defun set-table-element (table el i j)
        (set-list-element table (set-list-element (get-list-element table i) el j) i))

;;; Playing

(defun set-next-state (table move fromI fromJ toI toJ)
    (if (is-valid-move table move fromI fromJ toI toJ)
        (set-table-element (set-table-element table move toI toJ) '- fromI fromJ) table))

(defun is-valid-move (table move fromI fromJ toI toJ)
    (and 
        (or (equal fromI toI) (equal fromJ toJ))
        (equal '- (car (get-table-element table toI toJ)))
        (equal move (car (get-table-element table fromI fromJ)))
        (valid-bounds table fromI fromJ)
        (valid-bounds table toI toJ)))

(defun plays-next (player)
    (logxor player 1))

(defun next-move (move)
    (if (equal move 'X) 'O 'X)) 

