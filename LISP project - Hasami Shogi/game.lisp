(require "./libs.lisp")
(require "./logics.lisp")

;;; Table functions

(defun create-table (n)
    (create-table-rec n n))

(defun get-table-element (table i j)
    (cond 
        ((or (> i (length table)) (< i 0)) table)
        ((or (> j (length table)) (< j 0)) table)
        (t (get-list-element (get-list-element table i) j))))

(defun set-table-element (table el i j)
    (cond
        ((or (> i (length table)) (< i 0)) table)
        ((or (> j (length table)) (< j 0)) table)
        (t (set-list-element table (set-list-element (get-list-element table i) el j) i))))

;;; Playing

(defun set-next-state (table move fromI fromJ toI toJ)
    (if (is-valid-move table move fromJ fromJ toI toJ)
    (set-table-element (set-table-element table move toI toJ) '- fromI fromJ) table))

(defun is-valid-move (table move fromI fromJ toI toJ)
    (if (and 
            (or (= fromI toI) (= fromJ toJ))
            (equal '(-) (get-table-element table toI toJ))) t))

(defun plays-next (player)
    (logxor player 1))

(defun next-move (move)
    (if (equal move 'X) 'O 'X)) 

