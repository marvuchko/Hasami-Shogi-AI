(require "./libs.lisp")
(require "./logics.lisp")

;;; Table functions

(defun createTable (n)
    (createTableRec n n))

(defun getTableElement (table i j)
    (cond 
        ((or (> i (length table)) (< i 0)) table)
        ((or (> j (length table)) (< j 0)) table)
        (t (getListElement (getListElement table i) j))))

(defun setTableElement (table el i j)
    (cond
        ((or (> i (length table)) (< i 0)) table)
        ((or (> j (length table)) (< j 0)) table)
        (t (setListElement table (setListElement (getListElement table i) el j) i))))

; Playing

(defun nextState (table move fromI fromJ toI toJ)
    (if (isValidMove table fromJ fromJ toI toJ)
    (setTableElement (setTableElement table move toI toJ) '- fromI fromJ) table))

(defun isValidMove (table fromI fromJ toI toJ)
    (if (and (or (= fromI toI) (= fromJ toJ)) (equal '(-) (getTableElement table toI toJ))) t))

(defun playsNext (player)
    (logxor player 1))

(defun nextMove (move)
    (if (equal move 'X) 'O 'X)) 

