(require "./libs.lisp")
(require "./logics.lisp")

;;; Table functions

(defun createTable (n)
    (createTableRec n n))

(defun showTable (table nums)
    (format t "~%~A    " #\tab)
    (printNumbers nums)
    (format t "~%~%")
    (showTableRec table nums))

(defun getTableElement (table i j)
    (cond 
        ((or (> i (length table)) (< i 0)) table)
        ((or (> j (length table)) (< j 0)) table)
        (t (getListElement (getListElement table i) j)))
)
    

(defun setTableElement (table el i j)
    (cond
        ((or (> i (length table)) (< i 0)) table)
        ((or (> j (length table)) (< j 0)) table)
        (t (setListElement table (setListElement (getListElement table i) el j) i)))
)

; Playing

(defun nextState (table move fromI fromJ toI toJ)
    (if (isValidMove table fromJ fromJ toI toJ)
    (setTableElement (setTableElement table move toI toJ) '- fromI fromJ) table))

(defun isValidMove (table fromI fromJ toI toJ)
    (if (and (or (= fromI toI) (= fromJ toJ)) (equal '(-) (getTableElement table toI toJ))) t))

