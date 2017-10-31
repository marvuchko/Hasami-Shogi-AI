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
    (getListElement (getListElement table i) j))

(defun setTableElement (table el i j)
    (setListElement table (setListElement (getListElement table i) el j) i))

; Playing

(defun playMove (table move fromI fromJ toI toJ)
    (setTableElement (setTableElement table move toI toJ) '- fromI fromJ))