;;;;;;;;;;; Recursive functions for some operations with table 

;creates row with all X values recursively
(defun create-x-row-rec (n)
    (if (zerop n) '()
        (append '(X) (create-x-row-rec (1- n)))))

;creates row with all O values recursively
(defun create-o-row-rec (n)
    (if (zerop n) '()
        (append '(O) (create-o-row-rec (1- n)))))

;creates row with all - values recursively
(defun create-free-row-rec (n)
    (if (zerop n) '()
        (append '(-) (create-free-row-rec (1- n)))))

;creates the table recursively recursively
(defun create-table-rec (n i)
    (if (zerop i) '()
    (append
        (cond
            ((and (> i 0) (<= i 2)) (list (create-o-row-rec n)))
            ((and (> i 2) (<= i (- n 2))) (list (create-free-row-rec n)))
            ((and (> i (- n 2)) (<= i n) (list (create-x-row-rec n))))) 
    (create-table-rec n (1- i)))))

;displays the table recursively
(defun show-table-rec (table numbers)
    (if (null table) '()
        (progn 
            (format t "~a~a  ~a~%" #\tab (code-char (+ 64 (car numbers))) (car table))
            (show-table-rec (cdr table) (cdr numbers)))))

;sets an element of the list recursively
(defun set-list-element (lst el n)
    (if (null lst) (if (listp el) el (list el)))
    (if (= n 0) 
        (append (list el) (cdr lst))
        (append (list (car lst)) (set-list-element (cdr lst) el (1- n)))))

;gets an element from the list recursively
(defun get-list-element (lst n)
    (if (> n (length lst)) '())
    (if (zerop n) 
        (if (listp (car lst)) 
            (car lst) 
            (list (car lst)))
        (append '() (get-list-element(cdr lst) (1- n)))))

;calculates the number of X atoms in the list recursively
(defun number-of-x-in-list (lst)
    (if (null lst) 0
        (+ 0 (if (equal (car lst) 'X) 1 0) (number-of-x-in-list (cdr lst)))))

;calculates the number of X atoms in the table recursively
(defun number-of-x (table)
    (if (null table) 0
        (+ (number-of-x-in-list (car table)) (number-of-x (cdr table)))))

;calculates the number of O atoms in the list recursively
(defun number-of-o-in-list (lst)
    (if (null lst) 0
        (+ 0 (if (equal (car lst) 'O) 1 0) (number-of-o-in-list (cdr lst)))))

;calculates the number of O atoms in the table recursively
(defun number-of-o (table)
    (if (null table) 0
        (+ (number-of-o-in-list (car table)) (number-of-o (cdr table)))))

;calculates avaiable moves to the left of the selected figure recursively
(defun avaiable-moves-left-rec (table fromI fromJ)
    (if (> fromJ 0)
        (if (equal '(-) (get-table-element table fromI (- fromJ 1)))
            (append (list (list fromI (- fromJ 1))) (avaiable-moves-left-rec table fromI (- fromJ 1))))))

;calculates avaiable moves to the right of the selected figure recursively
(defun avaiable-moves-right-rec (table fromI fromJ)
    (if (> (length table) fromJ)
        (if (equal '(-) (get-table-element table fromI (+ fromJ 1)))
            (append (list (list fromI (+ fromJ 1))) (avaiable-moves-right-rec table fromI (+ fromJ 1))))))

;calculates avaiable moves above the selected figure recursively
(defun avaiable-moves-top-rec (table fromI fromJ)
    (if (> fromI 0)
        (if (equal '(-) (get-table-element table (- fromI 1) fromJ))
            (append (list (list (- fromI 1) fromJ)) (avaiable-moves-top-rec table (- fromI 1) fromJ)))))

;calculates avaiable moves below the selected figure recursively
(defun avaiable-moves-bottom-rec (table fromI fromJ)
    (if (> (length table) fromI)
        (if (equal '(-) (get-table-element table (+ fromI 1) fromJ))
            (append (list (list (+ fromI 1) fromJ)) (avaiable-moves-bottom-rec table (+ fromI 1) fromJ)))))

;finds sublist in a list
(defun sublist-exsits (sub lst)
    (if (not (null lst))
        (or (equal sub (car lst)) (sublist-exsits sub (cdr lst)))))

;;;;;;;;;;; Recursive functions for some operations with numbers 

;creates a list of numbers from 1 to n recursively
(defun create-numbers (n)
    (if (= n 0) '()
    (append (create-numbers (1- n)) (list n))))

;displays a list of numbers from 1 to n recursively
(defun print-numbers (numbers)
    (if (null numbers) '()
        (progn 
            (format t "~a " (car numbers))
            (print-numbers (cdr numbers)))))

;;;;;;;;;;; Functions for some operations with chars 

;calculates index from ASCII chars input
(defun char-to-index (ch)
    (cond
        ((< (char-code ch) (char-code #\a)) (- (char-code ch) (char-code #\A)))
        (t (- (char-code ch) (char-code #\a)))))