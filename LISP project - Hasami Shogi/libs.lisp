;;; Table functions

(defun create-x-row-rec (n)
    (if (zerop n) '()
        (append '(X) (create-x-row-rec (1- n)))))

(defun create-o-row-rec (n)
    (if (zerop n) '()
        (append '(O) (create-o-row-rec (1- n)))))

(defun create-free-row-rec (n)
    (if (zerop n) '()
        (append '(-) (create-free-row-rec (1- n)))))

(defun create-table-rec (n i)
    (if (zerop i) '()
    (append
        (cond
            ((and (> i 0) (<= i 2)) (list (create-o-row-rec n)))
            ((and (> i 2) (<= i (- n 2))) (list (create-free-row-rec n)))
            ((and (> i (- n 2)) (<= i n) (list (create-x-row-rec n))))) (create-table-rec n (1- i)))))

(defun show-table-rec (table numbers)
    (if (null table) '()
        (format t "~a~a  ~a~%" #\tab (code-char (+ 64 (car numbers))) (car table)))
    (if (not (null table)) 
        (show-table-rec (cdr table) (cdr numbers))))

(defun set-list-element (lst el n)
    (if (null lst) (if (listp el) el (list el)))
    (if (= n 0) 
        (append (list el) (cdr lst))
        (append (list (car lst)) (set-list-element (cdr lst) el (1- n)))))

(defun get-list-element (lst n)
    (if (> n (length lst)) '())
    (if (zerop n) 
        (if (listp (car lst)) 
            (car lst) 
            (list (car lst)))
        (append '() (get-list-element(cdr lst) (1- n)))))

(defun number-of-x-in-list (lst)
    (if (null lst) 0
        (+ 0 (if (equal (car lst) 'X) 1 0) (number-of-x-in-list (cdr lst)))))

(defun number-of-x (table)
    (if (null table) 0
        (+ (number-of-x-in-list (car table)) (number-of-x (cdr table)))))

(defun number-of-o-in-list (lst)
    (if (null lst) 0
        (+ 0 (if (equal (car lst) 'O) 1 0) (number-of-o-in-list (cdr lst)))))

(defun number-of-o (table)
    (if (null table) 0
        (+ (number-of-o-in-list (car table)) (number-of-o (cdr table)))))

;;; Number list functions

(defun create-numbers (n)
    (if (= n 0) '()
    (append (create-numbers (1- n)) (list n))))

(defun print-numbers (numbers)
    (if (null numbers) '()
        (format t "~a " (car numbers)))
    (if (not (null numbers)) (print-numbers (cdr numbers))))

;;; Char functions

(defun char-to-index (ch)
    (cond
        ((< (char-code ch) (char-code #\a)) (- (char-code ch) (char-code #\A)))
        (t (- (char-code ch) (char-code #\a)))))