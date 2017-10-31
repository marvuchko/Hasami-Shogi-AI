
;;; Table functions

(defun createXRowRec (n)
    (if (zerop n) '()
        (append '(X) (createXRowRec (1- n)))))

(defun createORowRec (n)
    (if (zerop n) '()
        (append '(O) (createORowRec (1- n)))))

(defun createFreeRowRec (n)
    (if (zerop n) '()
        (append '(-) (createFreeRowRec (1- n)))))

(defun createTableRec (n i)
    (if (zerop i) '()
    (append
        (cond
            ((and (> i 0) (<= i 2)) (list (createORowRec n)))
            ((and (> i 2) (<= i (- n 2))) (list (createFreeRowRec n)))
            ((and (> i (- n 2)) (<= i n) (list (createXRowRec n))))) (createTableRec n (1- i)))))

(defun showTableRec (table numbers)
    (if (null table) '()
        (format t "~A~A  ~A~%" #\tab (code-char (+ 64 (car numbers))) (car table)))
    (if (not (null table)) 
        (showTableRec (cdr table) (cdr numbers))))

(defun setListElement(lst el n)
    (if (null lst) (if (listp el) el (list el)))
    (if (= n 0) 
        (append (list el) (cdr lst))
        (append (list (car lst)) (setListElement (cdr lst) el (1- n)))))

(defun getListElement (lst n)
    (if (> n (length lst)) '())
    (if (zerop n) 
        (if (listp (car lst)) 
            (car lst) 
            (list (car lst)))
        (append '() (getListElement(cdr lst) (1- n)))))

;;; Number list functions

(defun createNumbers (n)
    (if (= n 0) '()
    (append (createNumbers (1- n)) (list n))))

(defun printNumbers (numbers)
    (if (null numbers) '()
        (format t "~A " (car numbers)))
    (if (not (null numbers)) (printNumbers (cdr numbers))))