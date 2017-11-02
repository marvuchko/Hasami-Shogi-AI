(require "./interface.lisp")

(defun main ()
    (ext:run-shell-command "cls")
    (format t "~%~AUnesi dimenzije table: " #\tab)
    (initializeGame (read)))    

;;; Execution

(main)