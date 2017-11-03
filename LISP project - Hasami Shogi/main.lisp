(require "./app.lisp")

;;;;;;;;;;; Main function

;this code runs when program starts
(defun main ()
    (ext:run-shell-command "cls")
    (format t "~%~aUnesi dimenzije table: " #\tab)
    (initialize-game (read)))    

;;;;;;;;;;; Execution

(main)