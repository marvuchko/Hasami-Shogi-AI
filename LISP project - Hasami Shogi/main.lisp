(require "./app.lisp")

(defun main ()
    (ext:run-shell-command "cls")
    (format t "~%~aUnesi dimenzije table: " #\tab)
    (initialize-game (read)))    

;;; Execution

(main)