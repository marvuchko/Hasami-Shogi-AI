(require "./userInterface.lisp")

(defun main ()
    (ext:run-shell-command "cls")
    (format t "~%~AUnesi dimenzije table: " #\tab)
    (let ((dim 0)))
    (setf dim (read))
    (gameLoop '() (createTable dim) dim))

;;; Execution

(main)