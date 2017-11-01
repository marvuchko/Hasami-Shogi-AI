(require "./interface.lisp")

(defun main ()
    (ext:run-shell-command "cls")
    (let ((player -1) (dim -1)))
    (format t "~%~AUnesi dimenzije table: " #\tab)
    (setq dim (read))
    (format t "~%~AKo igra prvi, 0-igrac igra prvi, 1-igrac igra drugi: " #\tab)
    (setq player (read))
    (initializeGame player dim)
)    

;;; Execution

(main)