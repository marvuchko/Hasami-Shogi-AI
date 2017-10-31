(require "./game.lisp")

(defun gameLoop (someoneWins table dim)
    (ext:run-shell-command "cls")
    (showTable table (createNumbers dim))
    (format t "~%~%~AUnesi potez: " #\tab)
    (cond
        ((not (null someoneWins)) '())
        (t (gameLoop someoneWins (playMove table (read) (read) (read) (read) (read)) dim))))