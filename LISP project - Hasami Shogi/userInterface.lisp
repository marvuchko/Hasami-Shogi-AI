(require "./game.lisp")

(defun game-loop (someoneWins table dim)
    (ext:run-shell-command "cls")
    (show-table table (create-numbers dim))
    (format t "~%~%~aUnesi potez: " #\tab)
    (cond
        ((not (null someoneWins)) '())
        (t (game-loop someoneWins (playMove table (read) (read) (read) (read) (read)) dim))))