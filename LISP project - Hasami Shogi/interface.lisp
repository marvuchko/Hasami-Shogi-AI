(require "./game.lisp")

(defun initializeGame (n)
    (cond
        ((< n 4) nil)
        (t (gameLoop '() (createTable n) n))))

(defun gameLoop (someoneWins table dim)
    (ext:run-shell-command "cls")
    (showTable table (createNumbers dim))
    (format t "~%~%~AUnesi potez: " #\tab)
    (cond
        ((not (null someoneWins)) '())
        (t (gameLoop someoneWins (nextState table (read) (- (char-code (read-char)) 65) (- (read) 1) (- (char-code (read-char)) 65) (- (read) 1)) dim))))