(require "./game.lisp")

(defun initializeGame (player n)
    (cond
        ((< n 4) nil)
        (t (gameLoop '() (createTable n) player n))))

(defun gameLoop (someoneWins table player dim)
    (ext:run-shell-command "cls")
    (showTable table (createNumbers dim))
    (format t "~%~%~ANa potezu je: igrac ~A" #\tab (+ player 1))
    (format t "~%~%~AUnesi potez: " #\tab) 
    (cond
        ((not (null someoneWins)) '())
        (t (gameLoop someoneWins (nextState table (read) (- (char-code (read-char)) 65) (- (read) 1) (- (char-code (read-char)) 65) (- (read) 1)) player dim))))