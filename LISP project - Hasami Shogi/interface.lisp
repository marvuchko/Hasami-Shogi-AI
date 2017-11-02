(require "./game.lisp")

(defun initializeGame (n)
    (if (< n 4) '() 
        (format t "~%~AKo igra prvi, 0-igrac igra pvi, 1-igrac igra drugi: " #\tab))
    (cond
        ((< n 4) '())
        (t (gameLoop '() (createTable n) 'X (read) n))))

(defun showCurrentPlayer (player)
    (format t "~%~ANa potezu je: igrac ~A" #\tab (+ player 1)))

(defun showTable (table nums)
    (format t "~%~A    " #\tab)
    (printNumbers nums)
    (format t "~%~%")
    (showTableRec table nums))

(defun gameLoop (someoneWins table move player dim)
    (ext:run-shell-command "cls")
    (showTable table (createNumbers dim))
    (showCurrentPlayer player)
    (format t "~%~%~AUnesi potez: " #\tab) 
    (cond
        ((not (null someoneWins)) '())
        (t (gameLoop someoneWins (nextState table move (- (char-code (read-char)) 65) (- (read) 1) (- (char-code (read-char)) 65) (- (read) 1)) (nextMove move) (playsNext player) dim))))