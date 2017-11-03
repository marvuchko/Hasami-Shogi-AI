(require "./interface.lisp")

(defun initialize-game (n)
    (if (< n 4) '() 
        (format t "~%~aKo igra prvi, 0-igrac igra pvi, 1-igrac igra drugi: " #\tab))
    (cond
        ((< n 4) '())
        (t (game-loop '() (create-table n) 'X (read) n))))

(defun game-loop (someoneWins table move player dim)
    (ext:run-shell-command "cls")
    (show-table table (create-numbers dim))
    (show-number-of-x-figures table)
    (show-number-of-o-figures table)
    (show-current-player player)
    (format t "~%~%~aUnesi potez: " #\tab) 
    (cond
        ((not (null someoneWins)) '())
        (t (game-loop someoneWins  
            (set-next-state table move (char-to-index (read-char)) (- (read) 1) (char-to-index (read-char)) (- (read) 1)) 
            (next-move move) 
            (plays-next player) dim))))