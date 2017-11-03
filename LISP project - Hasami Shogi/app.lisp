(require "./interface.lisp")

;;;;;;;;;;; Application 

;this code runs when program starts
(defun initialize-game (n)
    (if (< n 5) '() ;exits the game if the dimensions are below 5
        (progn
            (format t "~%~aKo igra prvi, 0-igrac igra pvi, 1-igrac igra drugi: " #\tab)
            (game-loop '() (create-table n) 'X (read) n))))

;this code runs until the game is over
(defun game-loop (gameover table figure player dim)
    (ext:run-shell-command "cls")
    (show-table table (create-numbers dim))
    (show-number-of-x-figures table)
    (show-number-of-o-figures table)
    (show-current-player player)
    (format t "~%~%~aUnesi potez: " #\tab) 
    (cond
        (gameover '())
        (t (game-loop 
            gameover  
            (set-next-state table figure (char-to-index (read-char)) (- (read) 1) (char-to-index (read-char)) (- (read) 1)) 
            (next-figure figure) 
            (plays-next player) dim))))