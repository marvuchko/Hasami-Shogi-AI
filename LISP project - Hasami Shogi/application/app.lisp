(require "./interface.lisp")

;;;;;;;;;;; Application 

;this code runs when program starts
(defun initialize-game (n)
    (if (< n 5) '() ;exits the game if the dimensions are below 5
        (progn
            (show-initial-message)
            (game-loop '() (create-table n) 'X (read) (create-numbers n))))) ;initial game state

;this code runs until the game is over
(defun game-loop (gameover table figure player nums)
    (show-ui table nums player)
    (cond
        (gameover '())
        (t (game-loop gameover (play-move-user table figure) (next-figure figure) (plays-next player) nums))))