(require "./interface.lisp")

;;;;;;;;;;; Application 

;this code runs when program starts
(defun initialize-game (n)
    (if (< n 5) '() ;exits the game if the dimensions are below 5, not playable
        (progn
            (show-initial-message)
            (game-loop '() '() (create-table n) 'X (mod (read) 2) (create-numbers n))))) ;initial game state

;this code runs until the game is over
(defun game-loop (gameover previousstate currentstate figure player nums)
    (show-ui previousstate currentstate nums player)
    (cond
        (gameover '())
        ((equal previousstate currentstate)
            (game-loop (is-game-over currentstate) currentstate (play-move currentstate (next-figure figure)) figure player nums))
        (t (game-loop (is-game-over currentstate) currentstate (play-move currentstate figure) (next-figure figure) (plays-next player) nums))))