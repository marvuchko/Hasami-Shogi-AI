(require "./interface.lisp")

;;;;;;;;;;; Application 

;this code runs when program starts
(defun initialize-game (n)
    (if (< n 5) '() ;exits the game if the dimensions are below 5, not playable
        (progn
            (show-initial-message)
            (game-loop '() '() (create-table n) 'X (read) (create-numbers n))))) ;initial game state

;this code runs until the game is over
(defun game-loop (gameover currentState nextState figure player nums)
    (show-ui currentState nextState nums player)
    (cond
        (gameover '())
        ((if (equal currentState nextState)
            (game-loop gameover nextState (play-move nextState (next-figure figure)) figure player nums)
            (game-loop gameover nextState (play-move nextState figure) (next-figure figure) (plays-next player) nums)))
        ))