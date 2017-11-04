(require "./evaluation.lisp")

;;;;;;;;;;; Sandwich detection functions

;detects sandwich vertically
(defun detect-sandwich-vertically (table)
    )

;detects sandwich diagonally
(defun detect-sandwich-diagonally (table)
    )

;detects sandwich
(defun detect-sandwich (table)
    (or (detect-sandwich-vertically table) (detect-sandwich-diagonally table)))

;;;;;;;;;;; Gameover detection functions

;detects gameover 
(defun is-game-over(table)
    )