(require "./game.lisp")

;;;;;;;;;;; Console UI functions

;displays the current player in console
(defun show-current-player (player)
    (format t "~%~%~aNa potezu je: IGRAC ~a" #\tab (+ player 1)))

;displays the number of X figures
(defun show-number-of-x-figures (table)
    (format t "~%~aBroj X figurica je: ~a" #\tab (number-of-x table)))

;displays the number of O figures
(defun show-number-of-o-figures (table)
    (format t "~%~aBroj O figurica je: ~a" #\tab (number-of-o table)))

;displays the table with column number indicators and row letter indicators
(defun show-table (table nums)
    (format t "~%~a    " #\tab)
    (print-numbers nums)
    (format t "~%~%")
    (show-table-rec table nums)
    (format t "~%~%"))