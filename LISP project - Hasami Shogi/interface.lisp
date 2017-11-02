(require "./game.lisp")

(defun showCurrentPlayer (player)
    (format t "~%~%~aNa potezu je: IGRAC ~a" #\tab (+ player 1)))

(defun shownumber-of-x-figures (table)
    (format t "~%~aBroj X figurica je: ~a" #\tab (number-of-x table)))

(defun shownumber-of-o-figures (table)
    (format t "~%~aBroj O figurica je: ~a" #\tab (number-of-o table)))

(defun show-table (table nums)
    (format t "~%~a    " #\tab)
    (print-numbers nums)
    (format t "~%~%")
    (show-table-rec table nums))