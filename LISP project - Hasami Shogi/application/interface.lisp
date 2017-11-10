(require "./game.lisp")

;;;;;;;;;;; Console UI functions

;displays the current player in the console
(defun show-current-player (previousstate currentstate player)
    (if (equal previousstate currentstate)
        (format t "~a   Na potezu je: IGRAC ~a" #\tab (1+ (plays-next player)))
        (format t "~a   Na potezu je: IGRAC ~a" #\tab (1+ player))))

;displays the number of X and O figures
(defun show-number-of-figures (table)
    (format t "~%~a Broj X-a: ~a   Broj O-a: ~a" #\tab (number-of-x table) (number-of-o table)))

;displays how move should be played
(defun show-move-example ()
    (format t "~%~%~a   Primer poteza: b 2 d 2~% " #\tab))

;displays input message
(defun show-user-input ()
    (format t "~%~%~a   Unesi potez: " #\tab))

;displays initial game message
(defun show-initial-message ()
    (format t "~%~a   Ko igra prvi, 0-igrac igra pvi, 1-igrac igra drugi: " #\tab))

;displays message when loading a game
(defun loading-message ()
    (ext:run-shell-command "cls")
    (show-ascii-art)
    (format t "~%~a   Unesi dimenzije table: " #\tab))

;displays the table with column indicators and row letter indicators
(defun show-table (table nums)
    (format t "~%~a       " #\tab)
    (print-numbers nums)
    (format t "~%~%")
    (show-table-rec table nums))

(defun show-ascii-art ()
    (format t "                      
~a _____                   _ 
~a|  |  |___ ___ ___ _____|_|
~a|     | .'|_ -| .'|     | |
~a|__|__|__,|___|__,|_|_|_|_|  
~a          _           _     
~a      ___| |_ ___ ___|_|    
~a     |_ -|   | . | . | |    
~a     |___|_|_|___|_  |_|    
~a                 |___|      
~a ___________________________
~a|___________________________|
" #\tab #\tab #\tab #\tab #\tab #\tab #\tab #\tab #\tab #\tab #\tab))

(defun end-of-interface ()
    (format t "
~a ___________________________
~a|___________________________|" #\tab #\tab))

;displays the entire console UI
(defun show-ui (previousstate currentstate nums player)
    (ext:run-shell-command "cls")
    (show-ascii-art)
    (show-table currentstate nums)
    (show-number-of-figures currentstate)
    (end-of-interface)
    (show-move-example)
    (show-current-player previousstate currentstate player)
    (show-user-input))