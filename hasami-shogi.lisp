;;;;;;;;;;; MiniMax with Alpha Beta pruning 

;this is a searcher function for the best available move in Hasami Shogi game
(defun minimax-a-b (table depth a b figure)
    (let 
        ((moveslist (all-available-moves table figure))
        (bestvalue '()) (bestmove '()) (state '())
        (tmp '()) (alpha a) (beta b))
        (cond 
            ((or (null moveslist) (= depth 0)) (setf bestvalue (evaluate-basic table figure)))
            ((equal figure 'X) ;X plays the move - Maximizing player
                (progn
                    (setf bestvalue -999999)
                    (loop for move in moveslist do
                        (progn 
                            (setf state (set-next-state table 'X (car move) (cadr move) (caddr move) (cadddr move)))
                            (setf tmp (minimax-a-b state (- depth 1) alpha beta 'O))
                            (if (> (car tmp) bestvalue) (progn (setf bestvalue (car tmp)) (setf bestmove move)))
                            (setf alpha (max alpha bestvalue))
                            (if (>= alpha beta) (return-from minimax-a-b (list bestvalue bestmove)))))))
            ((equal figure 'O) ;O plays the move - Minimizing player
                (progn
                    (setf bestvalue 999999)
                    (loop for move in moveslist do
                        (progn
                            (setf state (set-next-state table 'O (car move) (cadr move) (caddr move) (cadddr move)))
                            (setf tmp (minimax-a-b state (- depth 1) alpha beta 'X))
                            (if (< (car tmp) bestvalue) (progn (setf bestvalue (car tmp)) (setf bestmove move)))
                            (setf beta (min beta bestvalue))
                            (if (>= alpha beta) (return-from minimax-a-b (list bestvalue bestmove)))))))) 
                                (list bestvalue bestmove)))

;;;;;;;;;;; Application 

;this code runs when program starts
(defun initialize-game (n)
    (if (< n 5) '() ;exits the game if the dimensions are below 5, not playable
        (progn
            (show-initial-message)
            (game-loop '() (create-table n) 'X (mod (read) 2) (create-numbers n))))) ;initial game state

;this code runs until the game is over
(defun game-loop (previousstate currentstate figure player nums)
    (show-ui previousstate currentstate nums player)
    (cond
        ((check-winner currentstate) '())
        ((equal previousstate currentstate)
            (game-loop currentstate (player-move currentstate (next-figure figure)) figure player nums))
        ((equal player 1) 
            (game-loop currentstate (ai-move currentstate figure) (next-figure figure) (plays-next player) nums))
        (t (game-loop currentstate (player-move currentstate figure) (next-figure figure) (plays-next player) nums))))

;;;;;;;;;;; Evaluation functions

;basic evaluation function
(defun evaluate-basic (table figure)
    (+ (evaluate-figure-number-difference table 100)
        (if (equal figure 'X) 100 -100)
        (if (equal (car (get-table-element table 3 6)) 'X) 1000 -1000)))

;evaluetes state based on number of figures
(defun evaluate-figure-number-difference (table factor)
    (* (- (number-of-x table) (number-of-o table)) factor))

;counts number of the same consecutive elements
(defun count-consecutives (column element counter maximum)
    (cond
        ((null column) (if (> maximum counter) maximum counter))
        ((equal element (car column)) (count-consecutives (cdr column) element (+ counter 1) maximum))
        (t (count-consecutives (cdr column) element 0 (if (> maximum counter) maximum counter)))))     

;evaluetes state of row/column/diagonal based on number of consecutive figures of the same type
(defun evaluate-consecutives (column element factor)
    (* (count-consecutives column element 0 0) factor))   

;evaluate-consecutives for entire table for one type of figure
(defun evaluate-all-consecutives (columns element factor)
    (if (null columns) 0
        (+ (evaluate-consecutives (car columns) element factor)
            (evaluate-all-consecutives (cdr columns) element factor))))

;evaluate-consecutives for entire table
(defun evaluate-consecutives-result (columns factor)
    (+ (evaluate-all-consecutives columns 'X factor) 
        (evaluate-all-consecutives columns 'O (* factor -1))))

;;;;;;;;;;; Table functions

;creates initialized table for the game
(defun create-table (n)
    (create-table-rec n n))

;tests if input is out of bounds 
(defun valid-bounds (table i j)
    (and (< i (length table)) (>= i 0) (< j (length table)) (>= j 0)))

;gets an element from the i-th row and j-th column of the table 
(defun get-table-element (table i j)
    (if (valid-bounds table i j)
        (get-list-element (get-list-element table i) j)))

;sets value of an element from the i-th row and j-th column of the table
(defun set-table-element (table el i j)
    (if (valid-bounds table i j)
        (set-list-element table (set-list-element (get-list-element table i) el j) i) table))

;;;;;;;;;;; Gameplay functions

;changes game state if the move is valid
(defun set-next-state (table figure fromI fromJ toI toJ)
    (if (is-valid-move table figure fromI fromJ toI toJ)
            (remove-sandwich-figures 
                (set-table-element (set-table-element table figure toI toJ) '- fromI fromJ)
                (list-of-sandwiched-figures 
                    (set-table-element (set-table-element table figure toI toJ) '- fromI fromJ)
                    figure toI toJ)) table))

;finds all available moves for one type of figures
(defun all-available-moves (table figure)
    (available-moves table figure 0 0))

;finds all available moves for one type of figures recursively
(defun available-moves (table figure fromI fromJ)
    (if (or (null table) (= fromI (length table))) '()
        (append (if (equal (car (get-table-element table fromI fromJ)) figure)
            (list-of-available-moves-for-figure table fromI fromJ))
            (cond
                ((= fromJ (length table)) (available-moves table figure (+ fromI 1) 0))
                (t (available-moves table figure fromI (+ fromJ 1)))))))

;creates the list of all available moves
(defun list-of-available-moves-for-figure (table fromI fromJ)
    (append 
        (available-moves-left table fromI fromJ)
        (available-moves-right table fromI fromJ)
        (available-moves-top table fromI fromJ)
        (available-moves-bottom table fromI fromJ)))

;creates the list of all available moves to the left of the figure
(defun available-moves-left (table fromI fromJ)
    (append 
        (mapcar (lambda (x) (append (list fromI) (list fromJ) x)) (available-moves-left-rec table fromI fromJ))
        (if (and 
            (not (equal (car (get-table-element table fromI (- fromJ 1))) '-))
            (equal (car (get-table-element table fromI (- fromJ 2))) '-))
                (list (list fromI fromJ fromI (- fromJ 2))))))

;creates the list of all available moves to the right of the figure
(defun available-moves-right (table fromI fromJ)
    (append 
        (mapcar (lambda (x) (append (list fromI) (list fromJ) x)) (available-moves-right-rec table fromI fromJ))
        (if (and
            (not (equal (car (get-table-element table fromI (+ fromJ 1))) '-))
            (equal (car (get-table-element table fromI (+ fromJ 2))) '-))
                (list (list fromI fromJ fromI (+ fromJ 2))))))

;creates the list of all available moves above the figure
(defun available-moves-top (table fromI fromJ)
    (append 
        (mapcar (lambda (x) (append (list fromI) (list fromJ) x)) (available-moves-top-rec table fromI fromJ))        
        (if (and
            (not (equal (car (get-table-element table (- fromI 1) fromJ)) '-))
            (equal (car (get-table-element table (- fromI 2) fromJ)) '-))
                (list (list fromI fromJ (- fromI 2) fromJ)))))

;creates the list of all available moves below the figure
(defun available-moves-bottom (table fromI fromJ)
    (append 
        (mapcar (lambda (x) (append (list fromI) (list fromJ) x)) (available-moves-bottom-rec table fromI fromJ))        
        (if (and
            (not (equal (car (get-table-element table (+ fromI 1) fromJ)) '-))
            (equal (car (get-table-element table (+ fromI 2) fromJ)) '-))
                (list (list fromI fromJ (+ fromI 2) fromJ)))))

;tests if the move is valid
(defun is-valid-move (table figure fromI fromJ toI toJ)
    (and 
        (valid-bounds table fromI fromJ) (valid-bounds table toI toJ)
        (sublist-exists (list fromI fromJ toI toJ) (list-of-available-moves-for-figure table fromI fromJ))
        (equal figure (car (get-table-element table fromI fromJ)))))

;user plays the move, user enters the coordinates using standard input
(defun player-move (table figure)
    (set-next-state table figure (char-to-index (read-char)) (1- (read)) (char-to-index (read-char)) (1- (read))))

;AI plays the move
(defun ai-move (table figure)
    (let ((move (cadr (minimax-a-b table 2 -999999 999999 figure))))
        (set-next-state table figure (car move) (cadr move) (caddr move) (cadddr move))))

;removes figures that are in sandwich
(defun remove-sandwich-figures (table lst)
    (cond
        ((null lst) table)
        ((null (cddr lst)) (set-table-element table '- (car lst) (cadr lst))) 
        (t (remove-sandwich-figures (set-table-element table '- (car lst) (cadr lst)) (cddr lst)))))

;decides who plays next
(defun plays-next (player)
    (logxor player 1))

;decides which figures can be played next
(defun next-figure (figure)
    (if (equal figure 'X) 'O 'X))

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
(defun show-user-input (previousstate currentstate)
    (if (equal previousstate currentstate) 
        (format t "~%~%~a   Nevalidan potez,~%~a   unesi ponovo: " #\tab #\tab)
        (format t "~%~%~a   Unesi potez: " #\tab)))

;displays initial game message
(defun show-initial-message ()
    (format t "~%~a   Izaberite ko igra prvi,~%~a   0-igrac igra pvi, ~%~a   1-igrac igra drugi: " #\tab #\tab #\tab))

;displays message when loading a game
(defun loading-message ()
    ;(run-shell-command "cls")
    (show-ascii-art)
    (format t "~%~a   Unesi dimenzije table: " #\tab))

;displays the table with column indicators and row letter indicators
(defun show-table (table nums)
    (format t "~%~a       " #\tab)
    (print-numbers nums)
    (format t "~%~%")
    (show-table-rec table nums))

;displays the ASCII art in the console
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

;displays end of ASCII frame
(defun end-of-interface ()
    (format t "
~a ___________________________
~a|___________________________|" #\tab #\tab))

(defun win-message (player)
    (format t "~%~%~a   POBEDIO JE: IGRAC ~a!" #\tab (1+ (plays-next player))))

;displays the entire console UI
(defun show-ui (previousstate currentstate nums player)
    (run-shell-command "clear")
    (show-ascii-art)
    (show-table currentstate nums)
    (show-number-of-figures currentstate)
    (end-of-interface)
    (show-move-example)
    (show-current-player previousstate currentstate player)
    (if (check-winner currentstate) 
        (win-message player) 
        (show-user-input previousstate currentstate)))
        
;;;;;;;;;;; Recursive functions for some operations with table 

;creates row with all X values recursively
(defun create-x-row-rec (n)
    (if (zerop n) '()
        (append '(X) (create-x-row-rec (1- n)))))

;creates row with all O values recursively
(defun create-o-row-rec (n)
    (if (zerop n) '()
        (append '(O) (create-o-row-rec (1- n)))))

;creates row with all - values recursively
(defun create-free-row-rec (n)
    (if (zerop n) '()
        (append '(-) (create-free-row-rec (1- n)))))

;creates the table recursively recursively
(defun create-table-rec (n i)
    (if (zerop i) '()
    (append
        (cond
            ((and (> i 0) (<= i 2)) (list (create-o-row-rec n)))
            ((and (> i 2) (<= i (- n 2))) (list (create-free-row-rec n)))
            ((and (> i (- n 2)) (<= i n) (list (create-x-row-rec n))))) 
            (create-table-rec n (1- i)))))

;displays the table recursively
(defun show-table-rec (table numbers)
    (if (null table) '()
        (progn 
            (format t "~a   ~a  ~a~%" #\tab (code-char (+ 64 (car numbers))) (car table))
            (show-table-rec (cdr table) (cdr numbers)))))

;sets an element of the list recursively
(defun set-list-element (lst el n)
    (if (null lst) (if (listp el) el (list el)))
    (if (= n 0) 
        (append (list el) (cdr lst))
        (append (list (car lst)) (set-list-element (cdr lst) el (1- n)))))

;gets an element from the list recursively
(defun get-list-element (lst n)
    (if (> n (length lst)) '())
    (if (zerop n) 
        (if (listp (car lst)) (car lst) (list (car lst)))
            (append '() (get-list-element(cdr lst) (1- n)))))

;gets a column by index
(defun get-column (table n)
    (if (null table) '()
        (append (list (nth n (car table))) (get-column (cdr table) n))))

;calculates the number of X atoms in the list recursively
(defun number-of-x-in-list (lst)
    (if (null lst) 0
        (+ 0 (if (equal (car lst) 'X) 1 0) (number-of-x-in-list (cdr lst)))))

;calculates the number of X atoms in the table recursively
(defun number-of-x (table)
    (if (null table) 0
        (+ (number-of-x-in-list (car table)) (number-of-x (cdr table)))))

;calculates the number of O atoms in the list recursively
(defun number-of-o-in-list (lst)
    (if (null lst) 0
        (+ 0 (if (equal (car lst) 'O) 1 0) (number-of-o-in-list (cdr lst)))))

;calculates the number of O atoms in the table recursively
(defun number-of-o (table)
    (if (null table) 0
        (+ (number-of-o-in-list (car table)) (number-of-o (cdr table)))))

;calculates available moves to the left of the selected figure recursively
(defun available-moves-left-rec (table fromI fromJ)
    (if (>= fromJ 0)
        (if (equal '(-) (get-table-element table fromI (- fromJ 1)))
            (append (list (list fromI (- fromJ 1))) (available-moves-left-rec table fromI (- fromJ 1))))))

;calculates available moves to the right of the selected figure recursively
(defun available-moves-right-rec (table fromI fromJ)
    (if (> (length table) fromJ)
        (if (equal '(-) (get-table-element table fromI (+ fromJ 1)))
            (append (list (list fromI (+ fromJ 1))) (available-moves-right-rec table fromI (+ fromJ 1))))))

;calculates available moves above the selected figure recursively
(defun available-moves-top-rec (table fromI fromJ)
    (if (>= fromI 0)
        (if (equal '(-) (get-table-element table (- fromI 1) fromJ))
            (append (list (list (- fromI 1) fromJ)) (available-moves-top-rec table (- fromI 1) fromJ)))))

;calculates available moves below the selected figure recursively
(defun available-moves-bottom-rec (table fromI fromJ)
    (if (> (length table) fromI)
        (if (equal '(-) (get-table-element table (+ fromI 1) fromJ))
            (append (list (list (+ fromI 1) fromJ)) (available-moves-bottom-rec table (+ fromI 1) fromJ)))))

;calculates the sandwich to the left recursively
(defun sandwich-left-rec (table figure i j)
    (if (and (>= j 0) (equal (car (get-table-element table i (- j 1))) (next-figure figure)))
        (append (list i (- j 1)) (sandwich-left-rec table figure i (- j 1)))))

;calculates the sandwich to the right recursively
(defun sandwich-right-rec (table figure i j)
    (if (and (< j (length table)) (equal (car (get-table-element table i (+ j 1))) (next-figure figure)))
        (append (list i (+ j 1)) (sandwich-right-rec table figure i (+ j 1)))))

;calculates the sandwich to the bottom recursively
(defun sandwich-bottom-rec (table figure i j)
    (if (and (< i (length table)) (equal (car (get-table-element table (+ i 1) j)) (next-figure figure)))
        (append (list (+ i 1) j) (sandwich-bottom-rec table figure (+ i 1) j))))

;calculates the sandwich to the top recursively
(defun sandwich-top-rec (table figure i j)
    (if (and (>= i 0) (equal (car (get-table-element table (- i 1) j)) (next-figure figure)))
        (append (list (- i 1) j) (sandwich-top-rec table figure (- i 1) j))))

;finds sublist in a list
(defun sublist-exists (sub lst)
    (if (not (null lst))
        (or (equal sub (car lst)) (sublist-exists sub (cdr lst)))))

;gets right diagonal of an element
(defun right-diagonal-rec (table i j)
    (if (and (< i (length table)) (>= i 0) (< j (length table)) (>= j 0))
        (append (get-table-element table i j) (right-diagonal-rec table (+ i 1) (+ j 1)))))

;gets left diagonal of an element
(defun left-diagonal-rec (table i j)
    (if (and (< i (length table)) (>= i 0) (< j (length table)) (>= j 0))
        (append (get-table-element table i j) (left-diagonal-rec table (+ i 1) (- j 1)))))

;;;;;;;;;;; Recursive functions for some operations with numbers 

;creates a list of numbers from 1 to n recursively
(defun create-numbers (n)
    (if (= n 0) '()
    (append (create-numbers (1- n)) (list n))))

;displays a list of numbers from 1 to n recursively
(defun print-numbers (numbers)
    (if (null numbers) '()
        (progn 
            (format t "~a " (car numbers))
            (print-numbers (cdr numbers)))))

;;;;;;;;;;; Functions for some operations with chars 

;calculates index from ASCII chars input
(defun char-to-index (ch)
    (cond
        ((< (char-code ch) (char-code #\a)) (- (char-code ch) (char-code #\A)))
        ((>= (char-code ch) (char-code #\a)) (- (char-code ch) (char-code #\a)))))

;;;;;;;;;;; Sandwich detection functions

;detects sandwich to the left
(defun sandwich-left (table figure i j)
    (cond
        ((null (sandwich-left-rec table figure i j)) '())
        ((equal (car (get-table-element table 
            (cadr (reverse (sandwich-left-rec table figure i j)))
            (- (car (reverse (sandwich-left-rec table figure i j))) 1))) figure) (sandwich-left-rec table figure i j))))

;detects sandwich to the right
(defun sandwich-right (table figure i j)
    (cond
        ((null (sandwich-right-rec table figure i j)) '())
        ((equal (car (get-table-element table 
            (cadr (reverse (sandwich-right-rec table figure i j)))
            (+ (car (reverse (sandwich-right-rec table figure i j))) 1))) figure) (sandwich-right-rec table figure i j))))

;detects sandwich to the top
(defun sandwich-top (table figure i j)
    (cond
        ((null (sandwich-top-rec table figure i j)) '())
        ((equal (car (get-table-element table 
            (- (cadr (reverse (sandwich-top-rec table figure i j))) 1)
            (car (reverse (sandwich-top-rec table figure i j))))) figure) (sandwich-top-rec table figure i j))))

;detects sandwich to the bottom
(defun sandwich-bottom (table figure i j)
    (cond
        ((null (sandwich-bottom-rec table figure i j)) '())
        ((equal (car (get-table-element table 
            (+ (cadr (reverse (sandwich-bottom-rec table figure i j))) 1)
            (car (reverse (sandwich-bottom-rec table figure i j))))) figure) (sandwich-bottom-rec table figure i j))))

;detects sandwich
(defun list-of-sandwiched-figures (table figure i j)
    (append 
        (sandwich-left table figure i j)
        (sandwich-right table figure i j)
        (sandwich-top table figure i j)
        (sandwich-bottom table figure i j)))

;;;;;;;;;;; Gameover detection functions

;check winner
(defun check-winner (table)
    (or (x-victory table) (o-victory table)))

;checks if O won
(defun o-victory (table)
    (cond 
        ((< (number-of-x table) 5) t)
        (t (or (five-o-vertically table) (five-o-diagonally table)))))  

;checks if X won
(defun x-victory (table)
    (cond 
        ((< (number-of-o table) 5) t)
        (t (or (five-x-vertically table) (five-x-diagonally table)))))  

;detects five consecutive Xs in a column
(defun five-x-vertically (table)
    (vertical-victory (cddr table) 'X (length (cddr table))))

;detects five consecutive Os in a column
(defun five-o-vertically (table)
    (vertical-victory (cddr (reverse table)) 'O (length (cddr table))))

;detects five consecutive Xs in a diagonal
(defun five-x-diagonally (table)
    (diagonal-victory (all-diagonals (cddr table)) 'X))

;detects five consecutive Os in a diagonal
(defun five-o-diagonally (table)
    (diagonal-victory (all-diagonals (cddr (reverse table))) 'O))

;detects five consecutive elements in a column
(defun has-five (column element counter)
    (cond 
        ((null column) (equal counter 5))
        ((equal counter 5) t)
        ((equal element (car column)) (has-five (cdr column) element (+ counter 1)))
        (t (has-five (cdr column) element 0))))

;checks if there is victory due to vertical 5 
(defun vertical-victory (table element n) 
    (if (< n 1) nil
        (or (has-five (get-column table (- n 1)) element 0) (vertical-victory table element (- n 1)))))

;checks if there is victory due to diagonal 5 
(defun diagonal-victory (lst element) 
    (if (not (null lst))
        (or (has-five (car lst) element 0) (diagonal-victory (cdr lst) element))))

;gets all right diagonals of a row   
(defun get-row-right-diagonals (table i j)
    (if (< j (length table))
        (append
            (if (>= (length (right-diagonal-rec table i j)) 5)
            (list (right-diagonal-rec table i j))) (get-row-right-diagonals table i (+ j 1)))))

;gets all right diagonals of a table
(defun get-right-diagonals (table i j)
    (if (< i (length table))
        (append (get-row-right-diagonals table i j) (get-right-diagonals table (+ i 1) j))))

;gets all left diagonals of a row   
(defun get-row-left-diagonals (table i j)
    (if (>= j 0)
        (append 
            (if (>= (length (left-diagonal-rec table i j)) 5)
            (list (left-diagonal-rec table i j)))(get-row-left-diagonals table i (- j 1)))))

;gets all right diagonals of a table
(defun get-left-diagonals (table i j)
    (if (< i (length table))
        (append 
            (get-row-left-diagonals table i j) (get-left-diagonals table (+ i 1) j))))

;gets all diagonals of a table
(defun all-diagonals (table)
    (append
        (get-right-diagonals table 0 0) (get-left-diagonals table 0 (1- (length table)))))

;;;;;;;;;;; Main function

;this code runs when program starts
(defun main ()
    (loading-message)
    (initialize-game (read)))   

;;;;;;;;;;; Execution

(main)