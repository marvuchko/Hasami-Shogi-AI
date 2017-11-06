(require "./application/app.lisp")

;;;;;;;;;;; Main function

;this code runs when program starts
(defun main ()
    (loading-message)
    (initialize-game (read)))   

;;;;;;;;;;; Execution

(main)