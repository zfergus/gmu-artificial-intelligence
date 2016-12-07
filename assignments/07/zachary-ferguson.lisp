;; ASSIGNMENT 7: A MANCALA PLAYER
;; DUE: THURSDAY, DECEMBER 8, at MIDNIGHT
;;
;; Written by: Zachary Ferguson
;;
;; Computer player for playing Moncala.
;;
;; The first thing in your function is a package declaration.
;; You should name your package something beginning with a colon
;; followed by your full name with hyphens for spaces.
;; I named my package :sean-luke .  Keep in mind that the name
;; is CASE-INSENSITIVE.  The only symbol you should export is the
;; symbol COMPUTER-MAKE-MOVE, which should be the name of your top-
;; level computer move function.  Name your file the same
;; name as your package declaration, minus the colon.  For example,
;; my file is named "sean-luke.lisp"
;;
;; Once you've done this, you need to write your code.  Here
;; is a rough sketch of three functions you will find handy.
;; You don't need to implement them like this, except for the
;; COMPUTER-MAKE-MOVE function. You can write your code in
;; any fashion you like in this file, so long as you do a
;; proper alpha-beta heuristic search and your evaluation
;; function is stronger than just comparing the differences in
;; the mancalas.
;;
;;
;; INSTRUCTIONS FOR THE PROJECT
;; ----------------------------
;;
;; Turn in your foo-bar.lisp file (or whatever the proper name is given
;; your own name) as an attachment to the TA by November 30 at Midnight.
;; To obtain full credit, the code in your file must:
;;
;; 0. Compile cleanly with no warnings.
;; 1. Have a properly defined COMPUTER-MAKE-MOVE function.
;; 2. Have a properly defined package.
;; 3. Perform correct alpha-beta search.
;; 4. Have a board evaluation function which is SMARTER than just
;;    the difference in stones between the mancalas.  That is: it beats
;;    the difference-in-stones function.
;; 5. Perform correctly regardless of the settings of *num-pits*,
;;    *initial-stones-per-pit*, *go-again-rule*, and *big-win-rule*.
;; 6. Be well commented and good-quality lisp code.
;; 7. Have a 500 word summary at the end of the file detailing the
;;    decisions you made in writing your code, how it performs, etc.
;;
;; The subject line of your email message should be "CS480 Mancala"
;; or "CS580 Mancala" depending on the class you are in.


(defpackage :zachary-ferguson
  (:use :common-lisp-user :common-lisp)
  (:export computer-make-move))
(in-package :zachary-ferguson)


(defun alpha-beta (state current-depth max-depth max-player expand terminal
  evaluate alpha beta)
  "Does alpha-beta search.  Note that there is the addition of a variable
called MAX-PLAYER rather than a function which specifies if it's max's turn.
It's just more convenient in this system. The MAX-PLAYER variable is set to
either *player-1* or to *player-2* and indicates if *player-1* or *player-2*
should be considered 'max' (the other player is then considered to be 'min')"
  ;; If the state is terminal or we have reached max depth
  (if (or (funcall terminal state) (> current-depth max-depth))
    (funcall evaluate state max-player) ;; return the evaluated value of S
  ;; Otherwise,
    (dolist (child (funcall expand state) ;; Possible next moves
      (if (equalp max-player (state-turn state)) alpha beta))
      ;; Recursivly determine best child state
      (let ((mm (alpha-beta child (+ current-depth 1) max-depth max-player
        expand terminal evaluate alpha beta)))
        ;; Set the appropriate value for the current player
        (if (equalp max-player (state-turn state))
          (setf alpha (max alpha mm))
          (setf beta  (min beta  mm))))
      ;; Cut off if alpha crossed over beta
      (if (>= alpha beta)
        (return-from alpha-beta
          ;; Determine proper value to return depending on current player
          (if (equalp max-player (state-turn state)) beta alpha))))))


(definline sum-player-owned-pits (state player)
  " Sums all the player owned pit including the moncala. "
  (let ((start-index (left-pit player)))
    (reduce #'+ ;; Sum the number of seeds in the player's pits
      (subseq (state-board state) start-index (+ start-index *num-pits* 1)))))


(definline map-range-to-range (x in-min in-max out-min out-max)
  "Map from one range to another range.
Implementation based on arduino map function:
https://www.arduino.cc/en/Reference/Map"
  (+ out-min (/ (* (- x in-min) (- out-max out-min)) (- in-max in-min))))


(defparameter num-stones (* 2 *num-pits* *initial-stones-per-pit*)
  "Total number of stones on the board.")


(defun evaluate (state max-player)
  "Evaluates the game situation for MAX-PLAYER. Returns the value of STATE for
MAX-PLAYER (who is either *player-1* or *player-2*).  This should be a value
ranging from *min-wins* to *max-wins*."
  ;; Heuristic found here: http://blog.hackerrank.com/mancala/
  ;; Map from [-48, 48] to [min-wins, max-wins]
  (map-range-to-range (- (sum-player-owned-pits state max-player)
     (sum-player-owned-pits state (other-player max-player)))
     (- num-stones) num-stones *min-wins* *max-wins*))


(defun computer-make-move (state max-depth)
  "Given a state, makes a move and returns the new state. If there is no move
to make (end of game) returns nil. Each time this function calls the top-level
alpha-beta function to search for the quality of a state, computer-make-move
should print out the state (using PRINT, not PRINT-STATE) that is being
searched. Only search up to max-depth.  The computer should assume that he is
the player who's turn it is right now in STATE"
  ;; Initialize the best move to make
  (let ((best nil)
        (best-score *min-wins*))
    ;; Examine all the possible states from the current state
    (dolist (child (moves state) best)
      ;; Call alpha-beta min-max search on the child to get the score.
      (let ((score (alpha-beta (print child) 0 max-depth (state-turn state)
                      #'moves #'game-overp #'evaluate *min-wins* *max-wins*)))
        ;; Save only the best score and state found
        (if (> score best-score) (progn
          (setf best child)
          (setf best-score score)))))))


;; Go back to the standard :cl-user namespace
(in-package :cl-user)
