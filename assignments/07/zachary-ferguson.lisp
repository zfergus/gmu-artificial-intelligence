;; ASSIGNMENT 7: A MANCALA PLAYER
;;
;; Written by: Zachary Ferguson
;;
;; Program to play Mancala against an oponent player.

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
)


(defun evaluate (state max-player)
  "Evaluates the game situation for MAX-PLAYER. Returns the value of STATE for
MAX-PLAYER (who is either *player-1* or *player-2*).  This should be a value
ranging from *min-wins* to *max-wins*."
)


(defun computer-make-move (state max-depth)
  "Given a state, makes a move and returns the new state. If there is no move
to make (end of game) returns nil. Each time this function calls the top-level
alpha-beta function to search for the quality of a state, computer-make-move
should print out the state (using PRINT, not PRINT-STATE) that is being
searched. Only search up to max-depth.  The computer should assume that he is
the player who's turn it is right now in STATE"
)

;; Go back to the standard :cl-user namespace
(in-package :cl-user)
