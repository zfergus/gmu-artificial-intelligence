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
    (funcall evaluate state) ;; return the evaluated value of S
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


(defun evaluate (state max-player)
  "Evaluates the game situation for MAX-PLAYER. Returns the value of STATE for
MAX-PLAYER (who is either *player-1* or *player-2*).  This should be a value
ranging from *min-wins* to *max-wins*."
  ;; TODO: Write this function properly.
  (let ((player-1-score (score state *player-1*))
        (player-2-score (score state *player-2*))
        (player-1-wins (if (equalp max-player *player-1*)
          *max-wins* *min-wins*))
        (player-2-wins (if (equalp max-player *player-2*)
          *max-wins* *min-wins*)))
    (if (= player-1-score player-2-score) 0
      (if (> player-1-score player-2-score) player-1-wins player-2-wins))))


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
