;; ASSIGNMENT 7: A MANCALA PLAYER

;; DUE: THURSDAY, DECEMBER 8, at MIDNIGHT

;; In this project you will build a program that play Mancala, hopefully
;; one that beats you at the game.

;; You will write all of your code, and all of your project comments,
;; in ANOTHER file called "foo-bar.lisp" if your name was Foo Bar.  For
;; example, if I were to turn in my project, it would be in a file
;; called "sean-luke.lisp".  You are NOT to turn in the mancala.lisp file;
;; we already have that, thank you very much.  You are also not to move
;; any macros, functions, constants, or global variable declarations from
;; the mancala.lisp file and into the foo-bar.lisp file.  See the very end of
;; this file for a description of the foo-bar.lisp file and what goes
;; into it.  You don't type ANYTHING in the mancala.lisp file -- it stays
;; pristine and clean.  You will turn in your foo-bar.lisp file as your
;; homework assignment.

;; Your foo-bar.lisp file should compile cleanly with NO warnings of
;; ANY KIND.


;; THE MAIN FILE
;; -------------

;; The mancala.lisp file defines a bunch of stuff that you will find useful.
;; First, it defines some constants and parameters.  *player-1*
;; represents player 1 and *player-2* represents player 2.  Your
;; program might play as either player 1 or player 2.  *max-wins*
;; is a value which represents max winning.  *min-wins* is a value
;; which represents min winning.  Your board evaluation should return
;; numbers somewhere between these two (between -1 and 1 is fine).

;; *num-pits* specifies how long your board is.

;; *initial-stones-per-pit* specifies how many stones go in each pit.

;; *go-again-rule*, if t, causes the game to play the game with the
;; rule that if your last stone goes in your mancala, you get to go
;; again.  That can be turned off.

;; *big-win-rule*, if t, causes the game to play the game with the
;; rule that if your last stone goes in an empty pit on your side, you
;; get to put all the stones opposite into your opponent's mancala.
;; That can be turned off too.

;; STATE is the structure which represents game states.  It consists
;; of a board (an array of numbers representing the pits and mancalas
;; counter-clockwise from bottom-left) and TURN, which is either
;; *player-1* or *player-2*.  Player-1's pits are on the bottom
;; and player-2's pits are on the top.  There are a number of
;; functions for getting board position information and for manipulating,
;; printing, or otherwise using the state structure.

;; In your foo-bar.lisp file you will write the code which defines the
;; decision-making logic of the program.  This will culminate in a single
;; function, COMPUTER-MAKE-MOVE, which takes a STATE and a MAX-DEPTH,
;; and makes a move given that STATE, returning a new STATE, or NIL
;; if there's no move to make (because the game is over).  MAX-DEPTH
;; specifies the maximum depth to search.  COMPUTER-MAKE-MOVE will
;; probably work by calling alpha-beta on several different candidate
;; states.  Each time it does so, it should print out the state to
;; the screen using print (as in something like   (print new-state)  )

;; When handed in, your decision-making logic should print NOTHING to
;; the screen AT ALL when being called EXCEPT for the print statement
;; detailed above.

;; Near the end of the file are two functions that you should become
;; intimately familiar with.  PLAY-GAME lets you plug in your
;; COMPUTER-MAKE-MOVE function in and play it yourself.  Here is an example
;; of playing the game with a max-depth of 10, where you get to go first:

;; (play-game 10 #'sean-luke:computer-make-move t)

;; Additionally there is a function called TOURNAMENT, which lets you
;; plug two different COMPUTER-MAKE-MOVE functions in and have them play
;; each other.  Imagine if Dr. Hamburger wrote a COMPUTER-MAKE-MOVE function
;; and so did I.  We could play each other, with me handicapping him
;; by letting him have a depth of 10 while I have a depth of 5, and letting
;; him go first, as:

;; (tournament 10 5
;; 	    #'sanjeev-setia:computer-make-move
;; 	    #'sean-luke:computer-make-move)


;; LOADING FILES
;; -------------

;; Because you are dealing with separate files (the mancala file and your
;; foo-bar file) you need to remember that you must load them both.
;; The mancala file must be loaded BEFORE the foo-bar file.  You might
;; enforce this by having an explicit (load "mancala") at the top of your
;; foo-bar file.

;; To play two programs against each other, you'll need to load both
;; of those program's files of course.


;; INSTRUCTIONS FOR THE PROJECT
;; ----------------------------

;; Turn in your foo-bar.lisp file (or whatever the proper name is given
;; your own name) as an attachment to the TA by November 30 at Midnight.
;; To obtain full credit, the code in your file must:

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

;; The subject line of your email message should be "CS480 Mancala"
;; or "CS580 Mancala" depending on the class you are in.


;; INSTRUCTIONS FOR THE CONTEST
;; ----------------------------

;; IF you turn in your project on time (that is, before midnight on
;; November 30) AND your code compiles cleanly with no warnings, AND
;; the code runs properly, THEN you are eligible to enter the contest.
;; Entrants to the contest will compete in a single-elimination tournament.
;;
;; The rules are as follows.  I will first handicap players by maximum search
;; depth to make them about the same speed.  That is, if your program is
;; running SLOWLY, then you will have to run at a smaller search depth in
;; the contest.  I get to decide what that is.  You might research how
;; to improve the efficiency of Lisp code (there are many compiler options)
;; as well as how to make your evaluation function run both FAST and ACCURATELY.
;;
;; I will enter everyone in a single-elimination tournament with random
;; seeding.  The results will be announced, and then the final four or so of the
;; tournament will be done during the exams (we'll carve out a little time).
;;
;; The winner of the tournament in each class will get a chance to play
;; against MY program and also will get an improvement in grade, up to,
;; but probably not equal to, a full letter grade.  Runner-ups may get candy
;; bars. That incentive enough for you?


;; PLAYING OTHER STUDENTS
;; ---------------------

;; I will permit you to test your program by pitting it against an opposing
;; classmate's program.  However:

;; 1. If you do so, you must email me letting me know that you are doing so.
;; 2. You may only do so on mason (osf1).
;; 3. You must not show ANY PART of your code to your classmate.  Load your
;;    file and have your opponent load his file using explicit LOAD
;;    statements -- do NOT open the files in text editors and then load
;;    them from there.
;; 3. You must not discuss ANYTHING with your classmate regarding the project,
;;    Lisp help, or your implementation.




(in-package :common-lisp-user)
(export '(definline *player-1* *player-2* *max-wins*
	*min-wins* *num-pits* *initial-stones-per-pit*
	*go-again-rule* *big-win-rule* state-turn
	make-state state-board copy-state
	mancala-pit left-pit pit-opposite board-size
	player-owner other-player state game-overp score
	init-state state-from-move print-state valid-movep
	moves))


(defmacro definline (name &rest stuff)
  "Defines a function and declares it to be inlined"  ;; convenient, no?
  `(progn (declaim (inline ,name))
	  (defun ,name ,@stuff)))



;;; Constants -- don't change these

(defconstant *player-1* 0)
(defconstant *player-2* 1)
(defconstant *max-wins* 10000)
(defconstant *min-wins* -10000)

;; these values guide the parameters of the game
;; (how many pits you have, how many stones start
;; in a pit)
;; if you change these values, you have to completely
;; recompile and reload the file.
(defparameter *num-pits* 6)
(defparameter *initial-stones-per-pit* 4)
(defparameter *go-again-rule* t)
(defparameter *big-win-rule* t)


;; Some functions to do some inline math for us

(definline mancala-pit (player)
  "Returns the INDEX of the mancala-pit for the given player"
  (if (= player *player-1*) *num-pits* (1+ (* 2 *num-pits*))))

(definline left-pit (player)
  "Returns the INDEX of the leftmost pit for a given player
from his perspective.  The pits run from left-pit to
left-pit + *num-pits* - 1 respectively"
  (if (= player *player-1*) 0 (1+ *num-pits*)))

(definline pit-opposite (pit)
  "Returns the pit opposite the given pit index.
Does not work for mancala pits."
  (- (* 2 *num-pits*) pit))

(definline board-size ()
  "Returns the sum of the pits and mancalas on the board"
  (+ 2 (* 2 *num-pits*)))

(definline player-owner (pit)
  "Returns the owner of a pit.  Works for mancala pits too."
  (truncate pit (1+ *num-pits*)))

(definline other-player (player)
  "Returns the other player"
  (- 1 player))


;; the game state (the board and who's turn it is)

(defstruct state
  board
  ;; The board is a one-dimensional array of integers
  ;; representing the pits, going counterclockwise, starting at
  ;; *player-1*'s leftmost pit, and ending at *player-2*'s mancala.
  ;; Each integer is the number of stones in the respective pit.  You
  ;; may assume that the total number of pits and mancalas is equal
  ;; to *board-length*"

  turn
  ;; "The turn is either *player-1* or *player-2*"
  )


;; functions manipulating game states

(defun game-overp (st)
  "Returns t if the game is over"
  (let ((initial-pit (left-pit (state-turn st)))
    (board (state-board st)))
  (dotimes (x *num-pits* t)
    (when (/= 0 (svref board (+ x initial-pit)))
	(return nil)))))

(definline score (st &optional player)
  "Returns the current score for the state.
Score is the difference in stones in the
mancalas. If NO PLAYER is provided, then the
score is the score for the player who's turn it currently
is.  If a PLAYER is provided, then the score
is for that player."
  (let ((left-player
	 (cond ((null player) (state-turn st))
	       ((= player *player-1*) *player-1*)
	       (t *player-2*))))
    (- (svref (state-board st) (mancala-pit left-player))
       (svref (state-board st) (mancala-pit (other-player left-player))))))

(defun init-state (first-player)
  "Returns a state representing an initial game, with first-player
 (*player-1* or *player-2*) being the first player."
  (let ((initial-board (make-array (board-size))))
    (dotimes (x (length initial-board))
      (setf (svref initial-board x)
	    (if (and (/= x (mancala-pit *player-2*))  ;; not opponent's mancala
		     (/= x (mancala-pit *player-1*))) ;; not my mancala
		*initial-stones-per-pit* 0)))
    (make-state :board initial-board :turn first-player)))

(defun state-from-move (st n)
  "The state resulting in sowing from pit N in st.  We define
pit N relative to the current player's position.  If pit N cannot
be sown (because there are no stones), then nil is returned."
  (let ((new-board (copy-seq (state-board st)))
	(player-turn (state-turn st))
	cur-pos
	skip-mancala my-mancala)

    ;; determine the pit indices
    (if (eq player-turn *player-1*)
	;; pits start at i=0, skipped mancala starts at *num-pits* * 2 + 1,
	;; my-mancala is *num-pits*
	(setf cur-pos (+ n (left-pit *player-1*))
	      skip-mancala (mancala-pit *player-2*)
	      my-mancala (mancala-pit *player-1*))
      ;; pits start at i=*num-pits*+1, skipped mancala starts at *num-pits*,
      ;; my-mancala is *num-pits* * 2 + 1
      (setf cur-pos (+ n (left-pit *player-2*))
	    skip-mancala (mancala-pit *player-1*)
	    my-mancala (mancala-pit *player-2*)))

    ;; loop through sowing process
    (let ((numstones (svref new-board cur-pos))
	  (board-length (board-size)))
      (unless (= numstones 0)
	;; sow
	(setf (svref new-board cur-pos) 0)
	(loop for stones downfrom numstones above 0
	      do
	      (incf cur-pos)
	      (when (= cur-pos skip-mancala) (incf cur-pos))
	      (when (= cur-pos board-length) (setf cur-pos 0))
	      (incf (svref new-board cur-pos)))

	;; deal with the big win
	(when (and *big-win-rule*
		   (= (player-owner cur-pos) player-turn)
		   (= (svref new-board cur-pos) 1)
		   (/= cur-pos my-mancala))
	  (let ((opposite-pit (pit-opposite cur-pos)))
	    (incf (svref new-board my-mancala) (svref new-board opposite-pit))
	    (setf (svref new-board opposite-pit) 0)))

	;; return the new state
	(make-state :board new-board
		    :turn (if (and *go-again-rule* (= cur-pos my-mancala))
			      player-turn  ;; player's turn stays the same
			    (- 1 player-turn)))))))  ;; other player's turn

(defun print-state (state &optional to-string)
  "Prints the state in a pleasing manner"
  ;; you don't really want to understand how this function works, do you?  :-)
  (format (not to-string)
	  "~%   ~{~3A~}      ~%---~{~3A~}--   ~%   ~{~3A~}      ~A~%~3A~{~A~}~3A~A~%   ~{~3A~}      ~A~%---~{~3A~}--   ~%   ~{~3A~}"
	  ;; player-2-moves
	  (let (bag)
	    (dotimes (x *num-pits* bag)
	      (push (+ x (left-pit *player-2*)) bag)))
	  (let (bag)
	    (dotimes (x *num-pits* bag)
	      (push "---" bag)))
	  ;; opponent's row (reversed)
	  (reverse (coerce (subseq (state-board state) (left-pit *player-2*)
				   (mancala-pit *player-2*)) 'list))
	  ;; is it player-2's turn?
	  (if (= *player-2* (state-turn state)) "<- Player 2's turn" "")
	  ;; opponent's mancala
	  (svref (state-board state) (mancala-pit *player-2*))
	  ;; blank stuff
	  (make-list *num-pits* :initial-element "   ")
	  ;; my mancala
	  (svref (state-board state) (mancala-pit *player-1*))
	  ;; difference in scores
	  (cond ((> (score state *player-1*) 0)
		 (format nil "   Player 1 ahead by ~a" (score state *player-1*)))
		((> (score state *player-2*) 0)
		 (format nil "   Player 2 ahead by ~a" (score state *player-2*)))
		(t "   Game tied"))
	  ;; my row
	  (coerce (subseq (state-board state) (left-pit *player-1*) (mancala-pit *player-1*)) 'list)
	  ;; is it player-1's turn?
	  (if (= *player-1* (state-turn state)) "<- Player 1's turn" "")
	  ;; player-1 moves
	  (let (bag)
	    (dotimes (x *num-pits* bag)
	      (push "---" bag)))
	  (let (bag)
	    (dotimes (x *num-pits* (reverse bag))
	      (push (+ x (left-pit *player-1*)) bag)))))

(defun valid-movep (state move)
  "move is a value from 0 to just under *num-pits*"
  (and (integerp move)
       (>= move 0)
       (< move *num-pits*)
       (/= 0 (svref (state-board state) move))))


(defun moves (state)
  "Returns, as a list, all the states reachable with a single
move from this state."
  (let (bag move)
    (dotimes (x *num-pits* bag)
      (setf move (state-from-move state x))
      (when move (push move bag)))))




;;; Functions for playing tournaments or games

(defun human-make-move (state)
  "Given a state, asks the user for a move and returns the
new state.  If there is no move to make (end of game),
returns nil."
  (let ((moves (moves state)) user-move)
    (when moves
      (format t "~%Enter a move: ")
      (loop
       (setf user-move (read))
       (if (valid-movep
	    state
	    (+ (left-pit (state-turn state)) user-move))
	   (return (state-from-move state user-move)))
       (format t "~%Invalid move.  Enter a move: ")))))


(defun play-game (max-depth computer-move-func &optional (human-goes-first t))
  "Plays a game with a human against a provided computer move
function searching up to MAX-DEPTH.  If HUMAN-GOES-FIRST is true,
then the human gets to go first"
  (let (new-state
	(state
	 (init-state
	   (if human-goes-first *player-1* *player-2*))))
    (loop
     (print-state state)
     (if (= (state-turn state) *player-1*)
	 (setf new-state (human-make-move state))
       (setf new-state (funcall computer-move-func state max-depth)))
     (if new-state
	 (setf state new-state)
       (return)))
    (cond ((> (score state *player-1*) 0)
	   (format t "~%~%Human wins by ~a" (score state *player-1*)))
	  ((> (score state *player-2*) 0)
	   (format t "~%~%Computer wins by ~a" (score state *player-2*)))
	  (t
	   (format t "~%~%It's a draw")))
    nil))


(defun tournament (depth1 depth2 func1 func2 &optional (print t))
  "Plays computer move func1 against computer move func2, providing
maximum depths for each function.  Optionally prints the board each
move.  Plays two games, one with player 1 first, and one with player
2 first.  Returns the difference in total scores: if positive, then
player 1 is better.  If negative, then player 2 is better."
  (let (new-state total
	(state
	 (init-state *player-1*)))
    (when print (format t "~%GAME 1"))
    (loop
     (when print (print-state state))
     (if (= (state-turn state) *player-1*)
	 (setf new-state (funcall func1 state depth1))
       (setf new-state (funcall func2 state depth2)))
     (if new-state
	 (setf state new-state)
       (return)))
    (setf total (score state *player-1*))

    (setf state (init-state *player-2*))
    (when print (format t "~%GAME 2"))
    (loop
     (when print (print-state state))
     (if (= (state-turn state) *player-1*)
	 (setf new-state (funcall func1 state depth1))
       (setf new-state (funcall func2 state depth2)))
     (if new-state
	 (setf state new-state)
       (return)))
    (+ total (score state *player-1*))))





;;;; Here is a description of the stuff that would go into your
;;;; file.

;;;; The first thing in your function is a package declaration.
;;;; You should name your package something beginning with a colon
;;;; followed by your full name with hyphens for spaces.
;;;; I named my package :sean-luke .  Keep in mind that the name
;;;; is CASE-INSENSITIVE.  The only symbol you should export is the
;;;; symbol COMPUTER-MAKE-MOVE, which should be the name of your top-
;;;; level computer move function.  Name your file the same
;;;; name as your package declaration, minus the colon.  For example,
;;;; my file is named "sean-luke.lisp"

;; (defpackage :sean-luke
;;   (:use :common-lisp-user :common-lisp)
;;   (:export computer-make-move))
;; (in-package :sean-luke)


;;;; Once you've done this, you need to write your code.  Here
;;;; is a rough sketch of three functions you will find handy.
;;;; You don't need to implement them like this, except for the
;;;; COMPUTER-MAKE-MOVE function. You can write your code in
;;;; any fashion you like in this file, so long as you do a
;;;; proper alpha-beta heuristic search and your evaluation
;;;; function is stronger than just comparing the differences in
;;;; the mancalas.

;; (defun alpha-beta (state current-depth max-depth
;; 			 max-player expand terminal evaluate
;; 			 alpha beta)
;;   "Does alpha-beta search.  Note that there is the addition of
;; a variable called MAX-PLAYER rather than a function which specifies
;; if it's max's turn.  It's just more convenient in this system.
;; The MAX-PLAYER variable is set to either *player-1*
;; or to *player-2* and indicates if *player-1* or *player-2* should
;; be considered 'max' (the other player is then considered to be
;; 'min')"
;; )

;; (defun evaluate (state max-player)
;;   "Evaluates the game situation for MAX-PLAYER.
;; Returns the value of STATE for MAX-PLAYER (who
;; is either *player-1* or *player-2*).  This should
;; be a value ranging from *min-wins* to *max-wins*."
;; )

;; (defun computer-make-move (state max-depth)
;;   "Given a state, makes a move and returns the new state.
;; If there is no move to make (end of game) returns nil.
;; Each time this function calls the top-level
;; alpha-beta function to search for the quality of a state,
;; computer-make-move should print out the state (using PRINT,
;; not PRINT-STATE) that is being searched.
;; Only search up to max-depth.  The computer should assume
;; that he is the player who's turn it is right now in STATE"
;; )

;;;; In comments your file, you put your project notes.

;;;; The last thing in your file should be this line (uncommented
;;;; of course).

;; (in-package :cl-user)
