;;;;;; Assignment 4

; Write an information-theoretic CHOOSE-feature heuristic
; for a decision tree.
;
; Due THURSDAY, November 6, at MIDNIGHT.
;
; (A) I have provided you with the most of the basic code to do a decision tree,
; but the main building algorithm is missing and the CHOOSE-FEATURE function is
; stupid right now -- it just returns the first feature it sees, not the best one.
; Your job is to return the information-theoretic best feature.  Overall you need to
; implement the following FOUR functions below.  I have provided you with example
; results from correct versions of the function to help you out.
;
; INFORMATION
; REMAINDER
; CHOOSE-FEATURE
; BUILD-DECISION-TREE
;
; These functions roughly correspond to the functions in your lecture notes.
; You should implement these functions permitting ANY NUMBER of label values,
; as described in the label notes (that is, not just "true" and "false", per
; the recycling bin example).
;
; If your functions are correct, then the following function call:
;
; (build-decision-tree *restaurant-examples*
; 		     *restaurant-feature-defs* *restaurant-labels*)
;
; ...should output a small decision tree.
;
; I would start on BUILD-DECISION-TREE first.  Once it's running properly
; you should be able go generate terrible decision trees.  For example,
; using the default "stupid" CHOOSE-FEATURE function I've provided, my version
; of BUILD-DECISION-TREE will create the following humongous tree
; on the restaurant example:
;
; (ALTERNATIVE (T (HAS-BAR (T (OPEN-FRIDAY (T (HUNGRY (T (PATRONS
; (NONE (:label NIL)) (SOME (:label NIL)) (FULL (PRICE (CHEAP (:label T)) 													     (MEDIUM (:label NIL)) (EXPENSIVE (:label NIL)))))) (NIL (:label NIL))))
; (NIL (:label NIL)))) (NIL (OPEN-FRIDAY (T (HUNGRY (T (:label T))
; (NIL (:label NIL)))) (NIL (HUNGRY (T (PATRONS (NONE (:label T))
; (NIL (HAS-BAR (T (OPEN-FRIDAY (T (:label NIL)) (NIL (HUNGRY (T (:label T))
; (NIL (PATRONS (NONE (:label NIL)) (SOME (:label T)) (FULL (:label T))))))))
; (NIL (OPEN-FRIDAY (T (:label T)) (NIL (HUNGRY (T (:label T)) (NIL (:label NIL)))))))))
;
; Beware that some Lisp systems
; will only print part of the tree unless you execute three SETF statements
; detailed further down in the code (see below).
;
; You might find the LOOP macro useful in implementing these functions
; because it has a special "SUM" keyword.  As usual, you can implement it
; in a smaller fashion without LOOP, but LOOP might feel more natural to
; you in this particular case.  At any rate, standard warnings apply
; regarding LOOP: you should make be careful that
; you know what you're doing if you decide to implement it that way.
;
;
; (B) Once you have implemented the CHOOSE-FEATURE function, let's see how
; well it generalizes.  Rather than learn a decision tree on the whole set
; of examples, let's see how well it works on just a training set.  I selected
; a subset of the whole examples and put them in a variable called
; *restaurant-trial-examples*.  Execute the following code:
;
; ;;; create the decision tree on the training set
; (setf *t* (build-decision-tree *restaurant-trial-examples*
; 			       *restaurant-feature-defs* *restaurant-labels*))
;
; ;;; print out the "correct" labels for all restaurants
; ;;; (the training and the testing set)
; (mapcar #'label-of-example *restaurant-examples*)
;
; ;;; print out what the decision tree says are the correct labels
; ;;; -- does it get everyone right, or just the ones that were previously
; ;;; in the training set?
; (mapcar #'(lambda (x) (find-label-for-example x *t*)) *restaurant-examples*)
;
; ... how many labels did the decision tree get wrong?
;
; I have given you another example as well, the "department" example.
;
;
; C) Modify at least one, if not all, of the previously provided classification datasets
;     so that they will work with this code.  You should be able to do this fairly easily
;     with a small function that maps over each dataset.  Demonstrate generalization
;     using this additional dataset.  You'll probably have to go to the original dataset
;     information to figure out reasonable feature names.
;
;
; (D) [CS 580 ONLY -- CS 480 students may do this for extra credit]
;
; Extend this decision tree code in ONE of three ways:
;
; D1) Add the Gini index in addition to Information as an additional option
; for impurity.
;
; D2) Extend the decision tree to handle floating-point attributes as well as categorical ones
;
; D3) Add K-Fold Validation and demonstrate it
;
; D4) Add a pruning method (PEP is the easiest)
;
;
; You will mail as an *attachment* to the TA a modified version of THIS FILE, with the
; REMAINDER, GAIN, and CHOOSE-FEATURE functions implemented and
; well commented, plus any supporting functions you wrote to make them possible,
; The function you used to modify an existing dataset, and the modified  dataset as
; a global variable.   Basically it should be a fully-functioning piece of code.  Don't
; deviate too much from the provided template -- it makes grading very difficult.
;
; In your email message you should also include:
;
; 1. The number of labels the decision tree got wrong in the generalization step (B).
;
; 2. Your 500-word report.
;
;
; NOTE ON PRINTING
;
; Some Lisp systems get lazy and print out long lists with shorthand
; forms like:
;
; (foo (bar # #) # (baz #))
;
; ...the #'s mean "there's more stuff here but it's too long for me to bother
; printing out".  You can force Lisp to print out more stuff than this, usually
; by doing something like:
;
; (setf *print-level* nil)
; (setf *print-length* nil)
; (setf *print-lines* nil)






;;;;;;  Some global parameters which will force Lisp to print out an ENTIRE
;;;;;;  decision tree rather than just print out a little piece of it....

(setf *print-level* nil)
(setf *print-length* nil)
(setf *print-lines* nil)



;;;;;;  Some useful Lisp functions that aren't decision-tree specific.

;; this is similar to the one I gave you earlier in K-Nearest Neighbor
(defun most-common (elts &key (test #'equalp))
  "Returns the elt in the list elts that appears the most often;
     ties are broken arbitrarily, probably by picking the elt
     which appeared earliest. Two elts are considered to be
     the same if they pass the :test (by default, equalp)"
  (let* ((unique-elts (remove-duplicates elts :test test))
	 (unique-nums (mapcar #'(lambda (x) (count x elts :test test)) unique-elts))
	 (best-elt (first unique-elts))
	 (best-num (first unique-nums)))
    (mapc #'(lambda (elt num) (when (> num best-num)
				(setf best-elt elt) (setf best-num num)))
	  (rest unique-elts) (rest unique-nums))
    best-elt))

(defun normalize (numbers)
  "Normalizes a list of numbers by dividing them by their sum.  If
    the numbers are all 0.0, normalize just returns the original numbers."
  (let ((sum (apply #'+ numbers)))
    (if (= sum 0.0) numbers
      (mapcar #'(lambda (num) (/ num sum)) numbers))))




;;;;;; DEFINITIONS OF DECISION TREE STRUCTURES.
;;;
;;; To get a decision tree, you run the function build-decision-tree,
;;; feeding it a list of EXAMPLES, a list of feature-DEFS,
;;; and a list of labels.
;;;
;;; A single feature-DEF is a list of an feature followed
;;; by valid VALUES the feature can have:
;;;
;;;    (atr val1 val2 val3 ...)
;;;
;;; For example, here are some restaurant feature-defs:
;;;
;;;    (price cheap medium expensive)
;;;    (open-friday t nil)
;;;
;;; A label is a symbol representing a value a label could
;;; take on.  For example, in the restaurant problem, the
;;; ENTIRE list of labels is simply '(t nil).
;;;
;;; A single EXAMPLE looks like this:
;;;
;;;    ((atr1 val) (atr2 val) ... (:label label))
;;;
;;; For example, here is the first restaurant example:
;;;
;;;    ((alternative t) (has-bar nil) (open-friday nil) (hungry t)
;;;     (patrons some) (price expensive) (raining nil) (reservation t)
;;;     (type french) (estimated-wait-time 0-10) (:label t))
;;;
;;; The decision tree that you get back consists of leaf nodes
;;; and nonleaf nodes which constitute subtrees.   A leaf node
;;; in the tree looks like this:
;;;
;;;    (:label label)
;;;
;;; For example:       (:label t)   ;; the label is "t"
;;;
;;; A nonleaf node consists of an feature question to ask,
;;; followed by lists of the form (val <subtree>) where val
;;; is a posible value (answer) to the feature question.
;;; Thus it a nonleaf node looks like this:
;;;
;;;    (attr (val1 subtree1) (val2 subtree2) ... )
;;;
;;;  For example, a node asking "hungry?" might look like this:
;;;
;;;    (hungry (t ...subtree... ) (nil ...subtree... ))
;;;
;;; ...where the subtrees ask further questions or specify a label.
;;;
;;; You can feed a decision tree and an example into
;;; FIND-LABEL-FOR-EXAMPLE to run the decision tree on the
;;; example and see what label it decides on.
;;;
;;; What follows are some useful functions on examples and feature-defs
;;; which you may find valuable.  I think that of particular use to you may be
;;; NUM-EXAMPLES-WITH-FEATURE-VAL.
;;;
;;;
;;; IMPORTANT NOTE ABOUT THINGS THAT BEGIN WITH A COLON
;;; Stuff like :label or :foo or :bar are "keyword symbols" in Lisp.
;;; They do NOT need to be quoted, and they evaluate to themselves.
;;; Thus :label evaluates to :label .  If you're interested in this
;;; subject (it's not necessary for the project), the Lisp text talks
;;; a little bit about keyword symbols in section 8.7.
;;;



(defun feature-val-for-example (example feature)
  "Returns the value of a given feature as stored in an example"
  (second (assoc feature example)))


(defun label-of-example (example)
  "Returns the label of an example, as stored in the example"
  (second (assoc :label example)))


(defun find-label-for-example (example decision-tree)
  "Given a decision tree and an example, uses the decision tree
    to determine the expected label for the example"
  (if (equalp (first decision-tree) :label)
      (second decision-tree) ;; return the label
    (find-label-for-example example
			    (second (assoc
				     (feature-val-for-example example (first decision-tree))
				     (rest decision-tree))))))



(defun examples-have-same-label-p (examples)
  "Returns t if all examples have the same label, else returns nil"
  (let ((label (label-of-example (first examples))))
    (dolist (example examples t)
      (when (not (equalp label (label-of-example example)))
	(return nil)))))



(defun num-examples-with-label (examples label)
  "Returns the number of examples with a given label"
  (count-if #'(lambda (example) (equalp
				 (label-of-example example) label)) examples))



(defun num-examples-with-feature-val (examples feature value)
  "Returns the number of examples with a given value for some feature"
  (count-if #'(lambda (example) (equalp
				 (feature-val-for-example example feature) value)) examples))



(defun most-common-label (examples)
  "Returns the most common label found in examples"
  (most-common (mapcar #'label-of-example examples)))



(defun examples-with-feature-val (examples feature val)
  "Returns the examples whose feature has value val"
  (remove-if-not #'(lambda (x) (equalp (feature-val-for-example x feature) val)) examples))



(let ((unique (gensym)))
  (defun build-decision-tree (examples feature-defs labels &optional (default unique))
    "Given a set of examples, a set of feature-defs, and a
    set of label values, returns a decision tree which correctly
    labelifies the examples.  You can then run this decision tree
    on examples to see how it does by passing it to the function
    FIND-LABEL-FOR-EXAMPLE."

    ;; if you look closely, you'll see that this algorithm is
    ;; the same as the one in the notes, except that
    ;; you also have to also pass in a list of valid labels, to
    ;; make things a little simpler later on...

    ;; start with this little chunk of code which sets the default when the
    ;; user didn't supply one (when you call this function recursively you
    ;; should always provide a default)

    (if (equalp default unique)
      (setf default (most-common-label examples)))

    ;; okay, now your code goes here....

 ))    ;; note two parentheses (matching the LET)


;;;;;;;  The following function may come in very handy to you.  Be glad you didn't
;;;;;;;  have to write it!  :-)


(defun label-probabilities-for-feature-val (examples feature value labels)
  "Returns a list, one item per label,
    of the percentage of examples with a given value for a given feature,
    which belong to that label.  If they're all 0.0, you'll get back a list
    of 0.0's, even though that doesn't add to 1.0 (which should be just fine).
    If feature is nil, then this simply returns a list, one item per label,
    of the percentage of examples which belong to that label."

  (let ((examples-with-feature-val
	 (if (null feature) examples
	   (remove-if-not #'(lambda (x) (equalp
					 (feature-val-for-example x feature) value)) examples))))
    (normalize
     (mapcar #'(lambda (label) (/
				(num-examples-with-label examples-with-feature-val label)
				(length labels)))
	     labels))))



(defun information (probabilities)
  "Given a list of probabilities, returns the information
    theoretically stored in that list"

   ;;; Important note: LOG_2(0.0) is undefined.  You should
    ;;; assume that a 0.0 probability contributes 0.0 to the sum,
    ;;; and a non-zero probability P contributes  - P Log_2(P)
    ;;; to the sum.

  )

;;; Example outputs:
;;; (information '(1/2 1/3 1/6))	->	1.4591479
;;; (information '(0.1 0.4 0.3 0.2))	->	1.8464395
;;; (information '(1 0 0 0 0 0 0))	->	0
;;; (information '(0.5 0.5))		->	1.0
;;; (information '(1))			->	0



(defun remainder (feature-def examples labels)
  "Returns the sum, over each value that the feature can take on,
    of the probability of an example having that value, times the
    information content of the probabilities of various labels
    for all examples with that value."

  )

;;; Example outputs:
;;;	;; (remainder of HUNGRY)
;;; (remainder (fourth *restaurant-feature-defs*) *restaurant-examples* *restaurant-labels*)
;;; -> 0.80429035
;;;
;;;	;; (remainder of PATRONS)
;;; (remainder (fifth *restaurant-feature-defs*) *restaurant-examples* *restaurant-labels*)
;;; -> 0.45914793


(defun choose-feature (feature-defs examples labels)
  "Returns the feature-def chosen from feature-defs
    that best divides up examples. This is done by
    picking the feature-def whose feature has
    the lowest remainder given the examples and labels.  Ties
    for best are broken by picking the one considered earliest."

  ;; our default form always picks the first feature-def.  Pretty dumb, huh?
  ;; ...perhaps you might want to modify it.  :-)

  (first feature-defs))

;;; Example outputs (for the "fixed" version, not the stupid version):
;;;	;; the chosen first feature given the full restaurant examples:
;;; (choose-feature *restaurant-feature-defs* *restaurant-examples* *restaurant-labels*)
;;; -> (PATRONS NONE SOME FULL)
;;;
;;;	;; the chosen first feature given just the last five restaurants
;;; (choose-feature *restaurant-feature-defs* (last *restaurant-examples* 5) *restaurant-labels*)
;;; -> (ESTIMATED-WAIT-TIME |0-10| |10-30| |30-60| >60)










;;;;;; PRACTICE EXAMPLES

;;; The Department Problem

(defparameter *department-labels*
  '(t nil)
  "Label Values for the Departmental Reycling Bin Problem")

(defparameter *department-feature-defs*
  '((status faculty staff student)
					  (department ee cs)
					  (floor 3 4 5)
					  (size small medium large))
  "A list of feature-defs for the Departmental Recycling Bin Problem.
    Feature defs take the form (feature val1 val2 val3 ...)" )


(defparameter *department-examples*
'(((status faculty) (floor 3) (department ee) (size large) (:label nil))
				      ((status staff) (floor 3) (department ee) (size small) (:label nil))
				      ((status faculty) (floor 4) (department cs) (size medium) (:label t))
				      ((status student) (floor 4) (department ee) (size large) (:label t))
				      ((status staff) (floor 5) (department cs) (size medium) (:label nil))
				      ((status faculty) (floor 5) (department cs) (size large) (:label t))
				      ((status student) (floor 3) (department ee) (size small) (:label t))
				      ((status staff) (floor 4) (department cs) (size medium) (:label nil)))
  "A list of feature-defs for the Departmental Recycling Bin Problem.
    Examples take the form of ((attr1 val) (attr2 val) ... (:label label))")





;;; The Restaurant Problem

(defparameter *restaurant-labels*
  '(t nil)
  "label Values for the Restaurant problem")

(defparameter *restaurant-feature-defs*
  '((alternative t nil)
    (has-bar t nil)
    (open-friday t nil)
    (hungry t nil)
    (patrons none some full)
    (price cheap medium expensive)
    (raining t nil)
    (reservation t nil)
    (type french thai burger italian)
    (estimated-wait-time 0-10 10-30 30-60 >60))
  "A list of feature-defs for the Restaurant problem.
    feature defs take the form (feature val1 val2 val3 ...)")


(defparameter *restaurant-examples*
  '(((alternative t) (has-bar nil) (open-friday nil) (hungry t) (patrons some) (price expensive) (raining nil) (reservation t) (type french) (estimated-wait-time 0-10) (:label t))
    ((alternative t) (has-bar nil) (open-friday nil) (hungry t) (patrons full) (price cheap) (raining nil) (reservation nil) (type thai) (estimated-wait-time 30-60) (:label nil))
    ((alternative nil) (has-bar t) (open-friday nil) (hungry nil) (patrons some) (price cheap) (raining nil) (reservation nil) (type burger) (estimated-wait-time 0-10) (:label t))
    ((alternative t) (has-bar nil) (open-friday t) (hungry t) (patrons full) (price cheap) (raining nil) (reservation nil) (type thai) (estimated-wait-time 10-30) (:label t))
    ((alternative t) (has-bar nil) (open-friday t) (hungry nil) (patrons full) (price expensive) (raining nil) (reservation t) (type french) (estimated-wait-time >60) (:label nil))
    ((alternative nil) (has-bar t) (open-friday nil) (hungry t) (patrons some) (price medium) (raining t) (reservation t) (type italian) (estimated-wait-time 0-10) (:label t))
    ((alternative nil) (has-bar t) (open-friday nil) (hungry nil) (patrons none) (price cheap) (raining t) (reservation nil) (type burger) (estimated-wait-time 0-10) (:label nil))
    ((alternative nil) (has-bar nil) (open-friday nil) (hungry t) (patrons some) (price medium) (raining t) (reservation t) (type thai) (estimated-wait-time 0-10) (:label t))
    ((alternative nil) (has-bar t) (open-friday t) (hungry nil) (patrons full) (price cheap) (raining t) (reservation nil) (type burger) (estimated-wait-time >60) (:label nil))
    ((alternative t) (has-bar t) (open-friday t) (hungry t) (patrons full) (price expensive) (raining nil) (reservation t) (type italian) (estimated-wait-time 10-30) (:label nil))
    ((alternative nil) (has-bar nil) (open-friday nil) (hungry nil) (patrons none) (price cheap) (raining nil) (reservation nil) (type thai) (estimated-wait-time 0-10) (:label nil))
    ((alternative t) (has-bar t) (open-friday t) (hungry t) (patrons full) (price cheap) (raining nil) (reservation nil) (type burger) (estimated-wait-time 30-60) (:label t)))
  "A list of examples for the Restaurant problem.
    Examples take the form of ((attr1 val) (attr2 val) ... (:label label))")


(defparameter *restaurant-trial-examples*
  '(((alternative t) (has-bar nil) (open-friday nil) (hungry t) (patrons full) (price cheap) (raining nil) (reservation nil) (type thai) (estimated-wait-time 30-60) (:label nil))
    ((alternative nil) (has-bar t) (open-friday nil) (hungry nil) (patrons some) (price cheap) (raining nil) (reservation nil) (type burger) (estimated-wait-time 0-10) (:label t))
    ((alternative t) (has-bar nil) (open-friday t) (hungry nil) (patrons full) (price expensive) (raining nil) (reservation t) (type french) (estimated-wait-time >60) (:label nil))
    ((alternative nil) (has-bar nil) (open-friday nil) (hungry t) (patrons some) (price medium) (raining t) (reservation t) (type thai) (estimated-wait-time 0-10) (:label t))
    ((alternative nil) (has-bar nil) (open-friday nil) (hungry nil) (patrons none) (price cheap) (raining nil) (reservation nil) (type thai) (estimated-wait-time 0-10) (:label nil))
    ((alternative t) (has-bar t) (open-friday t) (hungry t) (patrons full) (price cheap) (raining nil) (reservation nil) (type burger) (estimated-wait-time 30-60) (:label t)))
  "Some training-case examples for the Restaurant problem, selected from
    the full set of examples above.
    Examples take the form of ((attr1 val) (attr2 val) ... (:label label))")
