;;;;; CS 480: ASSIGNMENT 4
;;;;; Written by: Zachary Ferguson (zfergus2)

; Write an information-theoretic CHOOSE-feature heuristic for a decision tree.
;
; Due THURSDAY, October 6, at MIDNIGHT.
;
; (A) I have provided you with the most of the basic code to do a decision
; tree, but the main building algorithm is missing and the CHOOSE-FEATURE
; function is stupid right now -- it just returns the first feature it sees,
; not the best one. Your job is to return the information-theoretic best
; feature.  Overall you need to implement the following FOUR functions below.
; I have provided you with example results from correct versions of the
; function to help you out.
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
; (NONE (:LABEL NIL)) (SOME (:LABEL NIL)) (FULL (PRICE (CHEAP (:LABEL T))
; (MEDIUM (:LABEL NIL)) (EXPENSIVE (:LABEL NIL)))))) (NIL (:LABEL NIL))))
; (NIL (:LABEL NIL)))) (NIL (OPEN-FRIDAY (T (HUNGRY (T (:LABEL T))
; (NIL (:LABEL NIL)))) (NIL (HUNGRY (T (PATRONS (NONE (:LABEL T))
; (SOME (:LABEL T)) (FULL (:LABEL NIL)))) (NIL (:LABEL T))))))))
; (NIL (HAS-BAR (T (OPEN-FRIDAY (T (:LABEL NIL)) (NIL (HUNGRY
; (T (:LABEL T)) (NIL (PATRONS (NONE (:LABEL NIL)) (SOME (:LABEL T))
; (FULL (:LABEL T)))))))) (NIL (OPEN-FRIDAY (T (:LABEL T))
; (NIL (HUNGRY (T (:LABEL T)) (NIL (:LABEL NIL)))))))))
;
; Beware that some Lisp systems
; will only print part of the tree unless you execute three SETF statements
; detailed further down in the code (see below).
;
; You might find the LOOP macro useful in implementing these functions
; because it has a special "SUM" keyword.  As usual, you can implement it
; in a smaller fashion without LOOP, but LOOP might feel more natural to
; you in this particular case.  At any rate, standard warnings apply
; regarding LOOP: you should make be careful that you know what you're doing
; if you decide to implement it that way.
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
; C) Modify at least one, if not all, of the previously provided classification
;    datasets so that they will work with this code.  You should be able to do
;    this fairly easily with a small function that maps over each dataset.
;    Demonstrate generalization using this additional dataset.
;    You'll probably have to go to the original dataset information to figure
;    out reasonable feature names.
;
;
; (D) [CS 580 ONLY -- CS 480 students may do this for extra credit]
;
; Extend this decision tree code in ONE of three ways:
;
; D1) Add the Gini index in addition to Information as an additional option
; for impurity.
;
; D2) Extend the decision tree to handle floating-point attributes as well as
;     categorical ones
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

;;;;;;;;;;;;;;;;;;;;;;;;
;; Contents:          ;;
;; Part A (308 - 453) ;;
;; Part B (538 - 555) ;;
;; Part C (558 - 854) ;;
;; Part D (361 - 372) ;;
;;;;;;;;;;;;;;;;;;;;;;;;


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


;; Additional function for build-decision-tree.
(defun examples-have-same-feature-vals (examples)
  "Returns t if all examples have the same feature values, else returns nil"
  (let ((feature-vals (butlast (first examples))))
    (dolist (example examples t)
      (when (not (equalp feature-vals (butlast example)))
  (return nil)))))


;;;;  The following function may come in very handy to you.  Be glad you didn't
;;;;  have to write it!  :-)


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
    (- (apply #'+ ; Negative sum of P(i) lg(P(i))
      (mapcar #'(lambda (prob)
        (if (equalp prob 0.0) 0.0
          (* prob (log prob 2))))
      probabilities))))

;;; Example outputs:
;;; (information '(1/2 1/3 1/6))	->	1.4591479
;;; (information '(0.1 0.4 0.3 0.2))	->	1.8464395
;;; (information '(nil 0 0 0 0 0 0))	->	0
;;; (information '(0.5 0.5))		->	1.0
;;; (information '(1))			->	0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Part D ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add the Gini index in addition to Information as an additional option for ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; impurity. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gini-coeff (probabilities)
  "Given a list of probabilities, returns the gini coefficient."
  (- 1 (apply #'+
    (mapcar #'(lambda (prob) (* prob prob)) probabilities))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun remainder (feature-def examples labels &key (impurity #'information))
  "Returns the sum, over each value that the feature can take on,
    of the probability of an example having that value, times the
    information content of the probabilities of various labels
    for all examples with that value."
  (apply #'+
    (mapcar #'(lambda (value)
      (* (/ (num-examples-with-feature-val examples (first feature-def) value)
          (length examples))
        (funcall impurity
          (label-probabilities-for-feature-val examples (first feature-def)
            value labels)))) (rest feature-def))))

;;; Example outputs:
;;; ;; (remainder of HUNGRY)
;;; (remainder (fourth *restaurant-feature-defs*) *restaurant-examples* *restaurant-labels*)
;;; -> 0.80429035
;;;
;;; ;; (remainder of PATRONS)
;;; (remainder (fifth *restaurant-feature-defs*) *restaurant-examples* *restaurant-labels*)
;;; -> 0.45914793


(defun choose-feature (feature-defs examples labels)
  "Returns the feature-def chosen from feature-defs
    that best divides up examples. This is done by
    picking the feature-def whose feature has
    the lowest remainder given the examples and labels.  Ties
    for best are broken by picking the one considered earliest."
  ; Sort the feature-defs by their remainder, and return the first feature-def
  (first (sort (copy-list feature-defs) #'(lambda (fi fj)
    (< (remainder fi examples labels) (remainder fj examples labels))))))

;;; Example outputs (for the "fixed" version, not the stupid version):
;;; ;; the chosen first feature given the full restaurant examples:
;;; (choose-feature *restaurant-feature-defs* *restaurant-examples* *restaurant-labels*)
;;; -> (PATRONS NONE SOME FULL)
;;;
;;; ;; the chosen first feature given just the last five restaurants
;;; (choose-feature *restaurant-feature-defs* (last *restaurant-examples* 5) *restaurant-labels*)
;;; -> (ESTIMATED-WAIT-TIME |0-10| |10-30| |30-60| >60)


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

    ; if out of examples return the default label
    (if (null examples) (list :label default)
      ; if all examples have the same label return that label
      (if (examples-have-same-label-p examples)
        (list :label (label-of-example (first examples)))
        ; if all examples have the same feature values return most common label
        (if (or (null feature-defs) (examples-have-same-feature-vals examples))
          (list :label (most-common-label examples))
          ; otherwise build a nonleaf-node for the choosen feature
          (let ((feature-def (choose-feature feature-defs examples labels)))
            (append (list (first feature-def)) (mapcar #'(lambda (val)
              (list val (build-decision-tree
                (examples-with-feature-val examples (first feature-def) val)
                (remove-if #'(lambda (f) (equalp f feature-def)) feature-defs)
                labels (most-common-label examples))))
              (rest feature-def)))))))))




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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PART B ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (setf *t* (build-decision-tree *restaurant-trial-examples*
;   *restaurant-feature-defs* *restaurant-labels*))
; Output:
;   (PATRONS (NONE (:LABEL NIL)) (SOME (:LABEL T))
;     (FULL (HAS-BAR (T (:LABEL T)) (NIL (:LABEL NIL)))))
;
; (mapcar #'label-of-example *restaurant-examples*)
; Output:
;   (T NIL T T   NIL T NIL T NIL NIL NIL T)
;
; (mapcar #'(lambda (x) (find-label-for-example x *t*)) *restaurant-examples*)
; Output:
;   (T NIL T NIL NIL T NIL T T   T   NIL T)
;
; Results: The labels differ in 3 places.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PART C ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Teaching Assistant Data ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-dataset (examples feature-defs)
  (mapcar #'(lambda (example)
    (append
      (mapcar #'(lambda (feature-def feature-val)
        (list (first feature-def) feature-val))
        feature-defs (first example))
      (list (list :label (first (second example))))))
    examples))


(defparameter *tae-labels*
  '(nil 2 3)
  "Label values for tae problem.")


(defparameter *tae-feature-defs*
  '((native-english-speaker 1 2)
    (course-instructor 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)
    (course 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26)
    (summer-or-regular 1 2))
  "A list of feature-defs for the tae problem.")

(defparameter *tae-examples*
  '(((2 7 11 2 13) (2))
    ((2 5 2 2 48) (1))
    ((1 13 1 2 54) (3))
    ((2 9 5 2 19) (3))
    ((2 20 2 2 45) (3))
    ((2 7 11 2 30) (1))
    ((2 13 3 1 10) (2))
    ((1 23 3 2 38) (3))
    ((1 13 3 1 17) (3))
    ((2 22 3 2 46) (2))
    ((2 25 7 2 27) (2))
    ((2 6 17 2 42) (2))
    ((2 16 8 2 36) (1))
    ((2 6 17 2 39) (3))
    ((2 19 4 2 10) (1))
    ((2 9 5 2 19) (3))
    ((2 13 1 2 29) (1))
    ((1 17 18 2 44) (2))
    ((1 17 17 2 19) (3))
    ((2 10 3 2 19) (1))
    ((2 22 1 2 11) (2))
    ((1 18 7 2 48) (1))
    ((1 22 13 2 27) (2))
    ((2 10 3 2 27) (3))
    ((2 2 10 2 27) (1))
    ((2 23 3 2 24) (1))
    ((2 10 3 2 19) (1))
    ((2 14 15 2 38) (2))
    ((2 18 21 2 29) (3))
    ((2 13 1 2 31) (1))
    ((1 8 3 2 29) (3))
    ((2 25 7 2 23) (2))
    ((1 13 3 1 13) (1))
    ((2 25 7 2 23) (2))
    ((2 8 3 2 24) (2))
    ((2 9 2 2 39) (3))
    ((2 13 1 2 20) (2))
    ((2 6 17 2 37) (2))
    ((2 15 3 1 17) (3))
    ((2 23 3 1 20) (3))
    ((2 11 1 2 51) (1))
    ((1 22 3 2 15) (2))
    ((1 23 3 2 49) (3))
    ((2 23 3 2 10) (2))
    ((2 23 3 2 12) (1))
    ((2 15 1 2 66) (1))
    ((2 17 18 2 29) (1))
    ((1 6 17 2 35) (2))
    ((2 9 2 2 14) (2))
    ((2 15 13 2 37) (2))
    ((1 17 17 2 31) (3))
    ((2 6 17 2 43) (2))
    ((2 7 11 2 10) (2))
    ((2 10 22 2 9) (3))
    ((2 22 1 2 51) (1))
    ((2 7 11 1 20) (3))
    ((2 21 2 2 42) (1))
    ((1 23 3 1 20) (3))
    ((2 20 2 2 25) (2))
    ((2 9 24 2 20) (2))
    ((1 11 16 2 22) (2))
    ((2 9 5 2 24) (3))
    ((2 23 3 1 20) (3))
    ((2 24 26 2 21) (2))
    ((1 23 3 1 19) (3))
    ((2 5 2 2 48) (1))
    ((2 15 13 2 37) (2))
    ((2 8 3 2 24) (2))
    ((2 22 3 2 28) (1))
    ((2 18 21 2 19) (3))
    ((2 1 15 1 22) (2))
    ((1 5 2 2 33) (3))
    ((2 16 19 2 11) (3))
    ((2 9 6 2 7) (3))
    ((1 23 3 1 19) (3))
    ((2 16 20 2 15) (1))
    ((2 6 17 2 42) (2))
    ((1 10 3 2 21) (3))
    ((1 22 3 2 58) (3))
    ((2 13 3 1 11) (3))
    ((2 3 2 2 26) (1))
    ((1 22 3 1 58) (3))
    ((1 23 3 2 38) (1))
    ((2 18 21 2 29) (3))
    ((2 13 1 2 31) (1))
    ((2 14 23 2 17) (2))
    ((2 14 15 2 38) (2))
    ((1 14 15 2 32) (1))
    ((2 10 22 2 9) (3))
    ((2 8 7 2 23) (1))
    ((2 16 8 2 36) (1))
    ((2 7 25 2 42) (2))
    ((2 21 2 2 42) (1))
    ((2 18 5 2 19) (1))
    ((2 13 14 2 17) (3))
    ((2 18 5 2 19) (1))
    ((2 7 11 2 10) (2))
    ((2 1 15 2 31) (3))
    ((2 7 11 2 55) (3))
    ((2 25 7 2 27) (2))
    ((2 23 3 2 11) (1))
    ((2 7 11 2 13) (2))
    ((2 20 2 2 3) (1))
    ((2 11 1 2 51) (1))
    ((2 2 9 2 31) (2))
    ((2 15 1 2 19) (1))
    ((2 20 2 2 14) (1))
    ((2 7 25 2 42) (2))
    ((2 5 2 2 37) (1))
    ((2 6 17 2 43) (2))
    ((2 23 3 1 20) (3))
    ((2 22 3 2 28) (1))
    ((2 14 15 2 38) (1))
    ((1 23 3 1 20) (2))
    ((2 6 17 2 39) (3))
    ((2 7 11 2 55) (3))
    ((2 13 3 1 10) (2))
    ((2 13 1 2 30) (3))
    ((2 1 15 1 22) (2))
    ((2 15 3 1 17) (3))
    ((2 20 15 2 18) (2))
    ((2 13 1 2 30) (3))
    ((2 22 3 2 46) (2))
    ((2 15 3 1 20) (3))
    ((1 23 3 1 25) (3))
    ((2 2 9 2 29) (1))
    ((2 7 11 2 30) (1))
    ((2 12 7 2 34) (1))
    ((1 23 3 2 49) (3))
    ((2 18 12 2 16) (2))
    ((2 18 25 2 25) (3))
    ((2 23 3 1 20) (2))
    ((2 10 3 2 12) (1))
    ((1 5 2 2 33) (3))
    ((2 10 3 2 27) (3))
    ((2 4 16 2 21) (1))
    ((2 9 6 2 5) (2))
    ((2 12 8 2 24) (2))
    ((1 22 3 2 45) (3))
    ((2 1 8 2 18) (2))
    ((2 14 22 2 17) (3))
    ((2 14 15 2 36) (3))
    ((2 15 3 1 20) (3))
    ((2 14 15 2 38) (1))
    ((2 5 2 2 37) (1))
    ((2 2 9 2 31) (2))
    ((2 22 1 2 42) (2))
    ((1 13 3 1 13) (1))
    ((2 4 16 2 21) (1))
    ((2 3 2 2 37) (1))
    ((2 8 3 2 27) (1))))

(setf *tae-examples* (convert-dataset *tae-examples* *tae-feature-defs*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Ballons ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; https://archive.ics.uci.edu/ml/datasets/Balloons ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *ballon-labels* '(t nil)
  "Are the ballons inflated?")

(defparameter *ballon-feature-defs*
  '((color yellow purple)
    (size small large)
    (act stretch dip)
    (age child adult))
    "Features of the ballons.")

(defparameter *ballon-examples-1*
  '(((yellow small stretch adult) (t))
    ((yellow small stretch adult) (t))
    ((yellow small stretch child) (nil))
    ((yellow small dip adult) (nil))
    ((yellow small dip child) (nil))
    ((yellow large stretch adult) (t))
    ((yellow large stretch adult) (t))
    ((yellow large stretch child) (nil))
    ((yellow large dip adult) (nil))
    ((yellow large dip child) (nil))
    ((purple small stretch adult) (t))
    ((purple small stretch adult) (t))
    ((purple small stretch child) (nil))
    ((purple small dip adult) (nil))
    ((purple small dip child) (nil))
    ((purple large stretch adult) (t))
    ((purple large stretch adult) (t))
    ((purple large stretch child) (nil))
    ((purple large dip adult) (nil))
    ((purple large dip child) (nil)))
  "adult-stretch.data inflated is true if age=adult and act=stretch")

(defparameter *ballon-examples-2*
  '(((yellow small stretch adult) (t))
    ((yellow small stretch child) (t))
    ((yellow small dip adult) (t))
    ((yellow small dip child) (nil))
    ((yellow small dip child) (nil))
    ((yellow large stretch adult) (t))
    ((yellow large stretch child) (t))
    ((yellow large dip adult) (t))
    ((yellow large dip child) (nil))
    ((yellow large dip child) (nil))
    ((purple small stretch adult) (t))
    ((purple small stretch child) (t))
    ((purple small dip adult) (t))
    ((purple small dip child) (nil))
    ((purple small dip child) (nil))
    ((purple large stretch adult) (t))
    ((purple large stretch child) (t))
    ((purple large dip adult) (t))
    ((purple large dip child) (nil))
    ((purple large dip child) (nil)))
  "adult+stretch.data inflated is true if age=adult or act=stretch")

(defparameter *ballon-examples-3*
  '(((yellow small stretch adult) (t))
    ((yellow small stretch child) (t))
    ((yellow small dip adult) (t))
    ((yellow small dip child) (t))
    ((yellow large stretch adult) (t))
    ((yellow large stretch child) (nil))
    ((yellow large dip adult) (nil))
    ((yellow large dip child) (nil))
    ((purple small stretch adult) (t))
    ((purple small stretch child) (nil))
    ((purple small dip adult) (nil))
    ((purple small dip child) (nil))
    ((purple large stretch adult) (t))
    ((purple large stretch child) (nil))
    ((purple large dip adult) (nil))
    ((purple large dip child) (nil)))
  "small-yellow+adult-stretch.data inflated is true if (color=yellow and size =
       small) or (age=adult and act=stretch)")

(defparameter *ballon-examples-4*
  '(((yellow small stretch adult) (t))
    ((yellow small stretch child) (t))
    ((yellow small dip adult) (t))
    ((yellow small dip child) (t))
    ((yellow small stretch adult) (t))
    ((yellow small stretch child) (t))
    ((yellow small dip adult) (t))
    ((yellow small dip child) (t))
    ((yellow large stretch adult) (nil))
    ((yellow large stretch child) (nil))
    ((yellow large dip adult) (nil))
    ((yellow large dip child) (nil))
    ((purple small stretch adult) (nil))
    ((purple small stretch child) (nil))
    ((purple small dip adult) (nil))
    ((purple small dip child) (nil))
    ((purple large stretch adult) (nil))
    ((purple large stretch child) (nil))
    ((purple large dip adult) (nil))
    ((purple large dip child) (nil)))
  "small-yellow.data inflated is true if (color=yellow and size = small)")

(setf *ballon-examples-1*
  (convert-dataset *ballon-examples-1* *ballon-feature-defs*))
(setf *ballon-examples-2*
  (convert-dataset *ballon-examples-2* *ballon-feature-defs*))
(setf *ballon-examples-3*
  (convert-dataset *ballon-examples-3* *ballon-feature-defs*))
(setf *ballon-examples-4*
  (convert-dataset *ballon-examples-4* *ballon-feature-defs*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
