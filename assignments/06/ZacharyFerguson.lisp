;;; ASSIGNMENT 6
;;; A SIMPLE GENETIC ALGORITHM OPERATING OVER FLOATING-POINT VECTORS
;;; DUE: MIDNIGHT THE EVENING OF TUESDAY, NOVEMBER 8

;;; Please don't deviate too much from this file.  Rearranging
;;; things into multiple files etc. may be convenient for you but it's MUCH
;;; tougher to grade.


#|
In this project you will do three things:

1. Implement a very simple abstract high-level evolutionary computation framework

2. Implement the functions necessary for a floating-point genetic algorithm

3. Test it on various objective functions and find good settings of parameters
   which work well on those functions


WHAT YOU MUST PROVIDE:

1. Completed code file which works and compiles and is reasonably well documented.

2. IN COMMENTS at the end of the code file, a short report which discusses interesting
   things you discovered as you were implementing the code, and the results of
   an experiment where you attempted to find parameter settings which generally
   produced the best results for various test objective functions.  Keep in mind
   that this algorithm is STOCHASTIC, so sometimes it produces good results and
   sometimes it produces bad ones, based partly on randomness.  So don't rely on
   a single run to determine how good some parameter settings are; perhaps you
   might run some N > 50 times and take the average to get an idea of how well they
   do compared to others.

If you like you can also include a PDF file with a more extensive version of
the report which has graphs and such, but it's not necessary.  Do NOT include
a Word document.

I have given you a bunch of objective test problems to try.  They are roughly in
increasing order of "challenge" to the genetic algorithm.  Some parameters you
can fool around with: the tournament size, the mutation rate, the crossover rate,
and the mutation variance.

I would fix the size of the individual, the number of individuals in the population,
and the number of generations to constants -- they're not part of the experiment
though you have to say what you set them to.  Suggested settings: an individual
of size 20, 50 individuals in the population, and oh, I dunno, how about 1000
generations.  You'll find that as the individual size gets bigger the problem
gets harder for the GA to solve.  Also as the generations get longer or the
population size gets bigger, the GA is being given more total resources to throw
at your problem so you'd expect better results.

I VERY STRONGLY URGE YOU TO COMPILE YOUR CODE AND NOT RUN IT INTERPRETED!  If you're
running SBCL, or CCL it automatically compiles the code.

SUBMISSION
Mail the TA your file, including the report.  And if you decided to do a
separate PDF report, mail that too.

|#


;;; Some utility Functions and Macros that you might find to be useful (hint)

(defmacro while (test &rest body)
  "Repeatedly executes body as long as test returns true.  Then returns nil."
  `(loop while ,test do (progn ,@body)))

;;; Example usage
;;;
;;; (let ((x 0))
;;;    (while (< x 5)
;;;        (print x)
;;;        (incf x)))


(defun random? (&optional (prob 0.5))
  "Tosses a coin of prob probability of coming up heads,
then returns t if it's heads, else nil."
  (< (random 1.0) prob))

(defun generate-list (num function &optional no-duplicates)
  "Generates a list of size NUM, with each element created by
  (funcall FUNCTION).  If no-duplicates is t, then no duplicates
are permitted (FUNCTION is repeatedly called until a unique
new slot is created).  EQUALP is the default test used for duplicates."
  (let (bag)
    (while (< (length bag) num)
      (let ((candidate (funcall function)))
  (unless (and no-duplicates
         (member candidate bag :test #'equalp))
    (push candidate bag))))
    bag))

;; hope this works right
(defun gaussian-random (mean variance)
  "Generates a random number under a gaussian distribution with the
given mean and variance (using the Box-Muller-Marsaglia method)"
  (let (x y (w 0))
    (while (not (and (< 0 w) (< w 1)))
     (setf x (- (random 2.0) 1.0))
     (setf y (- (random 2.0) 1.0))
     (setf w (+ (* x x) (* y y))))
    (+ mean (* x (sqrt variance) (sqrt (* -2 (/ (log w) w)))))))


;;;;;; TOP-LEVEL EVOLUTIONARY COMPUTATION FUNCTIONS


;;; TOURNAMENT SELECTION

;; is this a good setting?  Try tweaking it (any integer >= 2) and see
(defparameter *tournament-size* 7)
(defun tournament-select-one (population fitnesses)
  "Does one tournament selection and returns the selected individual."
  ;;; See Algorithm 32 of Essentials of Metaheuristics
  (elt population (elt ; Return highest fitness individual
    ; Sort a list of random indecies in fitnesses
    (sort (generate-list *tournament-size*
        #'(lambda () (random (length fitnesses))))
      ; Get the index of the fittest individual
      #'(lambda (i j) (> (elt fitnesses i) (elt fitnesses j)))) 0)))


(defun tournament-selector (num population fitnesses)
  "Does NUM tournament selections, and puts them all in a list, then returns
  the list"
  ;;; Hint: This is a very short function.  Maybe one of the
  ;;; Utility functions I provided might be of benefit here
  (generate-list num #'(lambda ()
    (tournament-select-one population fitnesses))))



;; I'm nice and am providing this for you.  :-)
(defun simple-printer (pop fitnesses)
  "Determines the individual in pop with the best (highest) fitness, then
prints that fitness and individual in a pleasing manner."
  (let (best-ind best-fit)
    (mapcar #'(lambda (ind fit)
    (when (or (not best-ind)
        (< best-fit fit))
      (setq best-ind ind)
      (setq best-fit fit))) pop fitnesses)
    (format t "~%Best Individual of Generation...~%Fitness: ~a~%Individual:~a~%"
      best-fit best-ind)
    fitnesses))



(defun evolve (generations pop-size
         &key setup creator selector modifier evaluator printer)
  "Evolves for some number of GENERATIONS, creating a population of size
POP-SIZE, using various functions"

  ;;; IMPLEMENT ME
  ;;;
  ;; The functions passed in are as follows:
  ;;(SETUP)                     called at the beginning of evolution, to set up
  ;;                            global variables as necessary
  ;;(CREATOR)                   creates a random individual
  ;;(SELECTOR num pop fitnesses) given a population and a list of corresponding
  ;;                            fitnesses, selects and returns NUM individuals
  ;;                            as a list. An individual may appear more than
  ;;                            once in the list.
  ;;(MODIFIER ind1 ind2)        modifies individuals ind1 and ind2 by crossing
  ;;                            them over and mutating them. Returns the two
  ;;                            children as a list: (child1 child2).
  ;;                            Nondestructive to ind1 and ind2.
  ;;(PRINTER pop fitnesses)     prints the best individual in the population,
  ;;                            plus its fitness, and any other interesting
  ;;                            statistics you think interesting for that
  ;;                            generation.
  ;;(EVALUATOR individual)      evaluates an individual, and returns its
  ;;                            fitness.
  ;;Pop will be guaranteed to be a multiple of 2 in size.
  ;;
  ;; HIGHER FITNESSES ARE BETTER

  ;; your function should call PRINTER each generation, and also print out the
  ;; best individual discovered over the whole run at the end, plus its fitness
  ;; and any other statistics you think might be nifty.

  ;;; HINTS: You could do this in many ways.  But I implemented it using
  ;;; the following functions (among others)
  ;;;
  ;;; FUNCALL FORMAT MAPCAR LAMBDA APPLY
  (funcall setup) ; Call the setup function
  (let ((pop (generate-list pop-size creator))) ; Create the initial pop.
    (dotimes (i generations pop) ; For n generations
      (let ((fitnesses (mapcar #'(lambda (individual)
          (funcall evaluator individual)) pop)))
        (funcall printer pop fitnesses) ; Print the current generation
        (let ((next-gen nil)) ; Create a new generation
          (dotimes (j (/ pop-size 2)) ; Pair up parents
            (setf next-gen (append next-gen (apply modifier ; Create two new children
              (funcall selector 2 pop fitnesses))))) ; Select two fit individuals
          (setf pop next-gen)))) ; Set population to the new generation
    ; Print the best individual at the end
    (funcall printer pop (mapcar #'(lambda (individual)
        (funcall evaluator individual)) pop))))


;;;;;; FLOATING-POINT VECTOR GENETIC ALGORTITHM


;;; Here you will implement creator, modifier, and setup functions for
;;; individuals in the form of lists of floating-point values.
;;; I have provided some objective functions which you can use as
;;; fitness evaluation functions.

;;; If you were really into this, you might try implementing an evolution
;;; strategy instead of a genetic algorithm and compare the two.
;;;
;;; If you were really REALLY into this, I have an extension of this
;;; project which also does genetic programming as well.  That is a much
;;; MUCH MUCH MUCH more difficult project.



(defparameter *float-vector-length* 20
  "The length of the vector individuals")
(defparameter *float-min* -5.12
  "The minimum legal value of a number in a vector")
(defparameter *float-max* 5.12
  "The maximum legal value of a number in a vector")

(defun float-vector-creator ()
  "Creates a floating-point-vector *float-vector-length* in size, filled with
UNIFORM random numbers in the range appropriate to the given problem"

  ;;; IMPLEMENT ME
  ;;;
  ;;; The numbers must be uniformly randomly chosen between *float-min* and
  ;;; *float-max*.  See the documentation for the RANDOM function.

  ;;; HINT: Maybe a function I provided in the utilities might
  ;;; be handy here
  (generate-list *float-vector-length* #'(lambda ()
    (+ (random (- *float-max* *float-max*)) *float-min*))))



;; I just made up these numbers, you'll probably need to tweak them
(defparameter *crossover-probability* 0.1
  "Per-gene probability of crossover in uniform crossover")
(defparameter *mutation-probability* 0.1
  "Per-gene probability of mutation in gaussian convolution")
(defparameter *mutation-variance* 0.02
  "Per-gene mutation variance in gaussian convolution")




;; to impement FLOAT-VECTOR-MODIFIER, the following two functions are
;; strongly reccommended.


(defun uniform-crossover (ind1 ind2)
  "Performs uniform crossover on the two individuals, modifying them in place.
*crossover-probability* is the probability that any given allele will crossover.
The individuals are guaranteed to be the same length.  Returns NIL."

  ;;; For crossover: use uniform crossover (Algorithm 25) in
  ;;;                Essentials of Metaheuristics
  ;;; HINTS:
  ;;; DOTIMES, ELT, and ROTATEF
  (dotimes (i (min (length ind1) (length ind2)) nil)
    (if (random? *crossover-probability*)
      (rotatef (elt ind1 i) (elt ind2 i)))))


(defun gaussian-convolution (ind)
  "Performs gaussian convolution mutation on the individual, modifying it in place.
 Returns NIL."

  ;;; For mutation, see gaussian convolution (Algorithm 11) in
  ;;;                Essentials of Metaheuristics
  ;;; Keep in mind the legal minimum and maximum values for numbers.
  ;;; HINTS:
  ;;; Maybe a function or three in the utility functions above might be handy
  ;;; See also SETF
  (dotimes (i (length ind) nil) ; For each gene in the individual
    (if (random? *mutation-probability*) ; If mutate gene i
      (let ((new-gene (+ (elt ind i) (gaussian-random 0.0 *mutation-variance*))))
        ; Generate new gene values until one falls with in range.
        (while (or (< new-gene *float-min*) (> new-gene *float-max*))
          (setf new-gene (+ (elt ind i) (gaussian-random 0.0 *mutation-variance*))))
        ; Update the ith gene in the individual
        (setf (elt ind i) new-gene)))))


(defun float-vector-modifier (ind1 ind2)
  "Copies and modifies ind1 and ind2 by crossing them over with a uniform
crossover, then mutates the children. *crossover-probability* is the
probability that any given allele will crossover.  *mutation-probability* is
the probability that any given allele in a child will mutate.  Mutation does
gaussian convolution on the allele."

  ;;; IMPLEMENT ME
  ;;; It's pretty straightforward.
  ;;; This function should first COPY the two individuals, then
  ;;; CROSS THEM OVER, then mutate the result using gaussian covolution,
  ;;; then return BOTH children together as a list (child1 child2)
  ;;;
  ;;; HINTS:
  ;;; For copying lists:  See the Lisp Cheat Sheet
  ;;;                (http://cs.gmu.edu/~sean/lisp/LispCheatSheet.txt)
  (let ((children (list (copy-list ind1) (copy-list ind2))))
    (apply #'uniform-crossover children) ; Cross-over the copied parents
    (mapcar #'gaussian-convolution children) ; Mutate the children
    children)) ; Return the children as a list


;; you probably don't need to implement anything here
(defun float-vector-sum-setup ()
  "Does nothing.  Perhaps you might use this function to set
(ahem) various global variables which define the problem being evaluated
and the floating-point ranges involved, etc.  I dunno."
  )




;;; FITNESS EVALUATION FUNCTIONS

;;; I'm providing you with some classic objective functions.  See section 11.2.2 of
;;; Essentials of Metaheuristics for details on these functions.
;;;
;;; Many of these functions (sphere, rosenbrock, rastrigin, schwefel) are
;;; traditionally minimized rather than maximized.  We're assuming that higher
;;; values are "fitter" in this class, so I have taken the liberty of converting
;;; all the minimization functions into maximization functions by negating their
;;; outputs.  This means that you'll see a lot of negative values and that's fine;
;;; just remember that higher is always better.
;;;
;;; These functions also traditionally operate with different bounds on the
;;; minimum and maximum values of the numbers in the individuals' vectors.
;;; Let's assume that for all of these functions, these values can legally
;;; range from -5.12 to 5.12 inclusive.  One function (schwefel) normally goes from
;;; about -511 to +512, so if you look at the code you can see I'm multiplying
;;; the values by 100 to properly scale it so it now uses -5.12 to 5.12.


(defun sum-f (ind)
  "Performs the Sum objective function.  Assumes that ind is a list of floats"
  (reduce #'+ ind))

(defun step-f (ind)
  "Performs the Step objective function.  Assumes that ind is a list of floats"
  (+ (* 6 (length ind))
     (reduce #'+ (mapcar #'floor ind))))

(defun sphere-f (ind)
  "Performs the Sphere objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x) (* x x)) ind))))

(defun rosenbrock-f (ind)
  "Performs the Rosenbrock objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x x1)
        ; (1-x)**2 + 100 * (x1 - x**2)**2
        (+ (* (- 1 x) (- 1 x))
            (* 100 (- x1 (* x x)) (- x1 (* x x)))))
       ind (rest ind)))))

(defun rastrigin-f (ind)
  "Performs the Rastrigin objective function.  Assumes that ind is a list of floats"
  (- (+ (* 10 (length ind))
  (reduce #'+ (mapcar (lambda (x) (- (* x x) (* 10 (cos (* 2 pi x)))))
          ind)))))

(defun schwefel-f (ind)
  "Performs the Schwefel objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x) (* (- x) (sin (sqrt (abs x)))))
       (mapcar (lambda (x) (* x 100)) ind)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test evaluation functions
(defun average (lis)
  "Computes the average over a list of numbers.  Returns 0 if the list length is 0."
  (if (= (length lis) 0)
    0
    (/ (reduce #'+ lis) (length lis))))


(defun avg-best-fitness (num-trials generations pop-size
         &key setup creator selector modifier evaluator printer)
  "Computes the average of the best fitnesses for num-trials."
  (average (generate-list num-trials #'(lambda ()
    (first (sort (mapcar evaluator (evolve generations pop-size
      :setup setup
      :creator creator
      :selector selector
      :modifier modifier
      :evaluator evaluator
      :printer printer)) #'>))))))

(defparameter *number-of-trials* 1 ; Change this to 1 to get default results
  "Number of trials to run for testing.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following are several test case used to test the above functons.
;;; an example way to fire up the GA.  If you've got it tuned right, it should quickly
;;; find individuals which are all very close to +5.12

#|
(format t "~%Average best fitness for 50 trials: ~a~%"
  (avg-best-fitness *number-of-trials* 1000 50
    :setup #'float-vector-sum-setup
    :creator #'float-vector-creator
    :selector #'tournament-selector
    :modifier #'float-vector-modifier
    :evaluator #'sum-f
    :printer #'simple-printer))
|#

;;; Results
; Best Individual of Generation...
; Fitness: 102.13245
; Individual:(5.1091156 5.1094213 5.0949063 5.1055074 5.1177473 5.109817 5.111027
;             5.1055226 5.0985565 5.1169515 5.1165757 5.1181273 5.1093388 5.10713
;             5.112763 5.1080003 5.1184735 5.093994 5.1037664 5.0656943)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following converges to individuals with genes > 5.

#|
(format t "~%Average best fitness for 50 trials: ~f~%"
  (avg-best-fitness *number-of-trials* 1000 50
    :setup #'float-vector-sum-setup
    :creator #'float-vector-creator
    :selector #'tournament-selector
    :modifier #'float-vector-modifier
    :evaluator #'step-f ; f(x) = floor(x)
    :printer #'simple-printer))
|#

;;; Results
; Best Individual of Generation...
; Fitness: 218
; Individual:(5.1180067 5.066333 4.1752305 5.0845027 4.025402 5.081081 5.1020937
;             5.1188984 5.0452952 5.0155277 5.1027217 5.0260115 5.0900407
;             5.027154 5.0309134 5.0491047 5.111242 5.092253 5.029934 5.019791)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following minimizes the sum of the squared values.
;;; The gene values converge to 0.

#|
(format t "~%Average best fitness for 50 trials: ~f~%"
  (avg-best-fitness *number-of-trials* 1000 50
    :setup #'(lambda ()
      (setf *tournament-size* 14))
    :creator #'float-vector-creator
    :selector #'tournament-selector
    :modifier #'float-vector-modifier
    :evaluator #'sphere-f
    :printer #'simple-printer))
|#

;;; Results
; Best Individual of Generation...
; Fitness: -0.0056634336
; Individual:(-0.0074968413 -0.004433047 0.022472257 -0.0044431463 0.04387998
;             0.023790058 -0.0016307216 -0.0064120255 0.027510725 -0.0030036885
;             -0.012258083 -0.011953682 -0.008142242 -0.013576329 -0.012332678
;             0.0042012483 -0.016095182 0.014820685 0.014096186 -0.019257754)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The gene values should all converge to 1.

#|
(format t "~%Average best fitness for 50 trials: ~f~%"
  (avg-best-fitness *number-of-trials* 1000 50
    :setup #'(lambda ()
      (setf *tournament-size* 14)
      (setf *crossover-probability* 0.01)
      (setf *mutation-probability* 0.01)
      (setf *mutation-variance* 0.01))
    :creator #'float-vector-creator
    :selector #'tournament-selector
    :modifier #'float-vector-modifier
    :evaluator #'rosenbrock-f
    :printer #'simple-printer))
|#

;;; Results
; Best Individual of Generation...
; Fitness: -15.988052
; Individual:(0.81734437 0.66739005 0.45090497 0.19887169 0.033109196
;             0.0136255175 -0.0026979893 0.0099992305 7.3359604e-4 0.0075760875
;             0.0026294664 0.024225838 0.020321865 0.002469265 0.0043759774
;             0.02122669 0.018006817 0.0015915819 0.015672602 0.0022177175)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(format t "~%Average best fitness for 50 trials: ~f~%"
  (avg-best-fitness *number-of-trials* 1000 50
    :setup #'float-vector-sum-setup
    :creator #'float-vector-creator
    :selector #'tournament-selector
    :modifier #'float-vector-modifier
    :evaluator #'rastrigin-f
    :printer #'simple-printer))
|#

;;; Results
; Best Individual of Generation...
; Fitness: -0.4650607946025218d0
; Individual:(-0.009429347 -0.006413374 0.022219859 0.0048547797 0.014526263
;            0.007250933 -0.006687667 0.0041171685 -0.012395553 -0.008388802
;            6.117821e-4 -0.010391197 -0.0021239072 -0.008927017 -0.01838119
;            0.021270677 -0.0061068535 -0.0021966845 0.009331189 -0.0061398335)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(format t "~%Average best fitness for 50 trials: ~f~%"
  (avg-best-fitness *number-of-trials* 1000 50
    :setup #'float-vector-sum-setup
    :creator #'float-vector-creator
    :selector #'tournament-selector
    :modifier #'float-vector-modifier
    :evaluator #'schwefel-f
    :printer #'simple-printer))
|#

;;; Results
; Best Individual of Generation...
; Fitness: 8255.884
; Individual:(4.1916695 4.191471 4.206466 4.1881437 -3.0363421 4.2178636 4.212388
;            4.237754 4.208902 4.2316628 4.213921 4.226744 4.200037 4.233181
;            4.2241774 4.201371 4.2014604 4.197831 4.2071953 4.223152)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPORT
;; See the pdf for a full report of the results.
; Design and Implementation:
;
;   Implementing the functions for this assignment was a simple conversion from
; the algorithms provided in the lecture. Utilizing the provided helper
; functions, I was able to simplify the implementation in many cases. The only
; function pseudo-code not provided was the evolve function. This function,
; however, is a straight forward loop creating new generations each iteration.
;   First, I setup any parameters necessary for the problem, then I create the
; first generation with the given creator function. I then loop generation
; times, crossing-over and mutating a selected subset of the previous
; generation, repeating the above process with the new generation. Depending on
; the population size and number of generations the problem can take a
; significant time to solve.
;   Overall the implementation of the functions was straight forward and easy
; to implement in a C-like manor. Some thought was required when implementing
; in a more standard LISP style, but not more than any previous assignment.
;
; Reflection:
;
;   This assignment helped to cement the ideas behind genetic algorithms and
; there usefulness. The provided evaluation functions were helpful when testing
; and exemplified the ability of genetic algorithms. The only step of testing
; that was troublesome was devising good parameters for the functions. The
; values decided on were devise through a process of trial and error.
;
; Analyzing the Results:
;
;   Using the provided evaluation functions, I was able to test my
; implementation. This testing involved varying the parameters of the functions
; in the form of the tournament size, mutation rate, crossover rate, and
; mutation variance. The population size is held constant at 50 individuals.
; The size of the floating point vector is 20, and the number of generations is
; 1000.
; For the first test function, sum, the fitness is exactly equal to the sum of
; the genes of the individual. All parameters were left at their default
; values. The average best fitness of 50 trials was equal to 102.2. With a max
; fitness of 5.12 * 20 = 102.4, the percent error of the results is 0.2%. This
; is well within reason.
; For the next test, step, the fitness is equal to the sum of the floor of each
; gene plus six times the length of the vector. Therefore the max fitness
; achievable is 20 * 5 + 20 * 6 = 220. As above the default parameters were
; used and the resulting average best fitness was 217.86. The percent error of
; these results is 0.97% well within reason.
; The sphere test function minimizes the squared sum of the vectors values.
; Therefore vectors closer to zero have a better fitness. In experimentation,
; using a tournament size of 14 resulted in an average best fitness of
; -0.000315. For this test fitnesses range from -524.288 to 0.0, so the
; resulting value is well within expectation.
; For the Rosenbrock testing function the expected value is a vector of all
; ones. This implies that the best fitness is 0.0. From experimentation with a
; tournament-size of 14, crossover probability of 0.01, mutation probability of
; 0.01, and mutation variance of 0.01, an average best fitnesses of -28.98 was
; achieved.
