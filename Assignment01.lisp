; ASSIGNMENT 1: Getting up to speed in Lisp.
; NAME: Zachary Ferguson


; Questions 1 through 4 should be entirely answered as lisp comments

; In some cases there are "beat the professor" or "extra credit" elements.
; If you're going for those elements, please indicate so in comments.




; 1. What is printed and returned by the following expressions, and why?
; ...yes, I know that the third expression acts unusually.  Explain why.

(sqrt (- (+ 1/2 1/3 1/6)))
; This expression returns the square root of the negated sum of the fractions.
; This results in the complex number #C(0.0 1.0).

(quote (sqrt (- (+ 1/2 1/3 1/6))))
; This returns the quoted expression (sqrt (- (+ 1/2 1/3 1/6))).
; (quote) does not evaluate the argument, so it does not sum or negate the
; values.

(sqrt (quote (- (+ 1/2 1/3 1/6))))
;______________________________________________________________________________
;______________________________________________________________________________

(print (sqrt (- (+ 1/2 1/3 1/6))))
; This first evaluates the expression in the same way as the first expression.
; The evaluated value is then printed and returned.

; 2. Describe the steps the Lisp interpreter will go through to evaluate
; the following steps, including recursive substeps.

(+ (- 5 1) (+ 3 7))
; 1. The expression (- 5 1) is evaluated recursively
;   a. The value 5 is evaluated to be 5 since it is atomic
;   b. The value 1 is evaluated to be 1 since it is atomic
; 2. (- 5 1) results in 4
; 3. The expression (+ 3 7) is evaluated recursively
;   a. The value 3 is evaluated to be 3 since it is atomic
;   b. The value 7 is evaluated to be 7 since it is atomic
; 4. (+ 3 7) results in 10

(if (= 5 4)
  (print (+ 3 7))
  (print (print "hello")))


; 3.  Create Lisp expressions which perform the same function as the
; following C expressions:

; a + b * c - d
(- (+ a (* b c)) d)

; ((a != b) ?
;     ((a > b) ? "greater" : "less")
;     : "equal")
(if (not (equalp a b))
  (if (> a b) "greater"
    "less")
  "equal")

; if (a==b) return MAX(4 * 3 + cos(2.1), 7 / log(1.2)); else return
; exp(abs (a-b) * (3 % a));
(if (equalp a b)
    (max (+ (cos 2.1) (* 4 3)) (/ 7 (log 1.2)))
  (exp (* (abs (- a b)) (mod 3 a)


; 4. What do the following functions do?  I'm looking for BOTH a specific
; description AND some insight into what purpose you'd use it for.

;;; Function 1: MYSTERY ;;; example usage: (mystery)

(defparameter *mystery-value* 234567)

(defun mystery ()
 (setf *mystery-value*
       (mod (* *mystery-value* 16807) 2147483647)))

; (mystery) sets the global variable, *mystery-value*, to a pseudorandom value.
; This could be used to generate a pseudorandom sequence of numbers.

;;; Function 2: RIDDLE ;;; pass in a list for LIS ;;; example usage:
(riddle '(a b c d e f g))

(defun riddle (lis)
 (if (not (null lis)) ;; if lis is non-empty (it's not NIL)
     (progn
       (riddle (rest lis))
       (print (first lis))))
 "All Done")

; (riddle lis) prints the given list in reverse and then returns the string
; "All Done".


; 5. Define a function called FULL-REVERSE which reverses all the items in
; a list and returns that.  Each sublist in the list gets recursively
; full-reversed as well.  If the item passed in is an atom and not a list,
; just return it.  You can use any lisp function or macro you find.  The
; challenge is to do this in the smallest number of possible
; s-expressions.

; BEAT THE PROFESSOR.  The professor can get this done in 19
; s-expressions. We don't expect you to get that small, but see what you
; can do.  In fact, the professor actually can do it in just 5
; s-expressions, but to do so must do something which you would probably
; consider cheating.

; If you'd like to count s-expressions (the only fair way to count the
; length of a Lisp program -- since lines can be packed with so much
; stuff), here's a little program you can use.

(defun count-s-expressions (lis)
 "Counts the number of s-expressions in a given expression"
 (1+ (count-s-expressions-h lis)))

(defun count-s-expressions-h (lis)
 "Helper function: call count-s-expressions instead"
 (if (atom lis) 0
     (apply #'+ (length lis)
            (mapcar #'count-s-expressions-h lis))))

; Example usage:
; (count-s-expressions '(defun foo (x) (+ 3 x)))) -> 9
; [namely: (), defun, foo, (), x, (), +, 3, x ]

(defun full-reverse (lis)
  (if (atom lis)
    lis
  (append (full-reverse (rest lis))
    (list (first lis)))))
; 23 expressions

(defun cheap-full-reverse (lis)
  (if (atom lis)
    lis
  (reverse lis)))

; 6. Create a function called TRANSPOSE which performs a transposition on
; a 2-dimensional array, returning a new array as apporpriate.  Extra
; credit awesome superpowers to you if you can perform a "transpose" of
; sorts on arrays of ANY number of dimensions (including 1), where the
; dimensions are essentially rotated.  For example, given an n-dimensional
; array, return a new array where an element at <x0, x1, x2, x3, .. ,
; xn-1, xn> is now located at <xn, x0, x1, x2, x2, .. , xn-1>.  Note that this
; extra credit is quite nontrivial.

(defun transpose (mat)
  (apply #'mapcar #'list mat))



; 7.  Write a function called LOG* .  This function takes a single
; argument and calls LOG on it repeatedly until the result drops <= 0.
; Then it returns the number of times it called LOG.

; You are not permitted to use LET or SETF (or any other setting or
; definition function like SET or SETQ or DEFVAR or DEFPARAMETER).  Hint:
; this means you must write this function recursively.
(defun log* (n)
  (if (<= n 0.0)
    0
  (+ 1 (log* (log n)))))


; 8. Write an ITERATIVE function (fib1 n) which PRINTS the first n values
; of the Fibonacci Sequence.  The function should be O(n).  Do NOT make
; global variables.
(defun fib1 (n)
  (let ((prev1 0) (prev2 1))
    (dotimes (i n)
      (print prev1)
      (let ((tmp (+ prev1 prev2)))
        (rotatef prev1 prev2 tmp)))
    (print prev1)))


; 9. Write a RECURSIVE version (fib2 n) which does not use LET, DOTIMES,
; SETF, or any other variable-setting or iterative mechanism.  In short:
; you're not allowed to create or set variables.  You are permitted to
; write more than one function -- the fib2 function can call another one
; that's helpful to you.  The function does not need to be O(n) -- just
; write in the easy but "stupid" way.
(defun fib2 (n)
  (if (<= n 0) 0
    (if (= n 1) 1
      (+ (fib2 (- n 1)) (fib2 (- n 2))))))


; 10. A classic: Write a function called SMASH.  This function takes a
; single argument.  If it's not a list, then it returns a list consisting
; of the argument.  If it's nil, it returns nil.  Otherwise (it's a list)
; it removes all the parentheses and NILs and returns the result.  For
; example:
;
; > (smash '(((a b c) nil 4 (nil 5 () nil 6 (7 ((((8)))) 9)))))
; (A B C 4 5 6 7 8 9)
;
; > (smash '(a b c (d e f) nil nil () ((())) nil 5))
; (A B C D E F 5)
;
; > (smash nil)
; NIL
;
; > (smash '(((()))))
; NIL
;
; > (smash 14)
; (14)
;
; You are not permitted to use LET or SETF (or any other setting or
; definition function like SET or SETQ or DEFVAR or DEFPARAMETER).  Hint:
; this means you must write this function recursively.
;
; BEAT THE PROFESSOR.  There are two ways to write this function.  First,
; you can write it recursively.  Second, you can write it with a
; combination of recursion, APPLY, and MAPCAR.  The professor can get the
; first version down to 29 s-expressions, and the second version down to
; 24 s-expressions. See the COUNT-S-EXPRESSIONS function described earlier
; for counting s-expressions.



; 11. Write a function called MY-REDUCE.  This function takes as arguments
; *another* function (of two arguments) and a list of s-expressions.  The
; list will always be at least two expressions long.  MY-REDUCE takes the
; first two items in the list and passes them to the function.  It then
; takes the result, plus the next item in the list, and passes *that* to
; the function.  And so on.  At the end, it returns the result.
;
; For example, let's say you have a function called FOO which takes two
; arguments.  Then
;        (my-reduce #'foo '(a b c d e)) Is just like saying
;        (foo (foo (foo (foo a b) c) d) e)
;
; Again, you are not permitted to use LET or SETF (or any other setting or
; definition function like SET or SETQ or DEFVAR or DEFPARAMETER).
; Another hint: look up FUNCALL.  This one can't be done using MAPCAR, so
; don't bother playing with that.  Note: there is already a function
; called REDUCE in Lisp, and it does exactly this and lots more too.  You
; can look it up. But you are not permitted to use REDUCE -- that would
; make this exercise a little dumb, wouldn't it?  :-)
;
; Here's my implementation of MY-REDUCE in action:
;
; ;;; (expt 2 3) is 2^3
;
; > (my-reduce #'expt '(2 3 4 5 6 7))
; 3940842455221416269534854318363891517281917224975164265532215418234933
; 6765880096106556447863882000035605638833716703554207400894540191395023
; 6214360506399705231203021164366069389563701733455174652493802096528279
; 6593812594835089161767825168926163221548818705965056545777743298081872
; 5650237046825687537631627813593798578816088851880913783787318008632718
; 3792757748702946460720720770436177477377229784500022657580657233628383
; 9301379146196840092207912670897685521829036186031469500842192427800725
; 7807164800126572667987375177230234311435842855213499193805644680391721
; 6962620267368806273089867659639177213488960155211698149211030681779788
; 5781410543592742895564114004365987049278212752148814889702185765573255
; 51889577507340928956338410400961096026352642413831783448576
;
; BEAT THE PROFESSOR: Your professor can write this function in 31
; s-expressions.  See the COUNT-S-EXPRESSIONS function described earlier
; for counting s-expressions.


; 12.  Rewrite the function above so that instead of a list it takes an
; arbitrary number of arguments.  That is, you'd call it as:
;
;     (my-reduce #'expt 2 3 4 5 6 7)



; 13. Write the function MY-REMOVE-DUPLICATES.  This function takes a list
; and returns a new list with all duplicates removed.  The remaining
; elements must stay in the same order as they were before.  For example:
;
; (my-remove-duplicates '(a b c a b b d e e f a)
; > '(A B C D E F)
;
; You must implement this function in O(n).  Hint: a hash table might be
; helpful.  You may, of course, not use REMOVE-DUPLICATES,
; DELETE-DUPLICATES, or similary functions in Lisp.



; 14. Modify your FIB2 function to be O(n) using a hash table.
; The function should still be recursive.



; 15. Modify your FIB2 function to be O(n) without using a hash table.
; The function should still be recursive (though perhaps recursive in
; a different way).


; 16.  Write a function called DIVISIBLE which takes a number N and a list
; of prime numbers L.  If any of the numbers L can divide evenly into N,
; then DIVISIBLE should return true (t) else it should return false (nil).
; While you can write this function iteratively, it's probably easier to
; do it recursively or hacking over mapping functions.


; 17. Using the previous function, write a function called SIEVE which
; takes a single value VAL and prints to the screen all of the prime
; numbers from 2 to VAL inclusive, using the sieve of Eratosthenes. The
; general idea is: a number is prime if none of the prime numbers less
; than it can divide it. The function should return NIL.
;
; If you were really into this, you'd modify the function so that only the
; primes from 2 through Floor(Sqrt(VAL)) were checked to see if they
; divide into VAL.  To do so truly efficiently, you'd probably need to
; modify DIVISIBLE.



; 18. Write a function which prints a Lisp s-expression form of math into
; an infix form.  Extra credit if you can handle both unary and binary
; usages of the minus sign.  Also extra credit if you properly handle
; order of operations to cut down on the number of parentheses.  Finally,
; extra credit if you can provide an option to write it to any arbitrary
; stream or file.
;
; Example:
;
;        (infix '(* (+ 3 2) (/ 1 (- 3))))
;
; Prints...
;        (3 + 2) * 1 / -3
