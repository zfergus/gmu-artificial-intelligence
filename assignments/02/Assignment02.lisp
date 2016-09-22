; CS 480: ASSIGNMENT 02
; NAME: Zachary Ferguson

; Write small functions or expressions which do the following:

; 1. Return a random element from a sequence.


; 2. Take two sequences of equal length, one called KEYS, and one called VALUES,
; and return a hash table with <KEY, VALUE> pairs.


; 3. Return the keys of a hash table


; 4. Remove all subsequences of a certain form from a sequence. For example, if
; you pass in '(1 2 3) as the subsequence and
; '(9 1 2 3 7 1 2 3 1 2 3 0 1 1 2 3 2 3 7) as the sequence, you should get back
; '(9 7 0 7). Notice that 1 1 2 3 2 3 was eliminated in two rounds.


; 5. Convert a sequence into a list of pairs. Each pair (VAL NUM) contains a
; unique value VAL which appeared in the original sequence, plus NUM which
; indicates how often it appeared. To be extra impressive, the pairs would be
; sorted by NUM (biggest to smallest). For example, '(A B A C D E E A D A B)
; would convert to '((A 4) (E 2) (D 2) (B 2) (C 1)). This can be done without
; any iteration or setting any local variables.


; 6. Tokenize a string according to a tokenization character. For example, if the
; string was "Four score and seven years ago", and the tokenization character was
; #\Space then the result would be ("Four" "score" "and" "seven" "years" "ago").


; 7. Make a general deep copier: The function COPY-TREE makes a deep copy of a
; list and all its sublists (and handles NIL or () properly). For example:
; (copy-tree '(a b (c d e) f (g h ( ) ))) -> '(A B (C D E) F (G H NIL))
;
; COPY-TREE also accepts atoms, and just returns them:
; (copy-tree 'a)
; -> A


; 8. Create a function called DEEP-COPY which does the same thing, but for any
; sequence. It takes a single argument (a sequence) and returns a deep copy. NIL
; or () should be handled properly. Note: child sequences can be of different
; types! For example:
; (deep-copy #(a b (c d e) #(e f g) ( ) "hij"))
; -> #(A B (C D E) #(E F G) NIL "hij")
; Hint: MAP, TYPEP and TYPE-OF


; 9. Now create a new version called DEEP-COPY2 which takes two arguments
; (a sequence and a type) converts the sequence and all its subsequences into
; the given type:
; (deep-copy2 #(a b (c d e) #(e f g) ( ) "hij") 'list)
; -> (A B (C D E) (E F G) NIL (#\h #\i #\j))
