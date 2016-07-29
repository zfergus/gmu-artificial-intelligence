(defun count-s-expressions (lis)
 "Counts the number of s-expressions in a given expression"
 (1+ (count-s-expressions-h lis)))

(defun count-s-expressions-h (lis)
 "Helper function: call count-s-expressions instead"
 (if (atom lis) 0
     (apply #'+ (length lis)
            (mapcar #'count-s-expressions-h lis))))

(defun full-reverse (lis)
  (if (atom lis) lis
  (append (full-reverse (rest lis))
    (list (first lis)))))
