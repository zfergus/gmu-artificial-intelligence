;;; First Lisp file
;;; Writen by Zachary Ferguson

(print "Hello, World!")

(defun factorial (n)
    (if (<= n 1) 1
        (* n (factorial (- n 1)))))
