;;; First Lisp file
;;; Writen by Zachary Ferguson

(print "Hello, World!")

(defun factorial (n)
    (if (<= n 1) 1
        (* n (factorial (- n 1)))))

(defun factorialI (n)
  (let (x 1)
    (dotimes (i n x)
      (setf x (* x (+ 1 i))))))
