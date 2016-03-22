(in-package :cl-user)

(defpackage #:my-new-package
  (:nicknames #:newpack)
  (:use :cl :cl-user)
  (:export #:mutate))

(in-package :my-new-package)

(defun mutate-step (s acc)
  "Generates a progression of an l-system."
  (if (equal (car s) nil)
    acc
    (if (char= (car s) #\A)
      (mutate-step (cdr s) (concatenate 'string acc "AB"))
      (if (char= (car s) #\B)
        (mutate-step (cdr s) (concatenate 'string acc "A"))
        nil))))

(defun mutate-private (s iterations goal)
  (if (equal iterations goal)
    s
    (mutate-private (coerce (mutate-step s "") 'list) (+ iterations 1) goal)))

(defun mutate (input goal)
  "Generates n goal iterations of an l-system."
  (coerce (mutate-private (coerce input 'list) 0 goal) 'string))
