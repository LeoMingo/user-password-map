

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;       FILE IO     ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defpackage :file-io
  (:use :cl)
  (:export :read-file :append-line)
)


(defun read-file (filename)
  (defparameter file-str-arr (make-array '(0) :initial-element ""
                                              :element-type 'string
                                              :adjustable t
                                              :fill-pointer t))
  
  (let ((in (open filename :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
            while line do 
                (vector-push-extend line file-str-arr))
      
      (close in)))
      file-str-arr
    )


(defvar *j* "j")


(defun append-line (filename line-string)
  (with-open-file (my-stream
                   filename
                   :direction :output
                   :if-exists :append                 ; :supersede to overwrite
                   :if-does-not-exist :create)       
    (princ #\newline my-stream)
    ;Or with another approach 
    ;(setf ls (make-adjustable-string line-string))
    ;But this will output yet another #\newline, why? because of file EOF char missing?
    (princ line-string my-stream)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


