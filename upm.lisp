#!/usr/bin/clisp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;       FILE IO     ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#||
(defpackage :file-io
  (:use :cl)
  (:export :read-file :append-line)
)
||#

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;    Utils       ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#||
(defpackage :utils
  (:use :cl)
  (:export :make-adjustable-string
           :str-concat
           :push-char
           :split-at))
||#


(defun make-adjustable-string (str) 
  (make-array (length str)
              :fill-pointer (length str)
              :adjustable t
              :initial-contents str
              :element-type (array-element-type str)))

(defun str-concat (str1 str2) 
  (concatenate 'string str1 str2))

(defun push-char (c str)
  ;(setf str (make-adjustable-string str))
  (vector-push-extend c str)
  str)


;;Token character is ignored
(defun split-at (str token)
  (let  (   (slen (length str))
            (str-arr (make-array '(0)
                              :fill-pointer t
                              :adjustable t
                              :element-type 'string))
  
            (str-temp (make-array '(0) 
                               :fill-pointer t
                               :adjustable t
                               :element-type 'character
                               :initial-contents ""))
            (str-init (make-array '(0) 
                               :fill-pointer t
                               :adjustable t
                               :element-type 'character
                               :initial-contents ""))   )
  
        (dotimes (idx slen) 
            do      (if (equal (aref str idx ) token) 
                        (progn 
                            (vector-push-extend str-temp str-arr)
                            (setf str-temp str-init)
                        )
                        (push-char (aref str idx) str-temp)))    

        (vector-push-extend str-temp str-arr)
        str-arr))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;#!/usr/bin/sbcl --script 



#||
(defpackage :user-password-map
  (:use :cl :file-io :utils)
)
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;MAIN;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defparameter *upm-filename* "usrpwmap.data")
(defparameter up-split-token #\|)


(defvar umptb NIL)
(defun make-upm-kv-hashtb (upm-arr)
  (let  ((upm-kv-hashtb (make-hash-table :test 'equal)))

        (dotimes (i (length upm-arr))
            do    (setf upm-kv-pair (split-at (aref upm-arr i) up-split-token))
                  (setf (gethash (aref upm-kv-pair 0) upm-kv-hashtb) (aref upm-kv-pair 1)))
        upm-kv-hashtb))

;; -s --search
(defun search-pw (user-name)
  (setf umptb (make-upm-kv-hashtb (read-file *upm-filename*)))
  (let ((pw-rst (gethash user-name umptb)))
       
       pw-rst))



;; -p --print   

#||
Add nice frame to hold pairs later, it would be like:

||============================||========================||
||Idx|        USER            ||       PASSWORD         ||
||============================||========================||
||1  |       u1               ||      pwd1              ||
||============================||========================||
|| ...
|| ...
...

And if the information message is too long, it should automatically add #\newline to the message and enlengthen that respective message holding block vertically to write the correctly formed frame
||#

(defun print-map ()
  (let ((file-str-arr (read-file *upm-filename*)))
       (dotimes (str-index (length file-str-arr))
                (format t "~a ~%" (aref file-str-arr str-index)))))



;; -a --append
(defun append-upm-line (str)
  (append-line *upm-filename* str))





;; -h --help


(defvar help-msg 
"   -a <\"username|password\">          Append a user password map line  
   -p                                Print the user-password map      
   -s <username>                     Seach a password given the username  ")






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "~a ~%" *args*)

(defun exec (argv)
  (defvar flag (nth 0 argv))
  (defvar arg  (nth 1 argv))
  (if flag
    (cond ((equal flag "-p")
           (format t "~a ~% " (print-map)))
          ((equal flag "-s")
           (format t "~a ~%" (search-pw arg)))
          ((equal flag "-a")
           (format t "~a ~%" (append-upm-line arg)))
          (t (format t "~a" help-msg))))
  )

(exec *args*)





