;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;    Utils       ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defpackage :utils
  (:use :cl)
  (:export :make-adjustable-string
           :str-concat
           :push-char
           :split-at))



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
  (vector-push-extend c str))


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



