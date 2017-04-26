



(defpackage :user-password-map
  (:use :cl :file-io :utils)
)


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














