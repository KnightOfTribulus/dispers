;;;; dispers.lisp

(in-package #:dispers)

;;;; ЗАДАНИЕ 1:
(defun avg (list)
    (/ (apply #'+ list)
       (length list)))

(defun sum (list)
  (apply #'+ list))

(defclass group ()
  ((%xi :initarg :xi
	:accessor xi
	:initform (error "xi wasn't provided"))
   (%xi-mean :accessor xi-mean
	     :initform nil)
   (%xi-ximean :initform nil
	       :accessor xi-ximean)
   (%[xi-ximean]^2 :accessor [xi-ximean]^2
		   :initform nil)))


(defmethod initialize-instance :after ((this group) &key)
  (with-accessors ((xi-mean xi-mean)
		  (xi xi)
		  (xi-ximean xi-ximean)
		  (sq [xi-ximean]^2))
      this
    (setf xi-mean (avg xi))
    (setf xi-ximean (mapcar #'(lambda (xi-element) (- xi-element xi-mean))
			    xi))
    (setf sq (mapcar #'(lambda (xi-element) (expt (- xi-element xi-mean) 2))
			    xi))))

(defmethod units ((this group))
  (length (xi this)))

;; групповая дисперсия
(defmethod group-dispersion ((this group))
  (with-accessors ((sq [xi-ximean]^2))
      this
    (/ (apply #'+ sq)
       (length sq))))

;; межгрупповая дисперсия delta^2
(defun inter-dispersion (&rest groups)
  (let ((x-mean
	 (/ (sum (mapcar (lambda (group) (* (xi-mean group) (units group)))
			 groups))
	    (sum (mapcar (lambda (group) (units group))
			 groups)))))
    (/ (sum (mapcar (lambda (group)
		      (* (units group)
			 (expt (- (xi-mean group) x-mean) 2)))
		    groups))
       (sum (mapcar (lambda (group)
		      (units group))
		    groups)))))

(defmethod print-object ((this group) output)
  (with-accessors ((xi xi)
		   (xi-mean xi-mean)
		   (xi-ximean xi-ximean)
		   (sq [xi-ximean]^2))
      this
    (print-unreadable-object (this output) 
      (format output "~&~5,10T#~5,10TXi~5,10TXi-Xi_cp~5,10T[xi-ximean]^2")
      (format output "~&-----------------------------------------------")	       
      (loop for i from 0 below (length xi)
	 do  (progn
	       (format output
		       "~&~5,10T~a~5,10T~a~5,10T~a~5,10T~a"
		       (+ 1 i)  (elt xi i) (elt xi-ximean i) (elt sq i))))
      (format output "~&-----------------------------------------------"))))

;; средняя групповая дисперсия 
(defun mean-group-dispertion (&rest groups)
  (/ (apply #'+ (mapcar #'(lambda (gr) (group-dispersion gr))
			groups))
     (length groups)))

;; общая дисперсия sigma^2
(defun overall-dispersion (&rest groups)
  (+ (apply #'mean-group-dispertion groups)
     (apply #'inter-dispersion groups)))

;; показатель тесноты связи h
(defun correlation-factor (&rest groups)
  (sqrt (/ (apply #'inter-dispersion groups)
	   (apply #'overall-dispersion groups))))

(defparameter group-1 (make-instance 'group :xi '(13 14 15 17 16 15)))
(defparameter group-2 (make-instance 'group :xi '(18 19 22 20 24 23)))

;; провести тестовые рассчеты для отчета здесь:

;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

;;;; КОНЕЦ ЗАДАНИЯ 1.


;;;; ЗАДАНИЕ 2:

;; each point is a list of 2 numbers
(defparameter row-1 ')
(defparameter row-2)
