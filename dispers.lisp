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

(defmethod group-dispersion ((this group))
  (with-accessors ((sq [xi-ximean]^2))
      this
    (/ (apply #'+ sq)
       (length sq))))

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

(defun mean-group-dispertion (&rest groups)
  (/ (apply #'+ (mapcar #'(lambda (gr) (group-dispersion gr))
			groups))
     (length groups)))

(defun overall-dispersion (&rest groups)
  (+ (apply #'mean-group-dispertion groups)
     (apply #'inter-dispersion groups)))

(defun correlation-factor (&rest groups)
  (sqrt (/ (apply #'inter-dispersion groups)
	(apply #'overall-dispersion groups))))

(defparameter group-1 (make-instance 'group :xi '(13 14 15 17 16 15)))
(defparameter group-2 (make-instance 'group :xi '(18 19 22 20 24 23)))
