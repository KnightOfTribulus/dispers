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
  (/ (sum (mapcar #'(lambda (gr) (group-dispersion gr))
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

(defparameter *group-1* (make-instance 'group :xi '(13 14 15 17 16 15)))
(defparameter *group-2* (make-instance 'group :xi '(18 19 22 20 24 23)))

;; TODO провести тестовые рассчеты для отчета здесь:

;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

;;;; КОНЕЦ ЗАДАНИЯ 1.


;;;; ЗАДАНИЕ 2:

;; each point is a list of 2 numbers
(defparameter *x* '(105 102 100 106 112 115 118 116 120 125 125 128)
  "число жалоб(Х)")
(defparameter *y* '(68 71 69 66 65 70 75 76 78 77 79 82)
  "затраты на ремонт (Y)")

;; (defparameter *x*
;;   (mapcar (lambda (number) (coerce number 'double-float))
;;    '(105 102 100 106 112 115 118 116 120 125 125 128))
  
;;   "число жалоб(Х)")
;; (defparameter *y*
;;     (mapcar (lambda (number) (coerce number 'double-float))
;; 	    '(68 71 69 66 65 70 75 76 78 77 79 82))
;;   "затраты на ремонт (Y)")



;; коэффициент корреляции без временного лага
;; !!!probably need switching to "cl-mathstats"
;; statistics:

(defun correl (*x* *y* &key (lag 0))
  (statistics:correlation-coefficient
   (mapcar (lambda (x y) (list x y))
	   *x*
	   (nthcdr lag *y*))))

(defparameter *r0*
  (correl *x* *y*)
  "коэффициент корреляции без временного лага") 

(defparameter *r1*
  (correl *x* *y* :lag 1)
  "коэффициент корреляции с временным лагом 1")

(defparameter *r2*
  (correl *x* *y* :lag 2)
  "коэффициент корреляции с временным лагом 2") 

(defparameter *r3*
  (correl *x* *y* :lag 3)
  "коэффициент корреляции с временным лагом 3")

(defparameter *plot-name* "gnuplot-output")

(defun plot-correl (&key (x *x*)  (y *y*) (output-file "correl.png")  (lag 0))
  (let* ((new-y (nthcdr lag y)))
    (with-plots (*standard-output* :debug t)
      (gp-setup :terminal '(pngcairo)
		:output output-file)
      (gp :unset :key)
      (gp :set :size '("ratio 1"))
      (gp :set :title (format nil "r= ~D, lag ~a" (correl x new-y) lag))
      (plot #'(lambda ()
		(mapcar (lambda (x y)
			  (format t "~&~a ~a" x y))
			x new-y))
	    :with '(:points :linestyle 7)))))

;; TODO провести тестовые рассчеты для отчета здесь:

;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

;;;; ЗАДАНИЕ 3
(defvar *distance* '(3.5 2.4 4.9 4.2 3 1.3 1 3 1.5 4.1)
  "расстояние, км")

(defvar *time* '(16 13 19 18 12 11 8 14 9 16)
  "время, мин")

(statistics:linear-regression
 (mapcar (lambda (x y)
	   (list x y))
	 *distance*
	 *time*))



(defun plot-regression (&key (x *distance*) (y *time*) (output-file "regression.png"))
  (multiple-value-bind (b a)
      (statistics:linear-regression
       (mapcar (lambda (x y)
		 (list x y))
	       *distance*
	       *time*))
    (with-plots (*standard-output* :debug t)
      (gp-setup :terminal '(pngcairo)
		:output output-file)
      (gp :unset :key)
      ;;      (gp :set :xrange (list  min-x max-x))
      ;;      (gp :set :xrange (list min-y max-y))y
      (gp :set :size '("ratio 1"))
      (gp :set :title (format nil "r= ~D, y=~ax+~a" (correl x y) a b))
      (plot #'(lambda ()
		(mapcar (lambda (xi yi)
			  (format t "~&~a ~a" xi yi))
			x y))
	    :with '(:points :linestyle 7))
      ;;      (func-plot (format nil "[~a:~a] ~a * x + ~a" min-x max-x a b))
      (func-plot (format nil "~a * x + ~a" a b))
      )))


;; (plot #'(lambda ()
;; 		(format t "~a ~a" min-x (+ (* a min-x) b))
;; 		(format t "~&~a ~a" max-x (+ (* a max-x) b)))
;; 	    :with '(:lines))
