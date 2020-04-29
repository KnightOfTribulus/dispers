;;;; dispers.lisp

(in-package #:dispers)

;;;; вспомогательные функции:
(defun transpose (m)
  (apply #'mapcar #'list m))

(defun avg (list)
    (/ (apply #'+ list)
       (length list)))

(defun sum (list)
  (apply #'+ list))

(defun matrix-multiply (a b)
  (flet ((col (mat i) (mapcar #'(lambda (row) (elt row i)) mat))
         (row (mat i) (elt mat i)))
    (loop for row from 0 below (length a)
          collect (loop for col from 0 below (length (row b 0))
                        collect (apply #'+ (mapcar #'* (row a row) (col b col)))))))

;;;; ЗАДАНИЕ 1:
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
(defparameter *distance* '(3.5 2.4 4.9 4.2 3 1.3 1 3 1.5 4.1)
  "расстояние, км")

(defparameter *time* '(16 13 19 18 12 11 8 14 9 16)
  "время, мин")

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
      (gp :set :xlabel "x")
      (gp :set :ylabel "y")
      (gp :set :size '("ratio 1"))
      (gp :set :title (format nil "r= ~D, y=~ax+~a" (correl x y) a b))
      (plot #'(lambda ()
		(mapcar (lambda (xi yi)
			  (format t "~&~a ~a" xi yi))
			x y))
	    :with '(:points :linestyle 7))
      (func-plot (format nil "~a * x + ~a" a b)))))


;;;;ЗАДАНИЕ 4:
(defparameter *x-matrix*
  #2A((1 4    15   17  )
      (1 3.8  15.2 16.8)
      (1 8.2  15.5 16  )
      (1 14.7 18.1 20.2)
      (1 19.8 15.8 18.2)
      (1 8.6  18.3 17  )
      (1 12.6 15.4 16.4)
      (1 5.8  16   17.7))
  "матрица X")


(defparameter *x-1*  '(4 3.8 8.2 14.7 19.8 8.6 12.6 5.8)
  "Затраты на рекламу, млн. руб.")
(defparameter *x-2*  '(15 15.2 15.5 18.1 15.8 18.3 15.4 16)
  "Цена, руб")
(defparameter *x-3*  '(17 16.8 16 20.2 18.2 17 16.4 17.7)
  "Конкурентная цена, руб.")
(defparameter *x-4*  '(100 101 104 107 108 110 110.3 112.3)
  "Индекс покупательной способности")

(defparameter *y-column*
  '(126 148 274 432 367 321 331 364)
  "Реализация, млн. руб.")

(cl-mathstats:multiple-linear-regression-verbose *y-column* *x-1* *x-2* *x-3*)

(defun multiple-regression (y &rest xs)
  (multiple-value-bind (a-coefs)
      (apply #'cl-mathstats:multiple-linear-regression-brief y xs)
    (let* ((xa (matrix-multiply
		(transpose (append (list (make-list (length (elt xs 0)) :initial-element 1)) xs)) 
		(mapcar (lambda (ai) (list ai))
			a-coefs)))
	   (e-coefs (mapcar (lambda (yi xai) (- yi (car xai)))
	   		    y
	   		    xa))
	   (y-coefs (mapcar (lambda (ei yi) (- yi ei))
			    e-coefs y)))
      (format t "~&a coefficients: ~&~a~&e-coefficients: ~&~a~&y-coefficients:~&~a" a-coefs e-coefs y-coefs))))

(defun test-multi-regression ()
  (multiple-regression *y-column* *x-1* *x-2* *x-3* *x-4*))

(let ((y1 '(126 148 274 364))
      (x1 '(4 3.8 8.2 5.8))
      (x2 '(15 15.2 15.5 16))
      (x3 '(17 16.8 16 17.7)))
  (multiple-regression y1 x1 x2 x3))
