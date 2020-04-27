;;;; dispers.lisp

(in-package #:dispers)

;;;; ЗАДАНИЕ 1:

;; Определим структуру, соответствующуу испытанию:
(defstruct experiment
  (number      nil)
  (xi          nil)
  (xi-xicp     nil)
  (sqr[xi-xicp] nil) ;; в силу синтаксических особенностей Common Lisp, данное поле этой структуры отвечает (Xi-Xicp)^2
  )

;; для упрощения ввода таблиц определим следующую синтаксическую конструкцию:
(defmacro defgroup (name &body body)
  
  (let ((experiments (append '(list)
			     (mapcar (lambda (l) 
				       (destructuring-bind (number xi xi-xicp sqr-xi-xicp) l
					 `(make-experiment :number ,number
							   :xi ,xi
							   :xi-xicp ,xi-xicp
							   :sqr[xi-xicp] ,sqr-xi-xicp)))
				     body))))
    `(defvar ,name ,experiments)))

;; пример использоваия макрокомманды "defgroup"
;; (macroexpand-1 '(defgroup group-1 (1 1 1 1)
;;                                   (2 2 2 2) )) =>
;; (DEFVAR GROUP-1
;;   (LIST (MAKE-EXPERIMENT :NUMBER 1 :XI 1 :XI-XICP 1 :SQR-XI-XICP 1)
;;         (MAKE-EXPERIMENT :NUMBER 2 :XI 2 :XI-XICP 2 :SQR-XI-XICP 2)))

(defgroup group-1
  (1 13 -2 4)
  (2 14 -1 1)
  (3 15  0 0)
  (4 17  2 4)
  (5 16  1 1)
  (6 15  0 0))

(defgroup group-2
  (1 18 -3 9)
  (2 19 -2 4)
  (3 22  1 4)
  (4 20 -1 1)
  (5 24  3 9)
  (6 23  2 4))

(defun group-sum (group &key xi sqr[xi-xicp])
  (cond
    (xi
     (apply #'+ (mapcar (lambda (ex) (experiment-xi ex)) group)))
    (sqr[xi-xicp]
     (apply #'+ (mapcar (lambda (ex) (experiment-sqr[xi-xicp] ex)) group)))))

;; определим функцию для рассета внуртигрупповой дисперсии
(defun group-disperion (group)
  (/
   (group-sum group :sqr[xi-xicp] t)
   (length group) ;; количество испытаний в группе
   ))
