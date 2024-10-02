(in-package #:magical-rendering)

(defun seconds-since (prior-time current-time)
  (coerce (/ (- current-time prior-time) internal-time-units-per-second)
          'double-float))

(defmacro defun-fps-limited (fps func-name args &body body)
  (let ((prior-time (gensym))
        (prior-result (gensym))
        (current-time (gensym))
        (delta-seconds (gensym)))
    `(let ((,prior-time (- internal-time-units-per-second))
           (,prior-result nil))
       (defun ,func-name ,args
         (let* ((,current-time (get-internal-real-time))
                (,delta-seconds (seconds-since ,prior-time ,current-time)))
           (if (>= ,delta-seconds (/ 1 ,fps))
               (progn
                 (setf ,prior-time ,current-time)
                 (setf ,prior-result (progn ,@body)))
               (progn
                 (sleep 0.0001)
                 ,prior-result)))))))
