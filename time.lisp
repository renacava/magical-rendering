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

(defun now ()
  (float (/ (get-internal-real-time) internal-time-units-per-second)))

(defun timeslice (func min-time-between-calls &optional sleep-time)
  (let* ((prior-result nil)
         (prior-time 0))
    (lambda (&rest args)
      (let ((new-time (get-internal-real-time)))
        (if (>= (- new-time prior-time) (* (resolve min-time-between-calls) internal-time-units-per-second))
            
            (setf prior-result (apply func args)
                  prior-time new-time)
            (let ((resolved-sleep-time (resolve sleep-time)))
              (when (numberp resolved-sleep-time)
                (when sleep-time
                  (sleep resolved-sleep-time))))))
      prior-result)))

(defun timeslice-fps (func max-fps &optional sleep-time)
  (timeslice func (lambda () (/ 1 (resolve max-fps))) sleep-time))
