(in-package #:magical-rendering)

(defparameter *running?* nil)
(defparameter *main-thread-name* "magical-rendering-main-thread")
(defparameter *main-thread* nil)

(defun main (&optional idle-func)
  (setf *running?* t)
  (unless *main-thread*
    (setf *main-thread*
          (bt:make-thread
           (lambda ()
             (loop
               (if *running?*
                   (livesupport:continuable
                    (try-open-window)
                    (when (functionp idle-func)
                      (funcall idle-func))
                    (funcall #'render-func))
                   (progn
                     (try-close-window)
                     (sleep 0.1)))))
           :name *main-thread-name*))))

(defun exit ()
  (setf *running?* nil))

(let ((window-open? nil))
  (defun try-open-window ()
    (unless window-open?
      (setf window-open? t)
      (cepl:repl)))

  (defun try-close-window ()
    (unless (or (cepl.lifecycle:shutting-down-p)
                (cepl.lifecycle:uninitialized-p)
                (not window-open?))
      (cepl:quit)
      (setf window-open? nil))))

(defun render-func ()
  (livesupport:update-repl-link)
  (step-host))

(defun now ()
  (float (/ (get-internal-real-time) internal-time-units-per-second)))
