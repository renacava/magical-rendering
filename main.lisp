(in-package #:magical-rendering)

(defparameter *running?* nil)
(defparameter *main-thread-name* "magical-rendering-main-thread")
(defparameter *main-thread* nil)
(defparameter *idle-func* nil)

(defun main (&key (window-name "Magical Window")
                  (width 800)
                  (height 600)
                  (idle-func nil))
  (setf *running?* t)
  (set-idle-func idle-func)
  (unless *main-thread*
    (setf *main-thread*
          (bt:make-thread
           (lambda ()
             (loop
               (if *running?*
                   (livesupport:continuable
                     (try-open-window window-name width height)
                     (idle)
                     (render))
                   (progn
                     (try-close-window)
                     (sleep 0.1)))))
           :name *main-thread-name*))))

(defun exit ()
  (setf *running?* nil))

(let ((window-open? nil))
  (defun try-open-window (window-name width height)
    (unless window-open?
      (setf window-open? t)
      (cepl:repl width height)
      (setf (cepl:surface-title (cepl:current-surface)) (format nil "~a" window-name))
      (try-init-audio)))

  (defun try-close-window ()
    (unless (or (cepl.lifecycle:shutting-down-p)
                (cepl.lifecycle:uninitialized-p)
                (not window-open?))
      (shutdown-audio)
      (cepl:quit)
      (setf window-open? nil))))

(defun render ()
  (livesupport:update-repl-link)
  (step-host))

(defun idle ()
  (when (functionp *idle-func*)
    (funcall *idle-func*)))

(defun set-idle-func (func)
  (setf *idle-func* func))

(defun now ()
  (float (/ (get-internal-real-time) internal-time-units-per-second)))
