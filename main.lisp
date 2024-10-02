(in-package #:magical-rendering)

(defparameter *running?* nil)
(defparameter *main-thread-name* "magical-rendering-main-thread")
(defparameter *main-thread* nil)
(defparameter *idle-func* nil)
(defparameter *idle-fps* 60)
(defparameter *render-fps* 60)
(defparameter *input-fps* 60)
(defparameter *bg-colour* `(0 0 0))

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
                     (update-inputs)
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
      (setf (cepl.sdl2::vsync) t)
      (setf (cepl:surface-title (cepl:current-surface)) (format nil "~a" window-name))
      (try-init-audio)
      (setup-keyboard)))

  (defun try-close-window ()
    (unless (or (cepl.lifecycle:shutting-down-p)
                (cepl.lifecycle:uninitialized-p)
                (not window-open?))
      (shutdown-audio)
      (cepl:quit)
      (setf window-open? nil))))

(defun-fps-limited *render-fps* render ()
  (update-clear-colour)
  (cepl:clear)
  (cepl:swap)
  (livesupport:update-repl-link)
  (step-host))

(defun-fps-limited *idle-fps* idle ()
  (when (functionp *idle-func*)
    (funcall *idle-func*)))

(defun-fps-limited *input-fps* update-inputs ()
  (keyboard-call-events)
  (mouse-update))

(defun set-idle-func (func)
  (setf *idle-func* func))

(defun set-bg-colour (rgb)
  (setf *bg-colour* rgb))

(defun update-clear-colour ()
  (setf (cepl:clear-color) (rtg-math:v4! (first *bg-colour*)
                                         (second *bg-colour*)
                                         (third *bg-colour*)
                                         1)))

(defun valid-fps? (amount)
  (when (and (numberp amount)
             (> amount 0))
    amount))

(defun set-render-fps-cap (amount)
  (setf *render-fps* (or (valid-fps? amount) *render-fps*)))

(defun set-idle-fps-cap (amount)
  (setf *idle-fps* (or (valid-fps? amount) *idle-fps*)))

(defun set-input-fps-cap (amount)
  (setf *input-fps* (or (valid-fps? amount) *input-fps*)))
