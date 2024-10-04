(in-package #:magical-rendering)

;;(defparameter *running?* nil)
;; (defparameter *main-thread-name* "magical-rendering-main-thread")
;; (defparameter *main-thread* nil)
(defparameter *idle-func* nil)
(defparameter *idle-fps* 180)
(defparameter *render-fps* 180)
(defparameter *input-fps* 180)
(defparameter *bg-colour* `(0 0 0))


(defun main (&key (window-name "Magical Window")
                  (width 800)
                  (height 600)
                  (idle-func nil)
                  (post-startup-func nil))
  (set-idle-func idle-func)
  (init window-name width height)
  (call-if-func post-startup-func)
  (loop
    (livesupport:continuable
      (livesupport:update-repl-link)
      (when (cepl.lifecycle:uninitialized-p)
        (return))
      (update-inputs)
      (render)
      (idle))))

(defun exit ()
  (unless (or (cepl.lifecycle:shutting-down-p)
              (cepl.lifecycle:uninitialized-p))
    (shutdown-audio)
    (free-loaded-texture-data)
    (destroy-all-texture-objects)
    (cepl:quit)))

(defun init (window-name width height)
  (cepl:repl width height)
  (gl:enable :depth-test)
  (setf (cepl:depth-test-function) #'<)
  (set-vsync-enabled nil)
  (setf (cepl:surface-title (cepl:current-surface)) (format nil "~a" window-name))
  (defparameter *blending-params* (make-blending-params))
  (try-init-audio)
  (setup-keyboard))

(defun set-vsync-enabled (bool)
  (setf (cepl.sdl2::vsync) bool))

(defun toggle-vsync ()
  (set-vsync-enabled (not (vsync-enabled?))))

(defun vsync-enabled? ()
  (cepl.sdl2::vsync))

(defun-fps-limited *render-fps* render ()
  (update-clear-colour)
  (cepl:clear)
  (setup-ortho-matrix)
  (render-all-textures)
  (cepl:swap)
  (step-host))

(defun-fps-limited *idle-fps* idle ()
  (call-if-func *idle-func*))

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
