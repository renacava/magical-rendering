(in-package #:magical-rendering)

(defparameter *mouse* nil)

(defun-fps-limited *input-fps* mouse-update ()
  (let* ((mouse (cepl.skitter.sdl2:mouse))
         (pos (mouse-pos)))
    (setf *mouse*
          (list :lmb (cepl.skitter.sdl2:mouse-button mouse 1)
                :rmb (cepl.skitter.sdl2:mouse-button mouse 3)
                :mmb (cepl.skitter.sdl2:mouse-button mouse 2)
                :x (first pos)
                :y (second pos)))))

(defun mouse-pos ()
  (let* ((pos (cepl.skitter.sdl2:mouse-pos (cepl.skitter.sdl2:mouse)))
         (window-dimensions (cepl:surface-dimensions (cepl:current-surface)))
         (pos (list (aref pos 0)
                    (- (second window-dimensions)
                       (aref pos 1)))))
    pos))
