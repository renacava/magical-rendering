(in-package #:magical-rendering)

(defparameter *mouse* (list :lmb nil
                            :rmb nil
                            :mmb nil
                            :x 0.0
                            :y 0.0
                            :x-delta 0.0
                            :y-delta 0.0))
(defparameter *previous-mouse* *mouse*)
(defparameter *mouse-lmb-clicked-bindings* (make-hash-table :test #'equal))
(defparameter *mouse-rmb-clicked-bindings* (make-hash-table :test #'equal))
(defparameter *mouse-mmb-clicked-bindings* (make-hash-table :test #'equal))
(defparameter *mouse-move-bindings* (make-hash-table :test #'equal))

(defun mouse-update ()
  (let* ((mouse (cepl.skitter.sdl2:mouse))
         (pos (mouse-pos)))
    (setf *mouse*
          (list :lmb (cepl.skitter.sdl2:mouse-button mouse 1)
                :rmb (cepl.skitter.sdl2:mouse-button mouse 3)
                :mmb (cepl.skitter.sdl2:mouse-button mouse 2)
                :x (first pos)
                :y (second pos)
                :x-delta (- (first pos) (getf *mouse* :x))
                :y-delta (- (second pos) (getf *mouse* :y))))
    (when (mouse-state-changed?)
      (mouse-call-events))
    (setf *previous-mouse* *mouse*)))

(defun mouse-state-changed? ()
  (not (equalp *mouse* *previous-mouse*)))

(defun mouse-pos ()
  (let* ((pos (cepl.skitter.sdl2:mouse-pos (cepl.skitter.sdl2:mouse)))
         (window-dimensions (cepl:surface-dimensions (cepl:current-surface)))
         (pos (list (aref pos 0)
                    (- (second window-dimensions)
                       (aref pos 1)))))
    pos))

(defun resolve-mouse-binding-table (event-key)
  (case event-key
    (:lmb *mouse-lmb-clicked-bindings*)
    (:rmb *mouse-rmb-clicked-bindings*)
    (:mmb *mouse-mmb-clicked-bindings*)
    (:move *mouse-move-bindings*)))

(defun mouse-bind-event (event-key event-name func)
  (let ((table (resolve-mouse-binding-table event-key)))
    (when table
      (setf (gethash event-name table) func))))

(defun mouse-unbind-event (event-key event-name)
  (remhash event-name (resolve-mouse-binding-table event-key)))

(defun mouse-call-table-events (table)
  (maphash (lambda (event-name func)
             (funcall func))
           table))

(defmacro on-mouse-just-pressed (mouse-button-key &body body)
  `(when (and (not (getf *previous-mouse* ,mouse-button-key))
              (getf *mouse* ,mouse-button-key))
     ,@body))

(defun mouse-call-events ()
  (on-mouse-just-pressed :lmb
    (mouse-call-table-events *mouse-lmb-clicked-bindings*))
  (on-mouse-just-pressed :rmb
    (mouse-call-table-events *mouse-rmb-clicked-bindings*))
  (on-mouse-just-pressed :mmb
    (mouse-call-table-events *mouse-mmb-clicked-bindings*))
  
  (when (and (not (= (getf *mouse* :x)
                     (getf *previous-mouse* :x)))
             (not (= (getf *mouse* :y)
                     (getf *previous-mouse* :y))))
    (mouse-call-table-events *mouse-move-bindings*)))
