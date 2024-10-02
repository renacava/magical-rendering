(in-package #:magical-rendering)

(defparameter *keyboard-last-event* (list :down nil
                                          :keyboard nil
                                          :button-index 0
                                          :button-id nil
                                          :timestamp 0))
(defparameter *keyboard-listener* nil)
(defparameter *keyboard-callbacks* (make-hash-table :test #'equal))
(defparameter *pressed-keys* nil)
(defparameter *down-keys* nil)
(defparameter *released-keys* nil)
(defparameter *keyboard-button-pressed-events* (make-hash-table :test #'equal))
(defparameter *keyboard-button-released-events* (make-hash-table :test #'equal))
(defparameter *keyboard-button-down-events* (make-hash-table :test #'equal))

(defun keyboard-call-events ()
  (keyboard-call-events-in-table *pressed-keys* *keyboard-button-pressed-events*) 
  (keyboard-call-events-in-table *down-keys* *keyboard-button-down-events*)
  (keyboard-call-events-in-table *released-keys* *keyboard-button-released-events*)
  (decay-keyboard-inputs))

(defun decay-keyboard-inputs ()
  (setf *pressed-keys* nil
        *released-keys* nil))

(defun on-key-event (event)
  (setf *keyboard-last-event* event)
  (update-pressed-keys (resolve-keyboard-event-key event) (getf event :button-id))
  (keyboard-call-callbacks event))

(defun update-pressed-keys (event-key button-id)
  (let ((already-down? (find button-id *down-keys*))
        (already-pressed? (find button-id *pressed-keys*))
        (already-released? (find button-id *released-keys*)))
    (case event-key
      (:pressed (progn
                  (unless (or already-down? already-pressed?)
                    (push button-id *pressed-keys*))
                  (unless already-down?
                    (push button-id *down-keys*))))
      (:released (progn
                   (setf *down-keys* (remove button-id *down-keys*))
                   (unless already-released?
                     (push button-id *released-keys*)))))))

(defun resolve-keyboard-event-key (event)
  (let ((down? (getf event :down))
        (button-id (getf event :button-id))
        (event-key nil))
    (if down?
        (if (find button-id *down-keys*)
            (setf event-key :down)
            (setf event-key :pressed))
        (setf event-key :released))
    event-key))

(defun resolve-keyboard-binding-table (event-key)
  (case event-key
    (:pressed *keyboard-button-pressed-events*)
    (:released *keyboard-button-released-events*)
    (:down *keyboard-button-down-events*)))

(defun keyboard-bind-event (event-key button-id func)
  (let ((table (resolve-keyboard-binding-table event-key)))
    (when table
      (setf (gethash button-id table) func))))

(defun keyboard-unbind-event (event-key button-id)
  (remhash button-id (resolve-keyboard-binding-table event-key)))

(defun keyboard-call-events-in-table (button-ids table)
  (loop for button-id in button-ids
        do (multiple-value-bind (result found?) (gethash button-id table)
             (when (functionp result) (funcall result)))))

(defun keyboard-call-callbacks (event)
  (maphash (lambda (callback-name callback-func)
             (funcall callback-func event))
           *keyboard-callbacks*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plumbing
(defun skitter-keyboard-callback (data input-source index timestamp tpref)
  (on-key-event (list :down data
                      :keyboard input-source
                      :button-index index
                      :button-id (key-index-to-id index)
                      :timestamp timestamp)))

(defun key-index-to-id (index)
  (ignore-errors (elt skitter.sdl2::*key-button-names* index)))

(defun setup-keyboard ()
  (when *keyboard-listener*
    (cepl.skitter.sdl2:stop-listening *keyboard-listener*))
  (setf *keyboard-listener*
        (cepl.skitter.sdl2:listen-to (cepl.skitter.sdl2:make-event-listener #'skitter-keyboard-callback)
                                (cepl.skitter.sdl2:keyboard)
                                :button)))

(defun keyboard-bind-callback (name callback-func)
  (setf (gethash name *keyboard-callbacks*) callback-func))

(defun keyboard-unbind-callback (name)
  (remhash name *keyboard-callbacks*))
