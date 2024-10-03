(in-package #:magical-rendering)

(defparameter *queued-gl-commands* nil)

(defmacro in-gl (&body body)
  `(queue-gl-command ,@body))

(defmacro queue-gl-command (&body body)
  `(ntack (lambda () ,@body) *queued-gl-commands*))

(defun run-gl-commands ()
  (loop for command in *queued-gl-commands*
        do (funcall command)))
