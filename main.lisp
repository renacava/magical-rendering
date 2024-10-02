(in-package #:magical-rendering)

(defun main ()
  (cepl:repl)
  (loop (livesupport:continuable (funcall #'render-func))))

(defun render-func ()
  (livesupport:update-repl-link)
  (step-host)
  (sleep 0.0001)
)

(defun now ()
  (float (/ (get-internal-real-time) 1000)))

(defun debug-log (message &rest args)
  (apply #'format t message args)
  (finish-output))
