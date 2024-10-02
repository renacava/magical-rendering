(in-package #:magical-rendering)

(defparameter *audio-initialised?* nil)
(defparameter *loaded-sounds* (make-hash-table :test #'equal))

(defun init-audio ()
  (when (and *running?*
             (not *audio-initialised?*))
    (setf *audio-initialised?* t)
    (sdl2-mixer:init)
    (sdl2-mixer:open-audio 22050 :s16sys 1 1024)
    (sdl2-mixer:allocate-channels 1)))

(defun shutdown-audio ()
  (sdl2-mixer:halt-channel -1)
  (sdl2-mixer:close-audio)
  (maphash (lambda (key value)
             (when value
               (sdl2-mixer:free-chunk value)))
           *loaded-sounds*)
  (sdl2-mixer:quit))

(defun load-sound (filename)
  (or (gethash filename *loaded-sounds*)
      (when (probe-file filename)
        (let ((sound (ignore-errors (sdl2-mixer:load-wav filename))))
          (when sound
            (setf (gethash filename *loaded-sounds*) sound))))))

(defun play-sound (filename)
  (when filename
    (ignore-errors
     (sdl2-mixer:play-channel 0 (load-sound filename) 0))))

;; (defun load-bgm (filename))
