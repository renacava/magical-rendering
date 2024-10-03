(in-package #:magical-rendering)

(defparameter *audio-initialised?* nil)
(defparameter *n-sound-channels* 8)
(defparameter *loaded-sounds* (make-hash-table :test #'equal))
(defparameter *loaded-songs* (make-hash-table :test #'equal))

(defun try-init-audio ()
  (when (not *audio-initialised?*)
    (setf *audio-initialised?* t)
    (sdl2-mixer:init)
    (sdl2-mixer:open-audio 22050 :s16sys *n-sound-channels* 1024)
    (sdl2-mixer:allocate-channels *n-sound-channels*)
    t))

(defun shutdown-audio ()
  (unwind-protect
       (progn
         (sdl2-mixer:halt-channel -1)
         (sdl2-mixer:halt-music)
         (sdl2-mixer:close-audio)
         (maphash (lambda (key value)
                    (when value
                      (sdl2-mixer:free-chunk value)))
                  *loaded-sounds*)
         (maphash (lambda (key value)
                    (when value)
                    (sdl2-mixer:free-music value))
                  *loaded-songs*)
         (clrhash *loaded-sounds*)
         (clrhash *loaded-songs*))
    (sdl2-mixer:quit)
    (setf *audio-initialised?* nil)))

(let ((previous-channel 0))
  (defun get-oldest-sound-channel ()
    (setf previous-channel (mod (1+ previous-channel) *n-sound-channels*))))

(defun load-sound (filename)
  (when *audio-initialised?*
    (or (gethash filename *loaded-sounds*)
        (when (probe-file filename)
          (let ((sound (ignore-errors (sdl2-mixer:load-wav filename))))
            (when sound
              (setf (gethash filename *loaded-sounds*) sound)))))))

(defun play-sound (filename)
  (when filename
    (ignore-errors
     (sdl2-mixer:play-channel (get-oldest-sound-channel) (load-sound filename) 0))))

(defun load-song (filename)
  (when *audio-initialised?*
    (or (gethash filename *loaded-songs*)
        (when (probe-file filename)
          (let ((sound (ignore-errors (sdl2-mixer:load-music filename))))
            (when sound
              (setf (gethash filename *loaded-songs*) sound)))))))

(defun play-song (filename &optional (loop? t))
  (when filename
    (ignore-errors
     (sdl2-mixer:play-music (load-song filename) (if loop? -1 0)))))

(defun stop-song ()
  (sdl2-mixer:halt-music))

