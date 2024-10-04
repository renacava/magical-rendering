;;;; package.lisp

(defpackage #:magical-rendering
  (:use #:cl #:cepl #:cepl.sdl2 #:rtg-math #:nineveh #:vari)
  (:export   
   #:main
   #:exit
   #:set-idle-func
   #:set-bg-colour
   #:set-render-fps-cap
   #:set-input-fps-cap
   #:set-idle-fps-cap
   #:window-get-size
   #:timeslice
   
   ;; sound
   #:load-sound
   #:play-sound
   #:load-song
   #:play-song
   #:stop-song
   
   ;; input
   #:*mouse*
   #:mouse-pos
   #:mouse-bind-event
   #:mouse-unbind-event
   #:keyboard-bind-callback
   #:keyboard-unbind-callback
   #:keyboard-bind-event
   #:keyboard-unbind-event
   #:*pressed-keys*
   #:*down-keys*
   #:*released-keys*

   ;; textures
   #:texture-make
   #:texture-destroy
   #:texture-set-visible
   #:texture-hide
   #:texture-show
   #:texture-toggle-visible
   #:texture-size
   #:texture-translation
   #:texture-rotate
   #:texture-scale
   #:texture-path
   #:texture-z
   #:texture-origin
   #:texture-properties
   ))
