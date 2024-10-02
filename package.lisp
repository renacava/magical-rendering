;;;; package.lisp

(defpackage #:magical-rendering
  (:use #:cl #:cepl #:cepl.sdl2 #:rtg-math)
  (:export
   #:main
   #:exit
   #:set-idle-func
   #:set-bg-colour
   #:load-sound
   #:play-sound
   #:load-song
   #:play-song
   #:stop-song
   #:*mouse*
   #:set-render-fps-cap
   #:set-input-fps-cap
   #:set-idle-fps-cap
   #:mouse-bind-event
   #:mouse-unbind-event
   #:keyboard-bind-event
   #:keyboard-unbind-event))
