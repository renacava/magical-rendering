;;;; package.lisp

(defpackage #:magical-rendering
  (:use #:cl #:cepl #:cepl.sdl2 #:rtg-math)
  (:export
   #:main
   #:exit
   #:load-sound
   #:play-sound))
