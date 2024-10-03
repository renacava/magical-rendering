;;;; magical-rendering.asd

(asdf:defsystem #:magical-rendering
  :description ""
  :author "renacava"
  :license  "pending"
  :version "0.0.1"
  :serial t
  :depends-on (#:sdl2
               #:sdl2-mixer
               #:cepl
               #:cepl.sdl2
               #:cepl.skitter.sdl2
               #:nineveh
               #:rtg-math
               #:livesupport)
  :components ((:file "package")
               (:file "utilities")
               (:file "texture")
               (:file "time")
               (:file "audio")
               (:file "keyboard")
               (:file "mouse")
               (:file "main")))
