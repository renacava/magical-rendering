;;;; magical-rendering.asd

(asdf:defsystem #:magical-rendering
  :description ""
  :author "renacava"
  :license  "pending"
  :version "0.0.1"
  :serial t
  :depends-on (#:sdl2
               #:cepl
               #:cepl.sdl2
               #:rtg-math
               #:livesupport)
  :components ((:file "package")
               (:file "main")))
