(in-package #:magical-rendering)

(defun font-find (font-filename)
  (probe-file (asdf:system-relative-pathname :magical-rendering (format nil "fonts/~a" font-filename))))

(progn
  ;;(defparameter default-font-struct nil)
  (defparameter *strings-texture2d-table* (make-hash-table :test #'equalp))
  (defparameter *strings-sampler2d-table* (make-hash-table :test #'equalp))
  (defparameter *all-text-objects* (make-hash-table :test #'eq))
  (defparameter *default-font-filepath* (font-find "inconsolata-sugar-regular.ttf"))
  ;;(defparameter default-text-sampler nil)
 )
(defun text-init ()
  (cepl.sdl2-ttf:init-cepl-sdl2-ttf))

(defun texture2d-at-string (str &optional (quality 'low) (font-filepath *default-font-filepath*))
  (when str
    (multiple-value-bind (result found?) (gethash (list str quality font-filepath) *strings-texture2d-table*)
      (if found?
          result
          (setf (gethash (list str quality font-filepath) *strings-texture2d-table*)
                (texture2d-from-string str quality font-filepath))))))

(defun sampler2d-at-string (str &optional (quality 'low) (font-filepath *default-font-filepath*))
  (when str
    (multiple-value-bind (result found?) (gethash (list str quality font-filepath) *strings-sampler2d-table*)
      (if found?
          result
          (let ((texture2d (texture2d-at-string str quality font-filepath)))
            (when texture2d
              (setf (gethash (list str quality font-filepath) *strings-sampler2d-table*)
                    (sample texture2d))))))))

(defun texture2d-from-string (str &optional (quality 'low) (font-filepath *default-font-filepath*))
  (cepl.sdl2-ttf:with-font (font font-filepath (case quality
                                                            (low 48)
                                                            (medium 128)
                                                            (high 256)
                                                            (t 48))) 
    (text-to-tex (format nil "~a" (resolve str)) font)))

(defclass text-object ()
  ((text-string :initarg :text-string
                :initform ""
                :accessor text-string)
   (quality :initarg :quality
            :initform 'low
            :accessor quality)
   (font-filepath :initarg :font-filepath
                  :initform *default-font-filepath*
                  :accessor font-filepath)
   (width :initarg :width
          :initform 100.0
          :accessor width)
   (height :initarg :height
           :initform 100.0
           :accessor height)
   (loc :initarg :loc
        :initform (vec2 0f0 0f0)
        :accessor loc)
   (rot :initarg :rot
        :initform 0f0
        :accessor rot)
   (scale :initarg :scale
          :initform 1f0
          :accessor scale)
   (z-order :initarg :z-order
            :initform 0f0
            :accessor z-order)
   (visible :initarg :visible
            :initform t
            :accessor visible)
   (x-origin :initarg :x-origin
             :initform 0.5
             :accessor x-origin)
   (y-origin :initarg :y-origin
             :initform 0.5
             :accessor y-origin)))

(defun text-make (text-string &key width height (loc (vec2 0.0 0.0)) (rot 0.0) (scale 1.0) (z-order 0.0) (visible t) (x-origin 0.5) (y-origin 0.5) (quality 'low) (font-filepath *default-font-filepath*))
  (let* ((text-obj (make-instance 'text-object
                                  :text-string text-string
                                  :quality quality
                                  :font-filepath font-filepath
                                  :loc loc
                                  :rot (lambda () (float (resolve rot)))
                                  :scale (lambda () (float (resolve scale)))
                                  :x-origin x-origin
                                  :y-origin y-origin
                                  :visible visible
                                  :z-order z-order))
         (resolved-font-filepath (ignore-errors (probe-file (resolve font-filepath))))
         (resolved-font-filepath (or resolved-font-filepath
                                     (font-find resolved-font-filepath)
                                     *default-font-filepath*))
         (loaded-texture (texture2d-at-string (resolve text-string) (resolve quality) resolved-font-filepath)))
    (let* ((dimensions (or (ignore-errors (texture-base-dimensions loaded-texture))
                           (list 100 100))))
      (setf (width text-obj) (or width (first dimensions))
            (height text-obj) (or height (second dimensions)))
      (setf (gethash text-obj *all-text-objects*) text-obj)
      text-obj)))

(defun render-all-texts ()
  (maphash (lambda (key text-object)
             (with-blending *blending-params*
               (render-text-object text-object)))
           *all-text-objects*))

(defun render-text-object (text-object)
  (when (and text-object
             (resolve (visible text-object)))
    (let* ((resolved-font-filepath (resolve (font-filepath text-object)))
           (resolved-font-filepath (or (ignore-errors (probe-file resolved-font-filepath))
                                       (font-find resolved-font-filepath)
                                       *default-font-filepath*)))
      (map-g #'text-pipeline (texture-default-verts-stream)
             :loc (let ((loc (resolve (loc text-object))))
                    (if (listp loc)
                        (vec2 (float (first loc)) (float (second loc)))
                        (if (arrayp loc)
                            loc
                            (vec2 0f0 0f0))))
             :z (float (resolve (z-order text-object)))
             :rot (coerce (deg-to-rad (resolve (rot text-object))) 'single-float) 
             :scale (float (resolve (scale text-object)))
             :ortho-matrix ortho-matrix
             :screen-size *current-screen-size*
             :origin (vec2 (float (resolve (x-origin text-object)))
                           (float (resolve (y-origin text-object))))
             :width (float (window-width))     ;;(float (resolve (width text-object)))
             :height (/ (window-height) 4) ;;(float (resolve (height text-object)))
             :sampler-2d (sampler2d-at-string (resolve (text-string text-object)) (resolve (quality text-object)) resolved-font-filepath)))
    ))

(defun-g text-vert-stage ((vert :vec3) (uv :vec2) &uniform
                          (z :float)
                          (ortho-matrix :mat4)
                          (screen-size :vec2)
                          (loc :vec2)
                          (rot :float)
                          (scale :float)
                          (origin :vec2)
                          (width :float)
                          (height :float))
  (let* ((rot-mat4 (rtg-math.matrix4:rotation-from-euler (vec3 0f0 0f0 rot)))
         ;;(width (/ width 4.0))
         (origin-offset (* origin (vec2 width height)))
         (vert (offset-texture-vert-by-dimensions vert width height))
         (vert (- vert (vec3 origin-offset 0.0)))
         (pos (vec4 vert 0.0))
         (pos (* pos scale))
         (pos (* pos rot-mat4))
         (pos (+ pos (vec4 loc 0.0 0.0)))
         (pos (vec4 (aref pos 0) (aref pos 1) -1.0 1.0))
         (pos (+ pos (vec4 (* screen-size -0.5) 0.0 0.0)))
         (pos (* ortho-matrix pos))
         (pos (- pos (vec4 0.0 0.0 (* z 0.01) 0.0))))
    (values pos 
            (vec4 vert 1.0)
            uv)))

(defun-g text-frag-stage ((pos :vec4) (uv :vec2) &uniform (sampler-2d :sampler-2d))
  (values (texture sampler-2d uv)))

(defpipeline-g text-pipeline ()
  (text-vert-stage :vec3 :vec2)
  (text-frag-stage :vec4 :vec2))

(defun destroy-all-text-objects ()
  (maphash (lambda (key text-object)
             (setf text-object nil))
           *all-text-objects*)
  (clrhash *all-text-objects*))

(defun free-all-text-data ()
  (free-hashtable *strings-sampler2d-table*)
  (free-hashtable *strings-texture2d-table*))

(defun text-to-tex (text font &optional (color (v! 255 255 255 0)))
  (let* ((texture-surface (sdl2:convert-surface-format (sdl2-ttf:render-utf8-blended
                                                        font text
                                                        (round (x color)) (round (y color))
                                                        (round (z color)) (round (w color))) :argb8888))
         (width (sdl2:surface-width texture-surface))
         (height (sdl2:surface-height texture-surface)))
    (let* ((carr (make-c-array-from-pointer
                  (list width height)
                  :uint8-vec4
                  (sdl2:surface-pixels texture-surface)))
           (text-texture (cepl:make-texture carr)))
      (sdl2:free-surface texture-surface)
      text-texture)))


