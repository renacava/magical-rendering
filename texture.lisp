(in-package #:magical-rendering)

(defparameter rect-vert-indices (list 0 1 2 0 2 3))
(defparameter default-texture-object nil)
(defparameter ortho-matrix nil)
(defparameter origin-matrix nil)
(defparameter current-screen-size (vec2 1f0 1f0))
(defparameter *all-texture-objects* (make-hash-table :test #'eq))

(defclass texture-object ()
  ((verts :initarg :verts
          :initform nil
          :accessor verts)
   (indices :initarg :indices
            :initform (make-gpu-array rect-vert-indices :element-type :uint)
            :accessor indices)
   (buffer-stream :initarg :buffer-stream
                  :initform nil
                  :accessor buffer-stream)
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
   (visible :initarg :visible
            :initform t
            :accessor visible)
   (x-origin :initarg :x-origin
             :initform 0.5
             :accessor x-origin)
   (y-origin :initarg :y-origin
             :initform 0.5
             :accessor y-origin)))

(defun make-texture-verts (width height)
  ;; (let ((half-width (* 0.5 width))
  ;;       (half-height (* 0.5 height)))
  ;;   (list
  ;;    (list (vec3 (- half-width) (- half-height) 0.0) (vec2 0.0 0.0))
  ;;    (list (vec3 half-width     (- half-height) 0.0) (vec2 1.0 0.0))
  ;;    (list (vec3 half-width     half-height     0.0) (vec2 1.0 1.0))
  ;;    (list (vec3 (- half-width) half-height     0.0) (vec2 0.0 1.0)))
  ;;   )
  (list
   (list (vec3 0.0 0.0 0.0) (vec2 0.0 0.0))
   (list (vec3 width 0.0 0.0) (vec2 1.0 0.0))
   (list (vec3 width height 0.0) (vec2 1.0 1.0))
   (list (vec3 0.0 height 0.0) (vec2 0.0 1.0))))

(defun make-texture (&key (width 100.0) (height 100.0) (loc (vec2 0.0 0.0)) (rot 0.0) (scale 1.0) (visible t))
  (let* ((texture-obj (make-instance 'texture-object :width (float width)
                                                     :height (float height)
                                                     :loc loc
                                                     :rot (float rot)
                                                     :scale (float scale)
                                                     :visible visible
                                                     :verts (make-gpu-array (make-texture-verts width height) :element-type 'g-pt)))
         (verts-array (verts texture-obj))
         (index-array (indices texture-obj)))
    (setf (buffer-stream texture-obj) (make-buffer-stream verts-array :index-array index-array))
    (setf (gethash texture-obj *all-texture-objects*) texture-obj)
    texture-obj))

(defun free-all-textures ()
  (maphash (lambda (key texture-object)
             (free-texture texture-object))
           *all-texture-objects*))

(defun free-texture (texture-object)
  (when texture-object
    (ignore-errors (try-free-objects (buffer-stream texture-object)
                                     (indices texture-object)
                                     (verts texture-object)))
    (setf (buffer-stream texture-object) nil
          (indices texture-object) nil
          (verts texture-object) nil)
    (remhash texture-object *all-texture-objects*)))

(defun try-free (x)
  (when x
    (free x)))

(defun try-free-objects (&rest objects)
  (mapcar #'try-free objects))

(defun setup-ortho-matrix ()
  (let ((res (cepl:surface-resolution (cepl:current-surface))))
    (setf (resolution (current-viewport)) res 
          current-screen-size res)
    (let* ((res-vec2 res)
           (origin-offset-mat4 (rtg-math.matrix4:translation (vec3 (* -0.5 (aref res-vec2 0))
                                                                   (* -0.5 (aref res-vec2 1))
                                                                   0.0)))
           
           (ortho-mat4 (rtg-math.projection:orthographic-v2 res-vec2 0.001 100.0)))
      
      (setf ortho-matrix ortho-mat4
            origin-matrix origin-offset-mat4)))
  
  )

(defun texture-set-visible (texture-object visibility)
  (when texture-object
    (setf (visible texture-object) visibility)))

(defun texture-hide (texture-object)
  (texture-set-visible texture-object nil))

(defun texture-show (texture-object)
  (texture-set-visible texture-object t))

(defun texture-toggle-visible (texture-object)
  (when texture-object
    (setf (visible texture-object) (not (visible texture-object)))))

(defun render-all-textures ()
  (maphash (lambda (key texture-object)
             (render-texture-object texture-object))
           *all-texture-objects*))

(defun render-texture-object (texture-object)
  (when (and texture-object
             (resolve (visible texture-object))
             (buffer-stream texture-object))
    (map-g #'texture-pipeline (buffer-stream texture-object)
           :loc (resolve (loc texture-object))
           :rot (coerce (deg-to-rad (resolve (rot texture-object))) 'single-float) 
           :scale (float (resolve (scale texture-object)))
           :ortho-matrix ortho-matrix
           :screen-size current-screen-size
           :origin (vec2 (float (resolve (x-origin texture-object)))
                         (float (resolve (y-origin texture-object))))
           :width (resolve (width texture-object))
           :height (resolve (height texture-object)))))

(defun-g texture-vert-stage ((vert :vec3) (uv :vec2) &uniform
                             (ortho-matrix :mat4)
                             (screen-size :vec2)
                             (loc :vec2)
                             (rot :float)
                             (scale :float)
                             (origin :vec2)
                             (width :float)
                             (height :float))
  (let* ((rot-mat4 (rtg-math.matrix4:rotation-from-euler (vec3 0f0 0f0 rot)))
         (origin-offset (* origin (vec2 width height)))
         (vert (- vert (vec3 origin-offset 0.0)))
         (pos (vec4 vert 0.0))
         (pos (* pos rot-mat4))
         (pos (+ pos (vec4 loc 0.0 0.0)))
         (pos (* pos scale))
         (pos (vec4 (aref pos 0) (aref pos 1) -1.0 1.0))
         (pos (+ pos (vec4 (* screen-size -0.5) 0.0 0.0)))
         (pos (* ortho-matrix pos)))
    (values pos 
            (vec4 vert 1.0)
            uv)))

(defun-g texture-frag-stage ((pos :vec4) (uv :vec2))
  (values pos))

(defpipeline-g texture-pipeline ()
  (texture-vert-stage :vec3 :vec2)
  (texture-frag-stage :vec4 :vec2))
