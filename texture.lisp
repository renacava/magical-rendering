(in-package #:magical-rendering)

(defparameter rect-vert-indices (list 0 1 2 0 2 3))
(defparameter default-texture-object nil)
(defparameter ortho-matrix nil)
(defparameter origin-matrix nil)
(defparameter current-screen-size (vec2 1f0 1f0))
(defparameter *all-texture-objects* (make-hash-table :test #'eq))
(defparameter *textures-at-paths* (make-hash-table :test #'equal))

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
   (path :initarg :path
         :initform nil
         :accessor path)
   (sampler :initarg :sampler
            :initform nil
            :accessor sampler)
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

(defun make-texture-verts (width height)
  (let ((width (float width))
        (height (float height)))
    (list
     (list (vec3 0.0 0.0 0.0) (vec2 0.0 1.0))
     (list (vec3 width 0.0 0.0) (vec2 1.0 1.0))
     (list (vec3 width height 0.0) (vec2 1.0 0.0))
     (list (vec3 0.0 height 0.0) (vec2 0.0 0.0)))))

(defun texture-make (path &key width height (loc (vec2 0.0 0.0)) (rot 0.0) (scale 1.0) (z-order 0.0) (visible t))
  (let* ((texture-obj (make-instance 'texture-object ;;:width (float (or width))
                                                     ;;:height (float (or ))
                                                     :path path
                                                     :loc loc
                                                     :rot (lambda () (float (resolve rot)))
                                                     :scale (lambda () (float (resolve scale)))
                                                     :visible visible
                                                     :z-order z-order
                                                     ;;:verts nil(make-gpu-array (make-texture-verts width height) :element-type 'g-pt)
                                                     ))
         
         ;;(verts-array (verts texture-obj))
         ;;(index-array (indices texture-obj))
         (cepl-texture (load-texture-at-path path)))
    (when cepl-texture
      (let* ((dimensions (texture-base-dimensions cepl-texture))
             (width (float (or width (first dimensions))))
             (height (float (or height (second dimensions))))
             (verts-array (make-gpu-array (make-texture-verts width height) :element-type 'g-pt))
             (index-array (indices texture-obj)))
        (setf (sampler texture-obj) (sample cepl-texture)
              (width texture-obj) width
              (height texture-obj) height
              (verts texture-obj) verts-array)
        (setf (buffer-stream texture-obj) (make-buffer-stream verts-array :index-array index-array))
        (setf (gethash texture-obj *all-texture-objects*) texture-obj)
        texture-obj)
)
))

(defun free-all-textures ()
  (maphash (lambda (key texture-object)
             (texture-free texture-object))
           *all-texture-objects*))

(defun texture-free (texture-object)
  (when texture-object
    (ignore-errors (try-free-objects (buffer-stream texture-object)
                                     (indices texture-object)
                                     (verts texture-object)
                                     (sampler texture-object)))
    (setf (buffer-stream texture-object) nil
          (indices texture-object) nil
          (verts texture-object) nil
          (sampler texture-object) nil)
    (remhash texture-object *all-texture-objects*)))

(defun free-all-textures-at-paths ()
  (maphash (lambda (path texture)
             (ignore-errors (try-free texture)))
           *textures-at-paths*)
  (clrhash *textures-at-paths*))

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
             (with-blending *blending-params*
               (render-texture-object texture-object)))
           *all-texture-objects*))

(defun render-texture-object (texture-object)
  (when (and texture-object
             (resolve (visible texture-object))
             (buffer-stream texture-object))
    (map-g #'texture-pipeline (buffer-stream texture-object)
           :loc (let ((loc (resolve (loc texture-object))))
                  (if (listp loc)
                      (vec2 (float (first loc)) (float (second loc)))
                      (if (arrayp loc)
                          loc
                          (vec2 0f0 0f0))))
           :z (float (resolve (z-order texture-object)))
           :rot (coerce (deg-to-rad (resolve (rot texture-object))) 'single-float) 
           :scale (float (resolve (scale texture-object)))
           :ortho-matrix ortho-matrix
           :screen-size current-screen-size
           :origin (vec2 (float (resolve (x-origin texture-object)))
                         (float (resolve (y-origin texture-object))))
           :width (float (resolve (width texture-object)))
           :height (float (resolve (height texture-object)))
           :sampler-2d (sampler texture-object))))

(defun-g texture-vert-stage ((vert :vec3) (uv :vec2) &uniform
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
         (origin-offset (* origin (vec2 width height)))
         ;;(vert (vec3 (aref vert 0) (aref vert 1) z))
         (vert (- vert (vec3 origin-offset 0.0)))
         (pos (vec4 vert 0.0))
         (pos (* pos scale))
         (pos (* pos rot-mat4))
         (pos (+ pos (vec4 loc 0.0 0.0)))

         (pos (vec4 (aref pos 0) (aref pos 1) -1.0 1.0))
         
         (pos (+ pos (vec4 (* screen-size -0.5) 0.0 0.0)))
         
         ;;(pos ())
         
         (pos (* ortho-matrix pos))
         (pos (- pos (vec4 0.0 0.0 (* z 0.01) 0.0))))
    (values pos 
            (vec4 vert 1.0)
            uv)))

(defun-g texture-frag-stage ((pos :vec4) (uv :vec2) &uniform (sampler-2d :sampler-2d))
  (values ;;pos
          (texture sampler-2d uv)
   ))

(defpipeline-g texture-pipeline ()
  (texture-vert-stage :vec3 :vec2)
  (texture-frag-stage :vec4 :vec2))

(defun load-texture-at-path (path &optional force-reload?)
  (unless path
    (return-from load-texture-at-path))
  (when force-reload?
    (try-free (gethash path *textures-at-paths*))
    (remhash path *textures-at-paths*))
  (multiple-value-bind (result found?) (gethash path *textures-at-paths*)
    (if found?
        result
        (let ((path-probe (probe-file path)))
          (when path-probe
            (setf (gethash path *textures-at-paths*)
                  (ignore-errors (make-texture (load-image-data path-probe) :element-type :uint8-vec4))))))))

(defun load-image-data (path)
  (flatten-image-array (opticl:convert-image-to-rgba (opticl:read-image-file path))))

(defun flatten-image-array (image-array)
  "Returns a flat vector based on the 3 dimensional image-array"
  (let ((dimensions (array-dimensions image-array)))
    (loop for row below (first dimensions)
          collect (loop for column below (second dimensions)
                       collect (make-array 4 :element-type `(unsigned-byte 8)
                                             :initial-contents (loop for pixel below (third dimensions)
                                                                     collect (aref image-array row column pixel)))))))
