(in-package #:magical-rendering)

(defparameter ortho-matrix nil)
(defparameter origin-matrix nil)
(defparameter current-screen-size (vec2 1f0 1f0))
(defparameter *all-texture-objects* (make-hash-table :test #'eq))
(defparameter *paths-texture2d-table* (make-hash-table :test #'equal))
(defparameter *paths-sampler2d-table* (make-hash-table :test #'equal))

(defclass texture-object ()
  ((path :initarg :path
         :initform nil
         :accessor path)
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

(defun texture2d-at-path (path)
  (when path
    (multiple-value-bind (result found?) (gethash path *paths-texture2d-table*)
      (if found?
          result
          (let ((path-probe (probe-file path)))
            (when path-probe
              (setf (gethash path *paths-texture2d-table*)
                    (make-texture (load-image-data path-probe) :element-type :uint8-vec4))))))))


(defun sampler2d-at-path (path)
  (when path
    (multiple-value-bind (result found?) (gethash path *paths-sampler2d-table*)
      (if found?
          result
          (let ((texture2d (texture2d-at-path path)))
            (when texture2d
              (setf (gethash path *paths-sampler2d-table*)
                    (sample texture2d))))))))

(defun texture-make (path &key width height (loc (vec2 0.0 0.0)) (rot 0.0) (scale 1.0) (z-order 0.0) (visible t))
  (let* ((texture-obj (make-instance 'texture-object
                                     :path path
                                     :loc loc
                                     :rot (lambda () (float (resolve rot)))
                                     :scale (lambda () (float (resolve scale)))
                                     :visible visible
                                     :z-order z-order))
         (loaded-texture (texture2d-at-path path)))
    (let* ((dimensions (or (ignore-errors (texture-base-dimensions loaded-texture))
                           (list 100 100))))
      (setf (width texture-obj) (float (or width (first dimensions)))
            (height texture-obj) (float (or height (second dimensions))))
      (setf (gethash texture-obj *all-texture-objects*) texture-obj)
      texture-obj)))

(defun try-free (x)
  (ignore-errors (free x)))

(defun free-hashtable (table)
  (maphash (lambda (key value)
             (try-free value))
           table)
  (clrhash table))

(defun free-loaded-texture-data ()
  (free-hashtable *paths-sampler2d-table*)
  (free-hashtable *paths-texture2d-table*))

(defun destroy-all-texture-objects ()
  (maphash (lambda (key texture-object)
             (setf texture-object nil))
           *all-texture-objects*)
  (clrhash *all-texture-objects*))

(defun setup-ortho-matrix ()
  (setf current-screen-size (cepl:surface-resolution (cepl:current-surface))
        (resolution (current-viewport)) current-screen-size 
        ortho-matrix (rtg-math.projection:orthographic-v2 current-screen-size 0.001 100.0)))

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
             )
    (map-g #'texture-pipeline (texture-default-verts-stream)
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
           :sampler-2d (sampler2d-at-path (resolve (path texture-object))))))

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

(defun texture-resize (texture-object &key width height)
  (when texture-object
    (when width
      (setf (width texture-object) width))
    (when height
      (setf (height texture-object) height))))

(defun texture-translate (texture-object xy)
  (when texture-object
    (setf (loc texture-object) xy)
    texture-object))

(defun texture-z (texture-object z-order)
  (when texture-object
    (setf (z-order texture-object) z-order)
    texture-object))

(defun texture-origin (texture-object &key x-origin y-origin)
  (when texture-object
    (when x-origin
      (setf (x-origin texture-object) x-origin))
    (when y-origin
      (setf (y-origin texture-object) y-origin))))

(defun texture-rotate (texture-object rotation-in-degrees)
  (when texture-object
    (setf (rot texture-object) rotation-in-degrees)
    texture-object))

(defun texture-scale (texture-object scale)
  (when texture-object
    (setf (scale texture-object) scale)
    texture-object))

(defun texture-change (texture-object image-path)
  (when texture-object
    (setf (path texture-object) image-path)
    texture-object))

(defun texture-properties (texture-object)
  (list :translation (loc texture-object)
        :rotation (rot texture-object)
        :scale (scale texture-object)
        :path (path texture-object)
        :width (width texture-object)
        :height (height texture-object)
        :z (z-order texture-object)
        :visible (visible texture-object)))

(defun-g offset-texture-vert-by-dimensions ((vert :vec3) (width :float) (height :float))
  (let* ((vert-index (int (aref vert 2)))
         (offset (vec2 0.0 0.0)))
    (if (= vert-index 1)
        (setf offset (vec2 width 0.0))
        (if (= vert-index 2)
            (setf offset (vec2 width height))
            (if (= vert-index 3) (setf offset (vec2 0.0 height)))))
    (vec3 offset 0.0)))

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

(defun-g texture-frag-stage ((pos :vec4) (uv :vec2) &uniform (sampler-2d :sampler-2d))
  (values (texture sampler-2d uv)))

(defpipeline-g texture-pipeline ()
  (texture-vert-stage :vec3 :vec2)
  (texture-frag-stage :vec4 :vec2))

(let (verts-stream)
  (defun texture-default-verts-stream ()
    (or verts-stream
        (setf verts-stream (make-buffer-stream (make-gpu-array (list
                                                                (list (vec3 0.0 0.0 0.0) (vec2 0.0 1.0))
                                                                (list (vec3 1.0 0.0 1.0) (vec2 1.0 1.0))
                                                                (list (vec3 1.0 1.0 2.0) (vec2 1.0 0.0))
                                                                (list (vec3 0.0 1.0 3.0) (vec2 0.0 0.0)))
                                                               :element-type 'g-pt)
                                               :index-array (make-gpu-array (list 0 1 2 0 2 3) :element-type :uint))))))

