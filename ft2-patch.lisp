;;;; ft2-patch.lisp
;;;;
;;;; In the cl-freetype2 system, the element-type of the array created by bitmap-to-array is
;;;; 'unsigned-byte. The :data keyword in CLX's render-add-glyph expects an '(unsigned-byte 8) for
;;;; an alpha8 picture format. The function has been redefined here to return an array of the
;;;; expected element-type.

(in-package ft2)

(defun bitmap-to-array (bitmap)
  (let ((buffer (ft-bitmap-buffer bitmap))
        (rows (ft-bitmap-rows bitmap))
        (width (ft-bitmap-width bitmap))
        (pitch (ft-bitmap-pitch bitmap))
        (format (ft-bitmap-pixel-mode bitmap)))
    (let ((pixel-fn (ecase format
                      (:mono #'nth-mono-pixel)
                      (:gray #'nth-gray-pixel)
                      (:lcd #'nth-gray-pixel)
                      (:lcd-v #'nth-gray-pixel)))
          (array (make-array (list rows width) :element-type '(unsigned-byte 8))))
      (declare (function pixel-fn))
      (loop for i from 0 below rows
            as ptr = (inc-pointer buffer (* i pitch))
            do (loop for j from 0 below width
                     do (setf (aref array i j) (funcall pixel-fn ptr j)))
            finally (return (values array format))))))
