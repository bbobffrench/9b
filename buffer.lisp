;;;; buffer.lisp

(defconstant gap-buffer-initial-len 110)
(defconstant gap-buffer-expansion-len 40)

(defmacro struct-bind ((&rest slots) var &body body)
  `(let ,(mapcar
           (lambda (slot)
             (let* ((type-str `(symbol-name (type-of ,var)))
                    (slot-str (symbol-name slot))
                    (func-str `(concatenate 'string ,type-str "-" ,slot-str)))
               `(,slot (funcall (intern ,func-str) ,var))))
           slots)
     ,@body))

(defstruct (gap-buffer (:print-function print-gap-buffer))
  (gap-pos 0 :type unsigned-byte)
  (gap-len gap-buffer-initial-len :type unsigned-byte)
  (data (make-array gap-buffer-initial-len :element-type 'character)
   :type (simple-array character (*))))

(defun print-gap-buffer (buff stream depth)
  (declare (type gap-buffer buff) (type stream stream) (ignore depth))
  (struct-bind (gap-pos gap-len data) buff
    (let ((front (subseq data 0 gap-pos))
          (back (subseq data (+ gap-pos gap-len))))
      (format stream "<~s ~d ~s>~%" front gap-len back))))

(defun gap-buffer-str (buff)
  (declare (type gap-buffer buff))
  (struct-bind (gap-pos gap-len data) buff
    (concatenate 'string (subseq data 0 gap-pos) (subseq data (+ gap-pos gap-len)))))

(defun line-len (buff)
  (declare (type gap-buffer buff))
  (- (length (gap-buffer-data buff)) (gap-buffer-gap-len buff)))

(defun gap-buffer-expand (buff &optional (expansion-len gap-buffer-expansion-len))
  (declare (type gap-buffer buff) (type unsigned-byte expansion-len))
  (struct-bind (gap-pos gap-len data) buff
    (let ((new-data (make-array (+ expansion-len (length data)) :element-type 'character)))
      (setf
        ;; Copy over data before the gap
        (subseq new-data 0 gap-pos) (subseq data 0 gap-pos)

        ;; Copy over data after the gap
        (subseq new-data (+ gap-pos gap-len expansion-len))
        (subseq data (+ gap-pos gap-len))

        ;; Replace the old buffer
        (gap-buffer-data buff) new-data

        ;; Set the new gap size
        (gap-buffer-gap-len buff) (+ gap-len expansion-len))))
  buff)

(defun gap-buffer-move (buff pos)
  (declare (type gap-buffer buff) (type unsigned-byte pos))
  (struct-bind (gap-pos gap-len data) buff
    ;; Copy displaced data, taking into account the direction of movement
    (if (> pos gap-pos)
        (setf (subseq data gap-pos pos)
              (subseq data (+ gap-pos gap-len) (+ gap-len pos)))
        (setf (subseq data (- (+ gap-pos gap-len) (- gap-pos pos)) (+ gap-pos gap-len))
              (subseq data pos gap-pos))))
  (setf (gap-buffer-gap-pos buff) pos)
  buff)

(defun gap-buffer-insert (buff str pos)
  (declare (type gap-buffer buff) (type string str) (type unsigned-byte pos))
  (struct-bind (gap-len) buff
    (if (> pos (line-len buff))
        (error "Invalid column ~d for line of length ~d" pos (line-len buff)))

    ;; Expand the buffer if necessary to fit the inserted string
    (when (>= (length str) gap-len)
      (if (> (length str) gap-buffer-expansion-len)
          (gap-buffer-expand buff)
          (gap-buffer-expand buff (+ (length str) gap-buffer-expansion-len)))))

  ;; Move the gap to the specified position
  (gap-buffer-move buff pos)

  ;; Rebind struct slots in case they were changed during expansion
  (struct-bind (gap-pos gap-len data) buff
    ;; Move the gap to the specified position and insert the string
    (setf (subseq data gap-pos) str
          (gap-buffer-gap-pos buff) (+ gap-pos (length str))
          (gap-buffer-gap-len buff) (- gap-len (length str))))
  buff)

(defun gap-buffer-delete (buff start end)
  (declare (type gap-buffer buff) (type unsigned-byte start end))
  (if (or (> start end)
          (> start (line-len buff))
          (> end (line-len buff)))
      (error "Invalid indices [~d, ~d] for line of length ~d~%" start end (line-len buff)))
  (incf (gap-buffer-gap-len buff) (- end start))
  (struct-bind (gap-pos) buff
    (if (< start gap-pos)
        (if (< end gap-pos)
            (decf (gap-buffer-gap-pos buff) (- end start))
            (decf (gap-buffer-gap-pos buff) (- gap-pos start)))))
  buff)

(defstruct (buffer (:print-function print-buffer))
  (data (make-gap-buffer) :type gap-buffer)
  (line-num 1 :type (integer 1 *))
  (next nil :type (or null buffer))
  (prev nil :type (or null buffer)))

(defun buffer-head (buff)
  (declare (type (or buffer null) buff))
  (if (buffer-prev buff)
      (buffer-head (buffer-prev buff))
      buff))

(defun buffer-line-count (buff)
  (declare (type (or buffer null) buff))
  (if buff
      (+ (buffer-line-num buff) (buffer-line-count (buffer-next buff)))
      0))

(defun print-buffer (buff stream depth)
  (declare (type buffer buff) (type stream stream) (ignore depth))
  (labels ((print-lines (line)
             (declare (type (or buffer null) line))
             (if line
                 (let ((line-num (buffer-line-num line))
                       (line-str (gap-buffer-str (buffer-data line))))
                   (format stream "~d~c~a~%" line-num #\tab line-str)
                   (print-lines (buffer-next line))))))
    (format stream "--- ~d line buffer ---~%" (buffer-line-count buff))
    (print-lines (buffer-head buff))))

(defun buffer-seek-line (buff pos)
  (declare (type (or buffer null) buff) (type unsigned-byte pos))
  (if buff
      (cond ((= pos (buffer-line-num buff)) buff)
            ((> pos (buffer-line-num buff))
             (buffer-seek-line (buffer-next buff) pos))
            ((< pos (buffer-line-num buff))
             (buffer-seek-line (buffer-prev buff) pos))
            (t (error "Invalid line number ~d for ~d line buffer~%"
                      pos
                      (buffer-line-count buff))))))

(defun buffer-insert-line (buff)
  (declare (type buffer buff))
  (let ((prev (buffer-prev buff)) (new (make-buffer)))
    (if prev (setf (buffer-next prev) new))
    (setf (buffer-prev new) prev
          (buffer-next new) buff
          (buffer-prev buff) new)
    new))

(defun buffer-append-line (buff)
  (declare (type buffer buff))
  (let ((next (buffer-next buff)) (new (make-buffer)))
    (if next (setf (buffer-prev next) new))
    (setf (buffer-prev new) buff
          (buffer-next new) next
          (buffer-next buff) new)
    new))

(defun buffer-reindex-lines (buff initial)
  (declare (type (or buffer null) buff) (type unsigned-byte initial))
  (when buff
    (setf (buffer-line-num buff) initial)
    (buffer-reindex-lines (buffer-next buff) (1+ initial))))

(defun buffer-insert-lines (buff pos count)
  (declare (type buffer buff) (type unsigned-byte pos count))
  (let ((buff (buffer-seek-line buff pos)))
    (labels ((insert-n-lines (buff n)
               (declare (type buffer buff) (type unsigned-byte n))
               (if (zerop n)
                   buff
                   (insert-n-lines (buffer-insert-line buff) (1- n)))))
      (buffer-reindex-lines (insert-n-lines buff count) pos)))
  buff)

(defun buffer-append-lines (buff pos count)
  (declare (type buffer buff) (type unsigned-byte pos count))
  (let ((buff (buffer-seek-line buff pos)))
    (labels ((append-n-lines (buff n)
               (declare (type buffer buff) (type unsigned-byte n))
               (if (zerop n)
                    buff
                   (append-n-lines (buffer-append-line buff) (1- n)))))
      (prog1 (append-n-lines buff count)
             (buffer-reindex-lines buff pos)))))

(defun buffer-delete-lines (buff start end)
  (declare (type buffer buff) (type unsigned-byte start end))
  (let ((preceding (buffer-prev (buffer-seek-line buff start)))
        (proceeding (buffer-next (buffer-seek-line buff end))))
    (if preceding (setf (buffer-next preceding) proceeding))
    (if proceeding (setf (buffer-prev proceeding) preceding))
    (cond (preceding (buffer-reindex-lines preceding (1- start)) preceding)
          (proceeding (buffer-reindex-lines proceeding 1) proceeding)
          (t (make-buffer)))))

(defun load-file-into-buffer (path)
  (declare (type string path))
  (with-open-file (stream path :if-does-not-exist nil)
    (if (null stream) (return-from load-file-into-buffer))
    (do ((line (read-line stream nil nil) (read-line stream nil nil))
         (buff (make-buffer) (buffer-append-lines buff (buffer-line-count buff) 1)))
        ((null line) buff)
      (gap-buffer-insert (buffer-data buff) line 0))))