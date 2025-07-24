;;;; buffer.lisp

(defpackage 9b/buffer
  (:use common-lisp 9b/utils)
  (:export make-buffer
           line-len
           buffer-insert
           read-file-into-buffer
           buffer-length
           buffer-subseq
           buffer-delete
           buffer-replace
           buffer-next-addr
           buffer-prev-addr))

(in-package 9b/buffer)

(defconstant initial-len 110)

(declaim (ftype function print-gap-buffer))
(defstruct (gap-buffer (:print-function print-gap-buffer))
  (data (make-array initial-len :element-type 'character))
  (gap-pos 0)
  (gap-len initial-len))

(defun print-gap-buffer (buff stream depth)
  (declare (ignore depth))
  (struct-bind (gap-pos gap-len data) buff
    (let ((front (subseq data 0 gap-pos))
          (back (subseq data (+ gap-pos gap-len))))
      (format stream "<GAP-BUFFER: ~s ~d ~s>~%" front gap-len back))))

(defgeneric line-len (obj))

(defmethod line-len ((obj gap-buffer))
  (- (length (gap-buffer-data obj)) (gap-buffer-gap-len obj)))

(defconstant expansion-len 40)

(defun gap-buffer-expand (buff)
  (struct-bind (gap-pos gap-len data) buff
    (let ((new-data (make-array (+ expansion-len (length data)) :element-type 'character)))
      (setf ;; Copy over data before the gap
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
  (struct-bind (gap-pos gap-len data) buff
    (if (= pos gap-pos) (return-from gap-buffer-move buff))
    ;; Copy displaced data, taking into account the direction of movement
    (if (> pos gap-pos)
        (setf (subseq data gap-pos pos)
              (subseq data (+ gap-pos gap-len) (+ gap-len pos)))
        (setf (subseq data (- (+ gap-pos gap-len) (- gap-pos pos)) (+ gap-pos gap-len))
              (subseq data pos gap-pos))))
  (setf (gap-buffer-gap-pos buff) pos)
  buff)

(defun gap-buffer-insert (buff char)
  ;; Expand the buffer if necessary
  (if (zerop (gap-buffer-gap-len buff))
      (gap-buffer-expand buff))

  ;; Write the character to the gap and update the gap characteristics
  (setf (elt (gap-buffer-data buff) (gap-buffer-gap-pos buff)) char)
  (incf (gap-buffer-gap-pos buff))
  (decf (gap-buffer-gap-len buff))
  buff)

(defun gap-buffer-subseq (buff start &optional (end (line-len buff)))
  (struct-bind (gap-pos gap-len data) buff
    (cond ((and (< start gap-pos) (< end gap-pos))
           (subseq data start end))
          ((< start gap-pos)
           (concatenate 'string
                        (subseq data start gap-pos)
                        (subseq data (+ gap-pos gap-len) (+ gap-len end))))
          (t (subseq data (+ gap-len start) (+ gap-len end))))))

(defun gap-buffer-delete (buff start &optional (end (line-len buff)))
  (gap-buffer-move buff start)
  (incf (gap-buffer-gap-len buff) (- end start))
  buff)


(declaim (ftype function print-dlist))
(defstruct (dlist (:print-function print-dlist))
  (data (make-gap-buffer))
  (next nil)
  (prev nil))

(defun print-dlist (dlist stream depth)
  (declare (ignore depth))
  (format stream "<DLIST")
  (if (dlist-prev dlist)
      (format stream "~% PREV: ~s" (gap-buffer-subseq (dlist-data (dlist-prev dlist)) 0)))
  (format stream "~% CURR: ~s" (gap-buffer-subseq (dlist-data dlist) 0))
  (if (dlist-next dlist)
      (format stream "~% NEXT: ~s" (gap-buffer-subseq (dlist-data (dlist-next dlist)) 0)))
  (format stream ">~%"))

(defmethod line-len ((obj dlist))
  (line-len (dlist-data obj)))

(defun dlist-head (dlist)
  (if (dlist-prev dlist)
      (dlist-head (dlist-prev dlist))
      dlist))

(defun dlist-tail (dlist)
  (if (dlist-next dlist)
      (multiple-value-bind (tail depth) (dlist-tail (dlist-next dlist))
        (values tail (1+ depth)))
      (values dlist 0)))

(defun dlist-append (dlist)
  (let ((new (make-dlist)))
    (if (dlist-next dlist)
        (setf (dlist-prev (dlist-next dlist)) new))
    (setf (dlist-prev new) dlist
          (dlist-next new) (dlist-next dlist)
          (dlist-next dlist) new)
    new))

(defun dlist-delete (dlist)
  (let ((next (dlist-next dlist)) (prev (dlist-prev dlist)))
    (if prev (setf (dlist-next prev) next))
    (if next (setf (dlist-prev next) prev))
    next))


(declaim (ftype function print-buffer))
(defstruct (buffer (:print-function print-buffer))
  (curr-line (make-dlist))
  (line-num 1))

(defun print-buffer (buff stream depth)
  (declare (ignore depth))
  (labels ((print-lines (line line-num)
             (if line
                 (let ((str (gap-buffer-subseq (dlist-data line) 0)))
                   (format stream "~d~c~a~%" line-num #\tab str)
                   (print-lines (dlist-next line) (1+ line-num))))))
    (print-lines (dlist-head (buffer-curr-line buff)) 1)))

(defmethod line-len ((obj buffer))
  (line-len (dlist-data (buffer-curr-line obj))))

(defun buffer-seek (buff line)
  (labels ((jump-lines (dlist n)
             (if (zerop n)
                 dlist
                 (if (> n 0)
                     (jump-lines (dlist-next dlist) (1- n))
                     (jump-lines (dlist-prev dlist) (1+ n))))))
    (setf (buffer-curr-line buff)
          (jump-lines (buffer-curr-line buff) (- line (buffer-line-num buff)))
          (buffer-line-num buff) line)
    buff))

(defun buffer-insert-from-stream (buff addr stream)
  (let ((char (read-char stream nil)))
    (if (null char)
        buff
        (let* ((buff (buffer-seek buff (car addr)))
               (line (gap-buffer-move (dlist-data (buffer-curr-line buff)) (cdr addr))))
          (if (char= char #\newline)
              ;; Store the portion of the line that will be deleted, then delete it
              (let ((deleted (gap-buffer-subseq line (gap-buffer-gap-pos line))))
                (gap-buffer-delete line (gap-buffer-gap-pos line))
                ;; Re-insert the deleted portion on the next line
                (with-input-from-string (stream deleted)
                  ;; Add a new line to the buffer
                  (incf (buffer-line-num buff))
                  (setf (buffer-curr-line buff)
                        (dlist-append (buffer-curr-line buff)))
                  ;; Insert the deleted portion of the previous line into this new line
                  (buffer-insert-from-stream buff (cons (1+ (car addr)) 0) stream))
                ;; Continue on with the rest of the string
                (buffer-insert-from-stream buff (cons (1+ (car addr)) 0) stream))
              (progn
                (gap-buffer-insert line char)
                (buffer-insert-from-stream buff (cons (car addr) (1+ (cdr addr))) stream)))))))

(defun buffer-insert (buff addr str)
  (with-input-from-string (stream str)
    (buffer-insert-from-stream buff addr stream)))

(defun read-file-into-buffer (path)
  (with-open-file (file path)
    (buffer-insert-from-stream (make-buffer) '(1 . 0) file)))

(defun buffer-length (buff)
  (multiple-value-bind (line depth) (dlist-tail (buffer-curr-line buff))
    (values (+ (buffer-line-num buff) depth)
            (line-len line)))) ; Return the length of the last line as a value

(defun buffer-subseq (buff start-addr end-addr)
  (let* ((start-line (car start-addr)) (start-char (cdr start-addr))
         (end-line (car end-addr)) (end-char (cdr end-addr))
         (buff (buffer-seek buff start-line)))
    (with-output-to-string (stream)
      (labels ((add-lines (line line-num)
                 (let ((buff (dlist-data line)))
                   (cond (; In case the start and end characters are on the same line
                          (= start-line end-line) ; Single-line edge case
                          (format stream "~a" (gap-buffer-subseq buff start-char end-char)))

                         ;; For the first line, add everything after the start character
                         ((= line-num start-line)
                          (format stream "~a~%" (gap-buffer-subseq buff start-char))
                          (add-lines (dlist-next line) (1+ line-num)))

                         ;; For the last line, add until the end character and exit
                         ((= line-num end-line)
                          (format stream "~a" (gap-buffer-subseq buff 0 end-char)))

                         ;; Otherwise, add the entire line
                         (t (format stream "~a~%" (gap-buffer-subseq buff 0))
                            (add-lines (dlist-next line) (1+ line-num)))))))
      (add-lines (buffer-curr-line buff) (buffer-line-num buff))))))

(defun buffer-delete (buff start-addr end-addr)
  (let* ((start-line (car start-addr)) (start-char (cdr start-addr))
         (end-line (car end-addr)) (end-char (cdr end-addr))
         (buff (buffer-seek buff start-line)))
    (labels ((delete-n-lines (line n)
               (if (= n 1)
                   (prog1 (dlist-data line)
                          (dlist-delete line))
                   (delete-n-lines (dlist-delete line) (1- n)))))
      (let ((first-line (dlist-data (buffer-curr-line buff))))
        (if (= start-line end-line)
            ;; In case the deletion only occurs on a single line
            (progn (gap-buffer-delete first-line start-char end-char)
                   buff)
            ;; Delete all but the first line, storing the last one that has been deleted
            (let ((last-line (delete-n-lines (dlist-next (buffer-curr-line buff))
                                             (- end-line start-line))))
              ;; Delete the necessary portion of the first line
              (gap-buffer-delete first-line start-char)

              ;; Merge the remaining portion of the last line with the first
              (gap-buffer-move first-line (line-len first-line))
              (map nil
                   (lambda (char) (gap-buffer-insert first-line char))
                   (gap-buffer-subseq last-line end-char))
              buff))))))

(defun buffer-replace (buff start-addr end-addr str)
  (buffer-insert (buffer-delete buff start-addr end-addr) start-addr str))

(defun buffer-extract (buff addr)
  (let ((line (dlist-data (buffer-curr-line (buffer-seek buff (car addr))))))
    (if (= (cdr addr) (line-len line))
        #\newline
        (char (gap-buffer-subseq line (cdr addr) (1+ (cdr addr))) 0))))

(defun buffer-next-addr (buff addr)
  (let ((line (buffer-curr-line (buffer-seek buff (car addr)))))
    (cond ((= (cdr addr) (1- (line-len line)))
           (if (dlist-next line) (cons (car addr) (1+ (cdr addr)))))
          ((= (cdr addr) (line-len line))
           (if (dlist-next line) (cons (1+ (car addr)) 0)))
          (t (cons (car addr) (1+ (cdr addr)))))))

(defun buffer-prev-addr (buff addr)
  (let ((line (buffer-curr-line (buffer-seek buff (car addr)))))
    (if (zerop (cdr addr))
        (if (dlist-prev line) (cons (1- (car addr)) (line-len (dlist-prev line))))
        (cons (car addr) (1- (cdr addr))))))
