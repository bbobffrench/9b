;;;; regex.lisp
;;;; as described (for the most part) by plan9port's regexp(7)

(defun extract-until (stream end-char &optional start-char)
  (let ((num-escapes 0) (balance 0))
    (with-output-to-string (out)
      (labels ((add-chars (char)
                 (cond ((null char)
                        (return-from extract-until)) ; Fail, the closing char was never found

                       ((char= char #\backslash)
                        (incf num-escapes)
                        (format out "~c" char)
                        (add-chars (read-char stream nil)))

                       ((and start-char (char= char start-char) (evenp num-escapes))
                        (incf balance)
                        (format out "~c" char)
                        (add-chars (read-char stream nil)))

                       ;; Do not exit if the closing character is escaped
                       ((and (char= char end-char) (oddp num-escapes))
                        (setf num-escapes 0)
                        (format out "~c" char)
                        (add-chars (read-char stream nil)))

                       ;; Do not exit if the characters are not balanced
                       ((and (char= char end-char) (> balance 0))
                        (decf balance)
                        (format out "~c" char)
                        (add-chars (read-char stream nil)))

                       ((and (char= char end-char)) nil)

                       (t (setf num-escapes 0)
                          (format out "~c" char)
                          (add-chars (read-char stream nil))))))
        (add-chars (read-char stream nil))))))

;;; An atom is a literal, charclass, group, REP operator, or alternative operator

(defun read-atom (stream)
  (let ((char (read-char stream nil)))
    (cond ((null char) nil) ; No atoms left
          ((char= char #\\)
           (let ((next (read-char stream nil)))
             (list 'literal
                   (if (char= next #\n)
                       #\newline
                       next))))
          ((char= char #\() (list 'concatenated (extract-until stream #\) #\()))
          ((char= char #\[) (list 'charclass (extract-until stream #\])))
          ((char= char #\|) 'alternative)
          ((char= char #\*) 'zero-or-more)
          ((char= char #\+) 'one-or-more)
          ((char= char #\?) 'zero-or-one)
          ((char= char #\.) '(anything))
          ((char= char #\^) '(start-of-line))
          ((char= char #\$) '(end-of-line))
          (t (list 'literal char)))))

(defgeneric read-atoms (obj))

(defmethod read-atoms ((obj stream))
  (let ((atom (read-atom obj)))
    (cond ((null atom) nil)
          ((symbolp atom) (cons atom (read-atoms obj)))
          ((eq (car atom) 'concatenated)
           (cons (cons 'concatenated (if (cadr atom) (read-atoms (cadr atom))))
                 (read-atoms obj)))
          (t (cons atom (read-atoms obj))))))

(defmethod read-atoms ((obj string))
  (with-input-from-string (stream obj)
    (read-atoms stream)))

(defun valid-regex-p (tree)
  (let (prev)
    (mapl (lambda (curr)
            (cond ;; Ensure all alternative operators have at least 2 operands
                  ((eq (car curr) 'alternative)
                   (if (or (eq prev 'alterative) (symbolp (cadr curr)))
                       (let ((err "regexp: missing operand for |"))
                         (return-from valid-regex-p (values nil err)))))

                  ;; Enure all REP operators have an operand
                  ((symbolp (car curr))
                   (if (or (null prev) (find prev '(one-or-more zero-or-more zero-or-one)))
                       (let* ((op (cdr (assoc (car curr) '((one-or-more  . #\+)
                                                           (zero-or-more . #\*)
                                                           (zero-or-one  . #\?)))))
                              (err (format nil "regexp: missing operand for ~c" op)))
                          (return-from valid-regex-p (values nil err)))))

                  ;; Recurse on groups, ensuring their validity
                  ((eq (caar curr) 'concatenated)
                   (if (cdar curr)
                       (multiple-value-bind (result err) (valid-regex-p (cdar curr))
                         (if (null result)
                             (return-from valid-regex-p (values nil err))))
                       (let ((err "regexp: empty or mismatched parentheses"))
                         (return-from valid-regex-p (values nil err)))))

                  ;; Ensure that all charclasses are valid
                  ((eq (caar curr) 'charclass)
                   (if (null (cdar curr))
                       (let ((err "regexp: malformed charclass"))
                         (return-from valid-regex-p (values nil err))))))
            (setf prev (car curr)))
          tree)) t)

;;; From this point on, all trees are assumed to be valid

(declaim (ftype function read-terms))

;;; An expression is a literal or repeated atom

(defun next-expr (tree)
  (let ((next (cadr tree))
        ;; Expand groups
        (atom (if (and (listp (car tree)) (eq (caar tree) 'concatenated))
                  ;; Remove redundant concatenates, not necessary
                  (let ((expansion (read-terms (cdar tree))))
                    (if (cdr expansion)
                        (cons 'concatenated expansion)
                        (car expansion)))
                  (car tree))))
    (if (or (listp next) (eq next 'alternative))
        (values atom (cdr tree))
        (values (list next atom) (cddr tree)))))

;;; A term is a sequence of concatenated or alternative expressions

(defun finish-alternative-term (tree)
  (if (eq (car tree) 'alternative)
      (multiple-value-bind (expr rest) (next-expr (cdr tree))
        (multiple-value-bind (term rest) (finish-alternative-term rest)
          (values (cons expr term) rest)))
      (values nil tree)))

(defun alternative-term-p (tree)
  (if (eq (cadr tree) 'alternative)
      t
      (if (symbolp (cadr tree)) ; In case of REP operator
          (eq (caddr tree) 'alternative))))

(defun finish-concatenated-term (tree)
  (if (or (null tree) (alternative-term-p tree))
      (values nil tree)
      (multiple-value-bind (expr rest) (next-expr tree)
        (multiple-value-bind (term rest) (finish-concatenated-term rest)
          (values (cons expr term) rest)))))

(defun next-term (tree)
  (multiple-value-bind (expr rest) (next-expr tree)
    (if (eq (car rest) 'alternative)
        (multiple-value-bind (term rest) (finish-alternative-term rest)
          (if expr
              (values (nconc (list 'alternative expr) term) rest)))
        (multiple-value-bind (term rest) (finish-concatenated-term rest)
          (if expr
              (values (nconc (list 'concatenated expr) term) rest))))))

(defun read-terms (tree)
  (if tree
      (multiple-value-bind (term rest) (next-term tree)
        (cons term (read-terms rest)))))

(defun parse-regex (str)
  (let ((tree (read-atoms str)))
    (multiple-value-bind (result err) (valid-regex-p tree)
      (if result
          ;; Remove redundant concatenates, not necessary
          (let ((terms (read-terms tree)))
            (if (cdr terms)
                (cons 'concatenated terms)
                (car terms)))
          err))))
