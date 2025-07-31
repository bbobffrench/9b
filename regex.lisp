;;;; regex.lisp
;;;; as described by plan9port's regexp(7)

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

(defun read-atom (stream)
  (let ((char (read-char stream nil)))
    (cond ((null char) nil) ; No atoms left
          ((char= char #\\)
           (let ((next (read-char stream nil)))
             (list 'literal
                   (if (char= next #\n)
                       #\newline
                       next))))
          ((char= char #\() (list 'group (extract-until stream #\) #\()))
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
          ((eq (car atom) 'group)
           (cons (cons 'group (if (cadr atom) (read-atoms (cadr atom))))
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
                  ((eq (caar curr) 'group)
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

(defun group-reps (tree)
  (mapcon (lambda (curr)
            (let ((atom (car curr)))
              (cond ((null atom) nil)
                    ((eq atom 'alternative) (list atom))
                    ((symbolp atom) nil)

                    ;; Group atoms being used as arguments to REP operators
                    ((find (cadr curr) '(zero-or-more one-or-more zero-or-one))
                     (list (list (cadr curr)
                                 (if (eq (car atom) 'group)
                                     (cons 'group (group-reps (cdr atom)))
                                     atom))))

                    ;; Otherwise keep the atom on its own
                    (t (if (eq (car atom) 'group)
                            (list (cons 'group (group-reps (cdr atom))))
                            (list atom))))))
          tree))

(defun group-terms (tree)
  (labels ((process-curr (tree) ; Recurse on groups
             (if (eq (caar tree) 'group)
                 (cons 'group (group-terms (cdar tree)))
                 (car tree)))

           (next-term (tree &optional type)
             (cond ((null tree) nil)
                   ;; Determine the term type and begin grouping
                   ((null type)
                    (let ((type (if (eq (cadr tree) 'alternative)
                                    'alternative
                                    'concatenated)))
                      (multiple-value-bind (term rest)
                          (next-term (if (eq type 'alternative)
                                         (cddr tree)
                                         (cdr tree))
                                     type)
                        (values (nconc (list type (process-curr tree)) term)
                                rest))))

                   ;; Group alternative terms
                   ((eq type 'alternative)
                    (if (eq (cadr tree) 'alternative)
                        (multiple-value-bind (term rest) (next-term (cddr tree) 'alternative)
                          (values (cons (process-curr tree) term)
                                  rest))
                        (values (list (process-curr tree))
                                (cdr tree))))

                   ;; Group concatenated terms
                   (t (if (eq (cadr tree) 'alternative)
                          (values nil tree)
                          (multiple-value-bind (term rest) (next-term (cdr tree) 'concatenated)
                            (values (cons (process-curr tree) term)
                                    rest)))))))
    ;; Return a list of all terms present in the tree
    (if tree
        (multiple-value-bind (term rest) (next-term tree)
          (cons term (group-terms rest))))))

(defun parse-regex (str)
  (let ((tree (read-atoms str)))
    (multiple-value-bind (result err) (valid-regex-p tree)
      (if result
          (cons 'group (group-terms (group-reps tree)))
          err))))
