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

(defun read-atoms (str)
  (with-input-from-string (stream str)
    (labels ((read-atoms% ()
               (let ((atom (read-atom stream)))
                 (cond ((null atom) nil)
                       ((symbolp atom) (cons atom (read-atoms%)))
                       ((eq (car atom) 'group)
                        (if (cadr atom)
                            (cons (list 'group (read-atoms (cadr atom)))
                                  (read-atoms%))))
                       (t (cons atom (read-atoms%)))))))
      (read-atoms%))))

(defun group-reps (tree)
  (mapcon (lambda (cur)
            (let ((atom (car cur)))
              (cond ((null atom) nil)
                    ((find atom '(zero-or-more one-or-more zero-or-one)) nil)
                    ((eq atom 'alternative) (list atom))

                    ;; Group atoms being used as arguments to REP operators
                    ((find (cadr cur) '(zero-or-more one-or-more zero-or-one))
                     (list (cadr cur)
                           (if (eq (car atom) 'group)
                               (list 'group (group-reps (cadr atom)))
                               atom)))

                    ;; Otherwise keep the atom on its own
                    (t (if (eq (car atom) 'group)
                            (list (list 'group (group-reps (cadr atom))))
                            (list atom))))))
          tree))

(defun group-terms (tree)
  (labels ((process-curr (tree) ; Recurse on groups
             (if (eq (caar tree) 'group)
                 (cons 'group (group-terms (cadar tree)))
                 (car tree)))

           (next-term (tree &optional type)
             (cond ((null tree) nil)
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

                   ((eq type 'alternative)
                    (if (eq (cadr tree) 'alternative)
                        (multiple-value-bind (term rest) (next-term (cddr tree) 'alternative)
                          (values (cons (process-curr tree) term)
                                  rest))
                        (values (list (process-curr tree))
                                (cdr tree))))

                   (t (if (eq (cadr tree) 'alternative)
                          (values nil tree)
                          (multiple-value-bind (term rest) (next-term (cdr tree) 'concatenated)
                            (values (cons (process-curr tree) term)
                                    rest)))))))
    (if tree
        (multiple-value-bind (term rest) (next-term tree)
          (cons term (group-terms rest))))))