(defun clean-string (str)
  (string-trim '(#\Space #\Tab #\Newline #\Return) str))

(defun split (str &key (delim #\,))
  (let ((pos (position delim str :from-end nil)))
    (if pos
        (cons (subseq str 0 pos)
              (split (subseq str (1+ pos)) :delim delim))
      (list str))))

(defun split-csv-line (str)
  (mapcar #'clean-string (split (clean-string str) :delim #\,)))

(defun compose-hash-record (keys values &optional default)
  (let ((tbl (make-hash-table :test 'equal)))
    (loop for k in keys
          for v in values
          do (setf (gethash k tbl) (or v default)))
    tbl))

(defun read-csv-hash (stream &optional keys)
  (let ((line (read-line stream nil nil)))
    (cond
      ((null line) nil)
      ((null keys)
       (let ((header (split-csv-line line)))
         (read-csv-hash stream header)))
      (t
       (let ((row (compose-hash-record keys (split-csv-line line))))
         (cons row (read-csv-hash stream keys)))))))

(defun normalize-hash-record (keys record)
  (when keys
    (let ((k (car keys)))
      (cons (cons k (gethash k record)) 
            (normalize-hash-record (cdr keys) record)))))

(defun serialize-table-generic (fmt stream table &optional keys)
  (when table
    (if (null keys)
        (let ((newkeys (loop for k being the hash-keys of (first table) collect k)))
          (format stream (first fmt) newkeys)
          (serialize-table-generic fmt stream table newkeys))
      (progn
        (format stream (second fmt)
                (mapcar #'cdr (normalize-hash-record keys (first table))))
        (serialize-table-generic fmt stream (rest table) keys)))))

(defun write-csv (stream table)
  (serialize-table-generic (list "~{~A~^,~}~%"
                                 "~{~A~^,~}~%")
                           stream table))

(defun print-table (table)
  (let ((fmt (list
              "~{~20A~}~%--------------------------------------------------------------------------------~%"
              "~{~20A~}~%")))
    (serialize-table-generic fmt t table)))

(defun hash-table->alist (ht)
  (let ((result '()))
    (maphash (lambda (k v) (push (cons k v) result)) ht)
    (nreverse result)))

(defun filter-row (record filters)
  (block maybe
    (loop for (key value) on filters by #'cddr do
      (let* ((kstr (string-upcase (symbol-name key)))
             (current (gethash kstr record)))
        (cond
          ((listp value)
           (unless (member current value :test #'equalp)
             (return-from maybe nil)))
          ((and value (not (equalp current value)))
           (return-from maybe nil)))))
    t))

(defun select (fname &optional user-filter)
  (lambda (&rest conds)
    (with-open-file (s fname)
      (let ((table (read-csv-hash s))
            (result nil))
        (dolist (row table)
          (when (and (or (null user-filter) (funcall user-filter row))
                     (filter-row row conds))
            (push row result)))
        (nreverse result)))))

(defun test-reading-functions ()
  (format t "===== Reading and printing test (Articles.csv) =====~%")
  (let ((get-articles (select "Articles.csv")))
    (let ((all-articles (funcall get-articles)))
      (format t "Number of records: ~D~%" (length all-articles))
      (print-table all-articles)
      (format t "~%Filtered (SPECIALIZATION = Law):~%")
      (print-table (funcall get-articles :SPECIALIZATION "Law"))
      (format t "~%Filtered (SPECIALIZATION = Management):~%")
      (print-table (funcall get-articles :SPECIALIZATION "Management"))
      (format t "~%Filtered (SPECIALIZATION = Psychology):~%")
      (print-table (funcall get-articles :SPECIALIZATION "Psychology"))))
  (format t "~%===== Reading and printing test (Specialties.csv) =====~%")
  (let ((get-specialties (select "Specialties.csv")))
    (let ((all-specialties (funcall get-specialties)))
      (format t "Number of records: ~D~%" (length all-specialties))
      (print-table all-specialties)
      (format t "~%Filtered (CODE = 073):~%")
      (print-table (funcall get-specialties :CODE "073"))
      (format t "~%Filtered (CODE = 081):~%")
      (print-table (funcall get-specialties :CODE "081"))
      (format t "~%Filtered (CODE = 122):~%")
      (print-table (funcall get-specialties :CODE "122")))))

(defun test-writing-functions ()
  (format t "===== Writing test =====~%")
  (let* ((get-articles (select "Articles.csv"))
         (comp-sci-articles (funcall get-articles :SPECIALIZATION "Computer science")))
    (format t "Records for 'Computer science': ~D~%" (length comp-sci-articles))
    (format t "Writing them to 'comp_sci_articles.csv'...~%")
    (with-open-file (s "comp_sci_articles.csv"
                       :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-csv s comp-sci-articles))))

(defun test-conversion-function ()
  (format t "===== Conversion test (hash-table->alist) =====~%")
  (let ((get-articles (select "Articles.csv")))
    (let ((articles (funcall get-articles)))
      (when articles
        (let ((first-article (car articles)))
          (format t "First record as hash:~%")
          (print-table (list first-article))
          (format t "The same record as alist:~%~S~%~%"
                  (hash-table->alist first-article)))))))

(defun run-tests ()
  (test-reading-functions)
  (test-writing-functions)
  (test-conversion-function)
  (format t "~%===== All tests completed. =====~%"))
