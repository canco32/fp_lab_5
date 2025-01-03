<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"
</p>
<p align="right"><strong>Студент:</strong> <i>Сілін Ілля Денисович КВ-12</i><p>
<p align="right"><strong>Рік:</strong> <i>2025</i><p>

## Загальне завдання

В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом
(п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV
файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним
типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.

1. Визначити структури або утиліти для створення записів з таблиць (в залежності від
типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію ```select``` , яка отримує на вхід шлях до файлу з таблицею, а
також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або
структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і
т. і. За потреби параметрів може бути кілька. ```select``` повертає лямбда-вираз,
який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було
передано у ```select``` . При цьому лямбда-вираз в якості ключових параметрів може
отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку
лише заданими значеннями (виконати фільтрування). Вибірка повертається у
вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від
варіанту):
    - структури у геш-таблиці
    - геш-таблиці у асоціативні списки
    - асоціативні списки у геш-таблиці
6. Написати функцію(-ї) для "красивого" виводу записів таблиці.

## Варіант 8

База даних: Наукові статті

Тип записів: Геш-таблиця

Таблиці: Спеціальності, Наукові статті

## Лістинг реалізації завдання
```
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
```

### Тестові набори та утиліти
```
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

```

### Тестування

```
CL-USER> (run-tests)
===== Reading and printing test (Articles.csv) =====
Number of records: 10
ID                  SPECIALIZATION      NAME                
--------------------------------------------------------------------------------
1                   Psychology          Journal of Psychology Research
2                   Psychology          Personality and Environmental Issues
3                   Journalism          Communications and Communicative Technologies
4                   Journalism          Ukrainian Information Space
5                   Management          Acta Academiae Beregsasiensis. Economics
6                   Management          Development Service Industry Management
7                   Law                 Advanced Space Law  
8                   Law                 Kyiv-Mohyla Law and Politics Journal
9                   Computer science    Computational Problems of Electrical Engineering
10                  Computer science    Computer Science and Applied Mathematics

Filtered (SPECIALIZATION = Law):
ID                  SPECIALIZATION      NAME                
--------------------------------------------------------------------------------
7                   Law                 Advanced Space Law  
8                   Law                 Kyiv-Mohyla Law and Politics Journal

Filtered (SPECIALIZATION = Management):
ID                  SPECIALIZATION      NAME                
--------------------------------------------------------------------------------
5                   Management          Acta Academiae Beregsasiensis. Economics
6                   Management          Development Service Industry Management

Filtered (SPECIALIZATION = Psychology):
ID                  SPECIALIZATION      NAME                
--------------------------------------------------------------------------------
1                   Psychology          Journal of Psychology Research
2                   Psychology          Personality and Environmental Issues

===== Reading and printing test (Specialties.csv) =====
Number of records: 5
ID                  SPECIALIZATION      CODE                
--------------------------------------------------------------------------------
1                   Psychology          053                 
2                   Journalism          061                 
3                   Management          073                 
4                   Law                 081                 
5                   Computer science    122                 

Filtered (CODE = 073):
ID                  SPECIALIZATION      CODE                
--------------------------------------------------------------------------------
3                   Management          073                 

Filtered (CODE = 081):
ID                  SPECIALIZATION      CODE                
--------------------------------------------------------------------------------
4                   Law                 081                 

Filtered (CODE = 122):
ID                  SPECIALIZATION      CODE                
--------------------------------------------------------------------------------
5                   Computer science    122                 
===== Writing test =====
Records for 'Computer science': 2
Writing them to 'comp_sci_articles.csv'...
===== Conversion test (hash-table->alist) =====
First record as hash:
ID                  SPECIALIZATION      NAME                
--------------------------------------------------------------------------------
1                   Psychology          Journal of Psychology Research
The same record as alist:
(("ID" . "1") ("SPECIALIZATION" . "Psychology")
 ("NAME" . "Journal of Psychology Research"))


===== All tests completed. =====
NIL
CL-USER> 
```
