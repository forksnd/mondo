(defpackage #:mondo/utils
  (:use #:cl)
  #-(or sbcl allegro ccl clisp)
  (:import-from #:babel)
  (:export #:find-shortest-nickname
           #:space-char-p
           #:*space-chars*
           #:string-space-trim
           #:integer-string-p
           #:starts-with
           #:string-to-octets
           #:octets-to-string))
(in-package #:mondo/utils)

(defun find-shortest-nickname (package-names)
  (first
    (sort package-names
          #'<=
          :key #'length)))

(defparameter *space-chars*
  '(#\Space #\Newline #\Tab #\Return))

(defun space-char-p (char)
  (check-type char character)
  (and (member char *space-chars*)
       t))

(defun string-space-trim (value)
  (check-type value string)
  (string-trim *space-chars* value))

(defun integer-string-p (value)
  (check-type value string)
  (let ((trimmed (string-space-trim value)))
    (and (/= 0 (length trimmed))
         (every #'digit-char-p trimmed))))

(defun starts-with (prefix string)
  (check-type string string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun string-to-octets (string &key (start 0) (end (length string)))
  #+sbcl (sb-ext:string-to-octets string :start start :end end :external-format :utf-8)
  #+allegro (excl:string-to-octets string :start start :end end :external-format :utf8)
  #+ccl (ccl:encode-string-to-octets string :start start :end end :external-format :utf-8)
  #+clisp (ext:convert-string-to-bytes string charset:utf-8 :start start :end end)
  #-(or sbcl allegro ccl clisp)
  (babel:string-to-octets string :start start :end end :encoding :utf-8))

(defun octets-to-string (octets &key (start 0) (end (length octets)))
  #+sbcl (sb-ext:octets-to-string octets :start start :end end :external-format :utf-8)
  #+allegro (excl:octets-to-string octets :start start :end end :external-format :utf8)
  #+ccl (ccl:decode-string-from-octets octets :start start :end end :external-format :utf-8)
  #+clisp (ext:convert-string-from-bytes octets charset:utf-8 :start start :end end)
  #-(or sbcl allegro ccl clisp)
  (babel:octets-to-string octets :start start :end end :encoding :utf-8))
