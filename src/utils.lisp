;;;; this file contains all the definitions for function

;;; constants

(defconstant *song* (car (directory "../Jack\ Stauber\ -\ Buttercup.mp3")))

(defconstant +id3-class-name+ 'id3-tag "name for the class")

(defun read-song (&optional (song *song*))
  "this function extracts the ID3 tags from a song '.mp3 file'"
  (with-open-file (stream song :element-type '(unsigned-byte 8)
			       :if-does-not-exist nil)
    (let ((result ()))
      (loop for byte = (read-byte stream nil 'eof)
	    until (eql byte 'eof)
	    when (= byte (char-code #\I))
	      do (if (id3-head? stream)
		     (progn
		       (format t "found ID3 header")
		       (push (collect-id3-tag stream) result))))
      result)))

(defun collect-id3-tag (stream)
  "this function assumes that the stream has already read the ID3 of the ID3 header"
  (append
   '(73 68 51)
   (loop collect (read-byte stream nil nil)
	 repeat 7)))

(defmacro id3-head? (stream)
  "this function reads two more bytes from a byte stream to determine whether or not there is an ID3 tag"
  `(progn (let* ((b1 (read-byte ,stream nil 'eof))
		 (b2 (read-byte ,stream nil 'eof)))
	    (and ,@(mapcar #'(lambda (x y)
			       `(= ,x (char-code ,y)))
			   '(b1 b2)
			   '(#\D #\3))))))

(defun as-keyword (sym)
  "converts a symbol to a keyword"
  (intern (string sym) :keyword))

(defun slot->defclass-slot (name)
  "helper function to create a class"
  `(,name :initarg ,(as-keyword name) :accessor name))

(defmacro define-binary-class (name slots)
  "this macro easily creates a class"
  `(defclass ,name ()
     ,(mapcar #'slot->defclass-slot slots)))

(defun id3-class ()
  "this function uses a macro to create a class"
  (define-binary-class id3-tag
      (identifier major-version revision flags size frames)))

(defmacro take-from (list &rest indices)
  "this function takes the indices from a list and puts them into a new list"
  `(list ,@(mapcar #'(lambda (x) `(nth ,x ,list)) indices)))

(defun id3-header->class (tag)
  "this function converts a tag (list) to a class"
  (let ((object (make-instance 'id3-tag)))
    (with-slots (identifier major-version revision flags size frames) object
      (setf identifier (take-from tag 0 1 2))
      (setf major-version (nth 3 tag))
      (setf revision (nth 4 tag))
      (setf flags (nth 5 tag))
      (setf size (take-from tag 6 7 8 9)))
    object))
