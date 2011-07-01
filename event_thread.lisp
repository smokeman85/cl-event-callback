;;

(defparameter *events* (make-hash-table))

(defparameter *event-state* '(:use :not-use))

(defparameter *callbacks* (make-hash-table))

(defun add-event (name)
  (setf (gethash name *events*) (cadr *event-state*)))

(defun del-event (name)
  (remhash name *events*))

(defun use-event (name)
  (setf (gethash name *events*) (car *event-state*)))

(defun not-use-event (name)
  (setf (gethash name *events*) (cadr *event-state*)))

(defun add-callback(f name)
  (setf (gethash name *callbacks*) f))

(defun del-callback(name)
  (remhash name *callbacks*))

(defun start-callback(name)
  (create-thread (gethash name *callbacks*)))

(defun get-list-event-name()
  (let (names)
  (loop for key being the hash-keys of *events*
       do (setq names (cons key names))) names))

(defun get-element-list-event(l)
  (if ( >= (length l) 0)
      (car l)
      nil))

(defun get-event-state(name)
  (gethash name *events*))

(defun check-use-event (name)
  (if (eq (get-event-state name) :use) T nil))

(defun create-thread (f &optional thread-name)
  (sb-thread:make-thread (lambda() (eval f)) :name thread-name))

(defun run-callbacks ()
  (loop do
  (loop for i in (get-list-event-name) 
       do
       (let (event) (setq event i)
	    (if (eq (check-use-event event) T)
		(start-callback event)
		;;(sleep 1))
	    (sleep 1)))
       )))

(defun thread-run-callbacks ()
  (create-thread '(run-callbacks) "run-callbacks"))

(defun thread-stop-callbacks ()
  (if (numberp (find-thread-by-name "run-callbacks"))
      (sb-thread:terminate-thread (nth (find-thread-by-name "run-callbacks") (sb-thread:list-all-threads)))))

(defun list-to-string (l)
  (format nil "~a" l))

(defun find-thread-by-name (name)
  (let ((th) (result) (k 0))
    (setq result nil)
    (setq th (sb-thread:list-all-threads))
    (loop for l in th do
	   (if (numberp (search name (list-to-string l)))
	       (return k))
	 (1+ k)
	 )))