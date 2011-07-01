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

(defun get-list-event-name()
  (let (names)
  (loop for key being the hash-keys of *events*
       do (setq names (cons key names))) names))

(defun get-element-list-event(l)
  (if ( >= (length l) 0)
      (car l)
      nil))

(defun test (l)
  (let (a)
    (setq a (get-element-list-event l))
    (if (eq a nil) 
	(print "list is nil")
	(print a))
    (test (cdr l))))

(defun test2 (l)
  (let (a) (setq a (get-element-list-event l))
    (cond ((eq a nil) (print "list is nil"))
	  (T (lambda(a l) (print a) (test2 (cdr l))) a l)
      )))

(defun get-event-state(name)
  (gethash name *events*))

(defun check-use-event (name)
  (if (eq (get-event-state name) :use) t nil))

(defun create-thread (f)
  (sb-thread:make-thread (lambda() (eval f))))

;;do make-thread
;;(defun run-callbacks ()
;;  (loop 
;;       do
;;       (let (events (get-list-event-name))
;;	 )))