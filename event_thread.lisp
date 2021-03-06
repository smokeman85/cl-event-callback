(defpackage #:cl-event-callback
  (:use :cl)
  (:export :connect
	   :disconnect
	   :trig-event
	   :not-trig-event
	   :thread-run-callbacks
	   :thread-stop-callbacks
	   :set-event-state))

(in-package #:cl-event-callback)

(defparameter *condition* (make-hash-table))

(defparameter *events* (make-hash-table))

(defparameter *event-state* '(:not-trig :trig :trig-stop))

(defparameter *callbacks* (make-hash-table))

(defun add-condition (name condition)
  (setf (gethash name *condition*) condition))

(defun del-condition (name)
  (remhash name *condition*))

(defun check-condition (name)
  (gethash name *condition*))

(defun set-event-state (name state)
  (if (member state *event-state*)
      (setf (gethash name *events*) state)
      (prin1 "Not found this state")))

(defun connect (f name &optional condition)
  (add-event name)
  (add-callback f name)
  (add-condition name condition))

(defun disconnect (name)
  (del-event name)
  (del-callback name)
  (del-condition name))

(defun add-event (name)
  "add new event"
  (setf (gethash name *events*) (car *event-state*)))

(defun del-event (name)
  "delete event"
  (remhash name *events*))

(defun trig-event (name)
  "triggered event"
  (setf (gethash name *events*) (cadr *event-state*)))

(defun not-trig-event (name)
  "dont triggered event"
  (setf (gethash name *events*) (car *event-state*)))

(defun add-callback(f name)
  "Link callback and event"
  (setf (gethash name *callbacks*) f))

(defun del-callback(name)
  "Delete link"
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
  (if (eq (get-event-state name) :trig) T nil))

(defun create-thread (f &optional name-of-thread)
  (bordeaux-threads:make-thread (lambda() (eval f)) :name name-of-thread))

(defun run-callbacks ()
  (loop do 
  (loop for i in (get-list-event-name)
       do
       (let (event state)
	 (setq event i)
	 (setq state (get-event-state event))
	 (cond ((check-condition event) (if (eval (gethash event *condition*)) (start-callback event) (not-trig-event event)))
	       ((eq state :trig) (start-callback event))
	       ((eq state :trig-stop) (start-callback event) (not-trig-event event))
	       (t (sleep 1)))))))

(defun thread-run-callbacks ()
  "start handler of events"
  (create-thread '(run-callbacks) "run-callbacks"))

(defun thread-stop-callbacks ()
  "stop handler of events"
  (if (numberp (find-thread-by-name "run-callbacks"))
      (bordeaux-threads:destroy-thread (nth (find-thread-by-name "run-callbacks") (bordeaux-threads:all-threads)))))

(defun list-to-string (l)
  (format nil "~a" l))

(defun find-thread-by-name (name)
  (let ((th) (result) (k 0))
    (setq result nil)
    (setq th (bordeaux-threads:all-threads))
    (loop for l in th do
	   (if (numberp (search name (list-to-string l)))
	       (return k))
	 (1+ k)
	 )))