(asdf:load-system "trivia")

(uiop:define-package #:clog-demo-5
  (:mix #:cl #:clog #:trivia)
  (:export #:start-demo))

(in-package #:clog-demo-5)

;;;
;;; A simple Actor
;;;

(defclass actor ()
  ((name :initarg :name
	 :accessor actor-name
	 :initform (error "actor must have a name!"))
   (behavior :initarg :behavior
	     :accessor actor-behavior
	     :initform (error "actor must have behavior"))
   (lock)
   (message-ready)
   (thread :initform nil)
   (quitp :initform nil :accessor actor-quitp)
   ;; front and back lists for efficient queue
   (queue-front :initform nil)
   (queue-back :initform nil)))

(defmethod initialize-instance :after ((actor actor) &key)
  (with-slots (lock message-ready name) actor
    (setf message-ready (bt:make-condition-variable :name (format nil "~A-~A" name "message-ready")))
    (setf lock (bt:make-lock (format nil "~A-~A" name "lock")))))

(defmethod run-actor ((actor actor))
  "Runs the actor on the current thread, blocks forever until quit"
  (with-slots (lock message-ready behavior name quitp thread) actor
    (loop
      (when quitp
	(format t "thread ~A quitting" name)
	(setf thread nil)
	(return))
      (let ((msg (recieve-message actor)))
	(funcall behavior actor msg)))))

(defmethod spawn-actor ((actor actor))
  "Runs the actor on a new thread"
  (with-slots (thread name) actor
    (assert (not thread) nil "thread is already running")
    (setf thread
	  (bt:make-thread
	   (lambda () (run-actor actor))
	   :name (format nil "~A-~A" name "thread")))))

(defmethod send-message ((actor actor) msg)
  (with-slots (lock message-ready thread queue-front queue-back) actor
    (bt:with-lock-held (lock)
      (setf queue-back (cons msg queue-back))
      (bt:condition-notify message-ready)
      (unless (and thread (bt:thread-alive-p thread))
	(warn "actor thread not started or is not alive")))))

(defmethod recieve-message ((actor actor))
  (with-slots (lock message-ready queue-front queue-back) actor
    (bt:with-recursive-lock-held (lock)
      (loop
	(cond
	  (queue-front
	   (let ((item (first queue-front)))
	     (setf queue-front (rest queue-front))
	     (return item)))
	  (queue-back
	   (setf queue-front (reverse queue-back)
		 queue-back '())
	   (return (recieve-message actor)))
	  (t
	   (format t "waiting for next message")
	   (bt:condition-wait message-ready lock)))))))

;;;
;;; Game Actor
;;;

(defclass game (actor)
  ())

(defparameter *game* nil)

(defun game-behavior (game msg)
  (declare (ignore game))
  (cond ((eq msg :start-game)
	 )
	(t (error "unknown message ~A" msg))))

;;;
;;; Player Actor
;;;

(defclass player (actor)
  ())

(defun player-behavior (player msg)
  (declare (ignore player))
  (match msg
    ((list :init body)
     (let ((h1 (clog:create-element body "h1" :content "Multi-roids Game")))
       (declare (ignore h1))))
    (t (error "unknow message ~A" msg))))

;;;
;;; Clog Setup
;;;


(defun on-new-window (body)
  (let ((player (make-instance 'player :name (string (gensym))
			       :behavior 'player-behavior)))
    (send-message player (list :init body))
    (run-actor player)))

(defun start-demo (&key (host "0.0.0.0") (port *clog-port*))
  (when *game*
    (setf (actor-quitp *game*) t))
  
  (setf *game* (make-instance 'game :name "game-actor"
				    :behavior 'game-behavior))

  (send-message *game* :start-game)
  (spawn-actor *game*)

  (initialize 'on-new-window :host host :port port)
  (open-browser))
