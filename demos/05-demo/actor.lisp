;;;
;;; A simple Actor model implementation
;;;
;;; Example usage:
;;;
;;; (let ((hello (make-instance 'actor
;;;                             :name "hello-actor"
;;;                             :behavior (lambda (actor msg)
;;;                                         (when (eq msg :quit)
;;;                                           (quit-actor actor))
;;;                                         (format t "hello ~A" msg)
;;;                                         (terpri)))))
;;;   (send-message hello :bob)
;;;   (send-message hello :sally)
;;;   (send-message hello :quit)
;;;   (run-actor hello))

(uiop:define-package  #:clog-demo-5/actor
  (:use #:cl)
  (:export #:actor
           #:name
           #:behavior
           #:run-actor
           #:spawn-actor
           #:send-message
           #:recieve-message
           #:quit-actor
           #:should-quit-p))

(in-package #:clog-demo-5/actor)

(defclass actor ()
  ((name :initarg :name
         :initform (error "actor must have a name!")
         :reader name)
   (behavior :initarg :behavior
             :initform (error "actor must have behavior"))
   (lock)
   (message-ready)
   (wait-timeout :initform 1)
   (thread :initform nil)
   (quitp :initform nil)
   ;; front and back lists for efficient queue
   (queue-front :initform nil)
   (queue-back :initform nil)))

(defmethod initialize-instance :after ((actor actor) &key)
  (with-slots (lock message-ready name) actor
    (setf message-ready (bt:make-condition-variable
                         :name (format nil "~A-~A" name "message-ready")))
    (setf lock (bt:make-lock (format nil "~A-~A" name "lock")))))

(defmethod should-quit-p ((actor actor))
  "Returns true if the actor should quit"
  (with-slots (quitp) actor
    quitp))

(defmethod run-actor ((actor actor))
  "Runs the actor on the current thread, blocks forever until quit"
  (with-slots (lock message-ready wait-timeout behavior name quitp thread) actor
    (loop
      (bt:with-recursive-lock-held (lock)
        (when (should-quit-p actor)
          (format t "recieved actor ~A quitting~%" name)
          (setf thread nil)
          (return))
        (let ((msg (recieve-message actor)))
          (cond (msg
                 (format t "actor ~A received message: ~A~%" name msg)
                 (funcall behavior actor msg))
                (t (bt:condition-wait message-ready lock
                                      :timeout wait-timeout))))))))

(defmethod spawn-actor ((actor actor))
  "Runs the actor on a new thread"
  (with-slots (thread name) actor
    (assert (not thread) nil "thread is already running")
    (setf thread
          (bt:make-thread
           (lambda () (run-actor actor))
           :name (format nil "~A-~A" name "thread")))))

(defmethod quit-actor ((actor actor))
  "Quits the actor, stops the thread if it is running"
  (with-slots (quitp lock name message-ready) actor
    (bt:with-lock-held (lock)
      (format t "sending actor ~A~%" name)
      (setf quitp t)
      (bt:condition-notify message-ready))))

(defmethod send-message ((actor actor) msg)
  (with-slots (lock message-ready thread queue-front queue-back name) actor
    (bt:with-lock-held (lock)
      (setf queue-back (cons msg queue-back))
      (bt:condition-notify message-ready)
      (unless (and thread (bt:thread-alive-p thread))
        (warn "~A thread not started or is not alive" name)))))

(defmethod recieve-message ((actor actor))
  (with-slots (lock message-ready queue-front queue-back) actor
    (bt:with-recursive-lock-held (lock)
      ;; TODO move this loop to the runner function
      (cond
        (queue-front
         (let ((item (first queue-front)))
           (setf queue-front (rest queue-front))
           item))
        (queue-back
         (setf queue-front (reverse queue-back)
               queue-back '())
         (recieve-message actor))
        (t nil)))))
