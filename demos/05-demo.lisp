(asdf:load-system "trivia")
(asdf:load-system "str")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for file in (list "actor.lisp" "game-actor.lisp" "player-actor.lisp")
        do (load (merge-pathnames
                  (format nil "05-demo/~A" file)
                  (or *load-truename*
                      *compile-file-truename*)))))

(uiop:define-package #:clog-demo-5
  (:mix #:cl
        #:clog
        #:trivia
        #:clog-demo-5/actor
        #:clog-demo-5/game-actor
        #:clog-demo-5/player-actor)
  (:export #:start-demo))

(in-package #:clog-demo-5)

;;;
;;; Clog Setup
;;;


(defun on-new-window (body)
  (let ((player (make-instance 'player-actor :name (string (gensym))
                               :body body)))
    (send-message player (list :init body))

    (run-actor player)
    (send-message *game* (list :leave-game player))
    ))

(defun start-demo (&key (host "0.0.0.0") (port *clog-port*))
  (when *game*
    (quit-actor *game*))

  (setf *game* (make-instance 'game-actor :name "game-actor"))

  (send-message *game* :start-game)
  (spawn-actor *game*)

  (initialize 'on-new-window :host host :port port)
  (open-browser))

#+nil
(loop for thread in (bt:all-threads)
      when (or (str:starts-with? "game-actor" (bt:thread-name thread))
               (str:starts-with? "player-actor" (bt:thread-name thread)))
        do (format t "killing thread ~A~%" (bt:thread-name thread))
           (bt:destroy-thread thread))
