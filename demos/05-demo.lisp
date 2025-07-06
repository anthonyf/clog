(asdf:load-system "trivia")

(eval-when (:load-toplevel)
  (loop for file in (list "actor.lisp" "game-actor.lisp" "player-actor.lisp")
        do (load (merge-pathnames
                  (format nil "05-demo/~A" file) *load-truename*))))

(uiop:define-package #:clog-demo-5
  (:mix #:cl
        #:clog
        #:trivia
        #:clog-demo-5/game-actor
        #:clog-demo-5/player-actor)
  (:export #:start-demo))

(in-package #:clog-demo-5)

;;;
;;; Clog Setup
;;;


(defun on-new-window (body)
  (let ((player (make-instance 'player-actor :name (string (gensym))
                               :behavior 'player-behavior)))
    (send-message player (list :init body))
    (run-actor player)))

(defun start-demo (&key (host "0.0.0.0") (port *clog-port*))
  (when *game*
    (setf (quitp *game*) t))

  (setf *game* (make-instance 'game-actor :name "game-actor"
                                    :behavior 'game-behavior))

  (send-message *game* :start-game)
  (spawn-actor *game*)

  (initialize 'on-new-window :host host :port port)
  (open-browser))
