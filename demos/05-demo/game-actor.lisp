;;;
;;; Game Actor
;;;

(uiop:define-package #:clog-demo-5/game-actor
  (:mix #:cl #:trivia)
  (:mix-reexport #:clog-demo-5/actor)
  (:export #:game-actor
	   #:*game*))

(in-package #:clog-demo-5/game-actor)

(defvar *game* nil
  "Singleton game actor instance.")

(defclass game-actor (actor)
  ((behavior :initform 'game-behavior)
   (players :initform nil)))

(defun game-behavior (game msg)
  (with-slots (players) game
    (match msg

      (:start-game
       (format t "game started"))

      ((list :join-game player)
       (pushnew player players))

      (t (error "unknown message ~A" msg)))))
