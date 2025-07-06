;;;
;;; Player Actor
;;;

(uiop:define-package #:clog-demo-5/player-actor
  (:mix #:cl
	#:clog
	#:clog-demo-5/game-actor
	#:trivia)
  (:mix-reexport #:clog-demo-5/actor)
  (:export #:player-actor))

(in-package #:clog-demo-5/player-actor)

(defclass player-actor (actor)
  ((behavior :initform 'player-behavior)))

(defun player-behavior (player msg)
  (match msg

    ((list :init body)
     (let ((h1 (clog:create-element body "h1" :content "Multi-roids Game")))
       (declare (ignore h1)))
     
     (break)

     (send-message *game* (list :join-game player)))

    (t (error "unknow message ~A" msg))))
