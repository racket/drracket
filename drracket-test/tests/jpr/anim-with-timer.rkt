#lang racket/gui
;;; anim-with-timer.rkt
;;; pour PF2, Racket 6.2.1

(module test racket/base)

(define FRAME (new frame% (label "Animation") (x 10) (y 10)))

(define SIZE 300)    ; taille du canvas
(define SIZE/2 (/ SIZE 2))
(define R 80)        ; rayon de la trajectoire de la balle

(define BITMAP (make-object bitmap% SIZE SIZE))        ; double buffer (cours 7) !
(define BITMAP-DC (new bitmap-dc% (bitmap BITMAP)))
(send BITMAP-DC set-pen (make-object pen% "blue" 3 'solid))

(define t 0)         ; l'angle polaire de la balle

(define CANVAS	 
  (new canvas% 
       (parent FRAME) 
       (min-width SIZE) 
       (min-height SIZE)
       (paint-callback (lambda (obj dc) 
                         (send BITMAP-DC clear)   ; on travaille dans le double buffer
                         (send BITMAP-DC draw-ellipse (- SIZE/2 R) (- SIZE/2 R) (* 2 R) (* 2 R))
                         (send BITMAP-DC draw-line SIZE/2 0 SIZE/2 SIZE)
                         (send BITMAP-DC draw-line 0 SIZE/2 SIZE SIZE/2)
                         (send BITMAP-DC draw-ellipse 
                               (- (+ SIZE/2 (* R (cos t))) 10) (- (+ SIZE/2 (* R (sin t))) 10) 20 20)
                         (send dc draw-bitmap BITMAP 0 0 'solid)))))


(define TIMER 
  (new timer% 
       (interval 20)
       (notify-callback (lambda ()
                          (set! t (+ t 0.05))
                          (send CANVAS refresh)))))

(new button%           ; inutile de donner un nom au bouton si l'on ne s'en sert pas !
     (parent FRAME)
     (label "Suspend")
     (callback (lambda (obj evt)
                 (case timer-on?
                     ((#t) (send TIMER stop) (set! timer-on? #f) (send obj set-label "Resume"))
                     ((#f) (send TIMER start DT) (set! timer-on? #t) (send obj set-label "Suspend"))))))

(send FRAME show #t)
(define DT 20)         ; top d'horloge toutes les 20 ms
(send TIMER start DT)
(define timer-on? #t)

