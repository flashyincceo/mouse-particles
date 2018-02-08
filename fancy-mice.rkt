;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |fancy mice|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/math)

;; A demo for different mouse effects

;; =================
;; Constants:
(define WIDTH 600)
(define HEIGHT 400)

(define FADE-SPEED 8)    ; Lower is a larger radius for the particles
(define ROTATE-SPEED 10)

(define MTS (empty-scene WIDTH HEIGHT))

;; =================
;; Data definitions:

(define-struct particle (dx dy angle opacity rotation))
;; Particle is (make-particle Number Number Number[0, 360) Number[0, 255] Number 
;; interp. an image dx (in the x-direction) and dy (in the y-direction) pixels away from the mouse moving away from it
;;         at the given angle with the given opacity rotated a given amount of radians

(define P1 (make-particle 100 20 90 50 1))
(define P2 (make-particle 92 301 175 255 0.4))

#;
(define (fn-for-particle p)
  (... (particle-dx p)
       (particle-dy p)
       (particle-angle p)
       (particle-opacity p)))

(define-struct mouse-effect (particles spawn-rate speed image ticks x y))
;; MouseEffect is (make-mouse-effect (listof Particle) Natural Number Image )
;; interp. a collection of particles (that are produced every spawn-rate ticks) that emanate from the mouse at the given speed (measured in pixels/tick)
;;         and fade out. The particles all have the given image and travel in random directions.
;;         x and y are the mouse position

#;
(define (fn-for-mouse-effect mef)
  (...(mouse-effect-particles  mef)
      (mouse-effect-spawn-rate mef)
      (mouse-effect-speed      mef)
      (mouse-effect-image      mef)
      (mouse-effect-ticks      mef)
      (mouse-effect-x          mef)
      (mouse-effect-y          mef)))

(define ME1 (make-mouse-effect (list P1 P2) 30 2 (square 10 "solid" "blue") 20 100 302))
(define DEFAULT (make-mouse-effect empty 2 2  (square 5 "solid" "blue") 1000 1000 0))

;; =================
;; Functions:

;; MouseEffect -> MouseEffect
;; start the world with (main DEFAULT)
;; 
(define (main mef)
  (big-bang mef                            ; MouseEffect
            (on-tick   update-effects)     ; MouseEffect -> MouseEffect
            (to-draw   render)             ; MouseEffect -> Image
            (on-mouse  handle-mouse)))     ; MouseEffect Integer Integer MouseEvent -> MouseEffect

;; MouseEffect -> MouseEffect
;; move each particle away from the current position of the mouse
;; and add another particle if (remainder ticks spawn-rate) equals 0

(define (update-effects mef)
  (make-mouse-effect (remove-clear (add-new (move-particles mef (mouse-effect-particles mef))
                                            (mouse-effect-spawn-rate mef)
                                            (mouse-effect-ticks mef)))
                     (mouse-effect-spawn-rate mef)
                     (mouse-effect-speed      mef)
                     (mouse-effect-image      mef)
                     (add1 (mouse-effect-ticks      mef))
                     (mouse-effect-x          mef)
                     (mouse-effect-y          mef)))

;; (listof Particle) -> (listof Particle)
;; remove any particles that have opacity < 0
(define (remove-clear lop)
  (local [(define (cleared? p) (>= (particle-opacity p) 0))]
    (filter cleared? lop)))

;; MouseEffect -> (listof Particle)
;; produce an updated list of particles that have moved away from the mouse at the given speed and decreased in opacity by FADE-SPEED
#;
(check-expect (move-particles (make-mouse-effect (list (make-particle 0 0 (/ pi 2) 255 0))
                                                 20
                                                 1
                                                 (square 10 "solid" "blue")
                                                 0
                                                 100
                                                 100)
                              (list (make-particle 0 0 (/ pi 2) 255)))

              (list (make-particle (+ 0 (* 1 (cos (/ pi 2))))
                                   (+ 0 (* 1 (sin (/ pi 2))))
                                   (/ pi 2)
                                   (- 255 FADE-SPEED)
                                   (+ 0 ROTATE-SPEED))))
                                   
;(define (move-particles mef) empty) ;stub
(define (move-particles mef lop)
  (cond [(empty? lop) empty]
        [else
         (cons (move-particle mef (first lop))
               (move-particles mef (rest lop)))]))

;; Particle MouseEffect -> Particle
;; move a single particle
(define (move-particle mef p)
  (local [(define a (particle-angle p))
          (define s (mouse-effect-speed mef))]
    (make-particle (+ (particle-dx p) (* s (cos a)))
                   (+ (particle-dy p) (* s (sin a)))
                   a
                   (- (particle-opacity p) FADE-SPEED)
                   (+ (particle-rotation p) ROTATE-SPEED))))
                 

;; (listof Particle) Number Number -> (listof Particle)
;; add a new particle if (remainder ticks spawn-rate) is 0
(check-expect (length (add-new (list (make-particle 4 15 pi 100 0.2))
                               10
                               40))
              2)

;(define (add-new lop sr t) empty) ;stub
(define (add-new lop sr t)
  (if (= (remainder t sr) 0)
      (cons (make-particle 0 0 (degrees->radians (random 360)) 255 0) lop)
      lop))


;; MouseEffect -> Image
;; render each of the mouse effect particles at dx dy from the mouse
#;
(check-expect (render (make-mouse-effect (list (make-particle 40 -40 (/ pi 2) 255))
                                         20
                                         1
                                         (square 10 "solid" "blue")
                                         0
                                         100
                                         100))
              (place-image (square 10 "solid" "blue")
                           (+ 100 40)
                           (+ 100 -40)
                           MTS))
              
;(define (render mef) MTS) ;stub
(define (render mef)
  (render-particles (mouse-effect-particles mef)
                    (mouse-effect-image mef)
                    (mouse-effect-x mef)
                    (mouse-effect-y mef)))

;; (listof Particle) Image Number Number -> Image
;; produce an image of all the particles surrounding the mouse

(define (render-particles lop i x y)
  (cond [(empty? lop) MTS]
        [else
         (place-image (rotate (particle-rotation (first lop))
                              (square 10 "solid" (make-color 0 0 255 (particle-opacity (first lop)))))
                      (+ x (particle-dx (first lop)))
                      (+ y (particle-dy (first lop)))
                      (render-particles (rest lop) i x y))]))


;; MouseEffect Integer Integer MouseEvent -> MouseEffect
;; update the world state with the new mouse x and y
(define (handle-mouse mef x y me)
  (make-mouse-effect (mouse-effect-particles  mef)
                     (mouse-effect-spawn-rate mef)
                     (mouse-effect-speed      mef)
                     (mouse-effect-image      mef)
                     (mouse-effect-ticks      mef)
                     x
                     y))
