(require 2htdp/universe)
(require 2htdp/image)

;; A simple spaceship game
;;=========================

;; Initiate using: 
;; (main (make-game (make-ship 300 550 "none") empty 0)) 


;; Constants:

(define WIDTH  600)
(define HEIGHT 600)
(define MTS (rectangle WIDTH HEIGHT "solid" "black")) 

(define INIT-CSPEED 8)    ;initial comet speed 
(define STAGE2-CSPEED 10) ;comet speed after score = 1000  
(define STAGE3-CSPEED 12) ;comet speed after score = 2000
(define STAGE4-CSPEED 14) ;comet speed after score = 3000
(define STAGE5-CSPEED 16) ;comet speed for score > 4000

(define SSPEED 5) 
(define SCORE-RATE 1)
(define SHIP-IMG (above
                  (triangle 30 "solid" "green")
                  (beside
                   (triangle 12 "solid" "black")
                   (triangle 12 "solid" "black"))))
(define MOVING-SHIP-IMG (above
                         (triangle 30 "solid" "green")
                         (beside
                          (overlay (triangle 6 "solid" "red") (triangle 12 "solid" "yellow"))
                          (overlay (triangle 6 "solid" "red") (triangle 12 "solid" "yellow"))))) 
(define COMET-IMG (star 32 "solid" "red"))  

;; Data definitions:
;====================

(define-struct ship (x y dir))
;; Ship is (make-ship Natural Natural Direction)
;; interp. the user-controlled spaceship, where:
;; - x is the x-coordinate of the ship in pixels
;; - y is the y-coordinate of the ship in pixels
;; - dir is the ship's direction (right, left or none)
(define SI (make-ship (/ WIDTH 2) (* HEIGHT .9) "none"))     ;initial ship position at bottom center
(define S2 (make-ship (* WIDTH 0.6) (* HEIGHT .95) "left"))   ;one possible ship state
#;
(define (fn-for-ship s)
  (... (ship-x s)
       (ship-y s)
       (ship-dir s)))


;; Dir is one of:
;; - "none"
;; - "left"
;; - "right"
;; - "up"
;; - "down"
;; interp. the direction the ship is travelling in. "none" means it moving neither left nor right
(define DL "left")

#; 
(define (fn-for-dir d)
  (cond [(string=? "left" d) (...)]
        [(string=? "right" d) (...)]
        [(string=? "up" d) (...)]
        [(string=? "down" d) (...)]
        [else 
         (...)]))

(define-struct comet (x y vel))
;; Comet is (make-comet Natural Natural Number)
;; interp. a comet careening through the void, where:
;; - x is the comet's x-coordinate in pixels
;; - y is the comet's y-coordinate in pixels
;; - vel is the comet's velocity in pixels per tick
(define C1 (make-comet (/ WIDTH 4) (/ HEIGHT 3) 5))  ; a comet near upper-left corner
                                                     ; of screen moving 5 pixels per tick


#;
(define (fn-for-comet c)
  (... (comet-x c)
       (comet-y c)
       (comet-vel c)))
#;
(define (fn-for-loc loc)
  (cond [(empty? loc) (...)]
        [else
         (... (fn-for-comet (first loc))
              (fn-for-loc (rest loc)))]))


(define-struct game (ship loc score))
;; Game is (make-game Ship (listof Comet) Natural)
;; interp. the state of the game where:
;; - ship is the current state of the ship
;; - loc is the current state of the list of comets
;; - score is the player's current score
(define G1 (make-game
            (make-ship (/ WIDTH 2) 
                       (* HEIGHT .92) 
                       "none")  ; a game with ship near center-bottom and 1 comet on sreen
            (list C1)
            100))
#;
(define (fn-for-game g)
  (... (fn-for-ship (game-ship g))
       (fn-for-loc (game-loc g))
       (... (game-score g))))


;; Functions:
;;========================

;; Game -> Game
;; start the world with initial state s, for example: (main (make-ship (/ WIDTH 2) (/ HEIGHT 2)))
(define (main g)
  (big-bang g                                             ; Game 
            (on-tick     tock-game)                       ; Game -> Game 
            (to-draw     render-game)                     ; Game -> Image 
            (on-key      handle-key)                      ; Game KeyEvent -> Game  
            (on-release  handle-release)                  ; Game KeyEvent -> Game 
            (stop-when   game-over? game-over-display)))  ; Game -> Boolean -> Image 



;; Game -> Game
;; Produce next game state by advancing all comets by vel,  
;; ship by SSPEED, and score by SCORE-RATE

;(define (tock-game g) g)  ;stub
(define (tock-game g)
    (make-game
     (tock-ship (game-ship g)) 
     (filter 
      (lambda (c) (< (comet-y c) HEIGHT)) 
      (add-comets (game-score g) (tock-loc (game-loc g)))) 
     (tock-score (game-score g))))

;; (listof Comet) -> Boolean
;; Produces true if a comet c is overlapping with another

;; Score (listof Comet) -> (listofComet)
;; Randomly adds comets to loc based on given score s
(define (add-comets s loc)
  (if
   (<= (random 1000) 150)
   (cond [(<= s 1000) 
          (cons (make-comet (random WIDTH) 
                            (- 0 (image-height COMET-IMG)) 
                            INIT-CSPEED)  
                loc)]
         [(<= s 2000)
          (cons (make-comet (random WIDTH) 
                            (- 0 (image-height COMET-IMG)) 
                            STAGE2-CSPEED) 
                loc)]
         [(<= s 3000)
          (cons (make-comet (random WIDTH) 
                            (- 0 (image-height COMET-IMG))  
                            STAGE3-CSPEED) 
                loc)]
         [(<= s 4000)
          (cons (make-comet (random WIDTH) 
                            (- 0 (image-height COMET-IMG)) 
                            STAGE4-CSPEED) 
                loc)]
         [else
          (cons (make-comet (random WIDTH) 
                            (- 0 (image-height COMET-IMG)) 
                            STAGE5-CSPEED) 
                loc)])
   loc))

;; Ship -> Ship
;; Produces next ship by advancing it by SSPEED in its current direction
(check-expect (tock-ship (make-ship 100 100 "none")) 
              (make-ship 100 100 "none"))
(check-expect (tock-ship (make-ship 100 100 "right")) 
              (make-ship (+ 100 SSPEED) 100 "right"))
(check-expect (tock-ship (make-ship 100 100 "down")) 
              (make-ship 100 (+ 100 SSPEED) "down"))  
;(define (tock-ship s) s) ;stub 
(define (tock-ship s)
  (cond [(string=? (ship-dir s) "left") 
         (make-ship (- (ship-x s) SSPEED) (ship-y s) (ship-dir s))]
        [(string=? (ship-dir s) "right") 
         (make-ship (+ (ship-x s) SSPEED) (ship-y s) (ship-dir s))]
        [(string=? (ship-dir s) "up") 
         (make-ship (ship-x s) (- (ship-y s) SSPEED) (ship-dir s))]
        [(string=? (ship-dir s) "down") 
         (make-ship (ship-x s) (+ (ship-y s) SSPEED) (ship-dir s))]
        [else
         (make-ship (ship-x s) (ship-y s) (ship-dir s))]))

;; (listof Comet) -> (listof Comet)
;; Produces next loc state by advancing all comets in loc by CSPEED
(check-expect (tock-loc empty) empty)
(check-expect (tock-loc (list (make-comet 100 100 5)))
              (list (make-comet 100 (+ 100 5) 5)))
(check-expect (tock-loc (list (make-comet 100 100 10) (make-comet 200 200 10)))
              (list (make-comet 100 (+ 100 10) 10) (make-comet 200 (+ 200 10) 10)))
;(define (tock-loc loc) empty) ;stub
(define (tock-loc loc)
  (local [(define (tock-comet c)
            (make-comet (comet-x c) 
                        (+ (comet-y c) (comet-vel c))
                        (comet-vel c)))] 
    (map tock-comet loc)))

;; Score -> Score
;; Advances the Score by 1 point per tick
(check-expect (tock-score 0) (+ SCORE-RATE 0))
(check-expect (tock-score 500) (+ SCORE-RATE 500))

(define (tock-score s)
  (+ SCORE-RATE s))

;; Game -> Image
;; Renders the image of the full game state
(check-expect (render-game (make-game
                            (make-ship 300 500 "none")
                            empty
                            0))
              (render-ship (make-ship 300 500 "none") 
                           (render-score 0 (render-loc empty)))) 
(check-expect (render-game (make-game
                            (make-ship 300 500 "none")
                            (list  
                             (make-comet 100 100 10) 
                             (make-comet 400 400 10))
                            100))
              (render-ship (make-ship 300 500 "none")
                           (render-score 100
                                         (render-loc (list
                                                      (make-comet 100 100 10)
                                                      (make-comet 400 400 10))))))
;(define (render-game g) MTS) 
(define (render-game g)
  (render-ship 
   (game-ship g)
   (render-score 
    (game-score g)
    (render-loc (game-loc g)))))


;; Ship Image -> Image
;; Renders image of the ship
(check-expect (render-ship (make-ship 200 500 "none") MTS)
              (place-image SHIP-IMG 200 500 MTS))
(check-expect (render-ship (make-ship 200 500 "left") MTS)
              (place-image MOVING-SHIP-IMG 200 500 MTS))

(define (render-ship s img)
  (if 
   (string=? "none" (ship-dir s))
   (place-image SHIP-IMG
                (ship-x s)
                (ship-y s)
                img)
   (place-image MOVING-SHIP-IMG
                (ship-x s)
                (ship-y s)
                img)))

;; Score Image -> Image
;; Renders image of the current score
(check-expect (render-score 0 MTS)
              (place-image 
               (text "0" 16 "white")
               575
               575
               MTS))
(define (render-score n img)
  (place-image
   (text (number->string n) 16 "white")
   575
   575
   img))

;; (listof Comet) -> Image
;; Renders image of the list of comets
(check-expect (render-loc empty) MTS)
(check-expect (render-loc (list (make-comet 100 100 10)))
              (place-image COMET-IMG 100 100 
                           MTS))
(check-expect (render-loc (list (make-comet 100 100 10)
                                (make-comet 400 400 10)
                                (make-comet 250 250 10)))
              (place-image COMET-IMG 100 100
                           (render-loc (list (make-comet 400 400 10)
                                             (make-comet 250 250 10)))))

;(define (render-loc loc) MTS) ;stub 
(define (render-loc loc)
  (cond [(empty? loc) MTS]
        [else
         (render-comet (first loc)
                       (render-loc (rest loc)))]))

;; Comet Image -> Image
;; Renders the image of a single comet
(check-expect (render-comet (make-comet 100 100 10) MTS) 
              (place-image COMET-IMG 100 100 MTS))
(define (render-comet c img)
  (place-image COMET-IMG
               (comet-x c)
               (comet-y c)
               img)) 

;; Game KeyEvent -> Game
;; Updates ship direction based on key events
(check-expect (handle-key (make-game 
                           (make-ship 300 500 "none")
                           (list
                            (make-comet 100 100 10)
                            (make-comet 400 200 10))
                           0)
                          "up") 
              (make-game (make-ship 300 500 "up")
                         (list
                          (make-comet 100 100 10)
                          (make-comet 400 200 10))
                         0))
(check-expect (handle-key (make-game
                           (make-ship 300 500 "up")
                           (list
                            (make-comet 100 100 15)
                            (make-comet 400 200 15))
                           0)
                          "down")
              (make-game (make-ship 300 500 "down")
                         (list
                          (make-comet 100 100 15)
                          (make-comet 400 200 15))
                         0))

(define (handle-key g ke)
  (cond [(key=? ke "up") (make-game 
                          (make-ship (ship-x (game-ship g))
                                     (ship-y (game-ship g))
                                     "up")
                          (game-loc g)
                          (game-score g))]
        [(key=? ke "down") (make-game 
                            (make-ship (ship-x (game-ship g))
                                       (ship-y (game-ship g))
                                       "down")
                            (game-loc g)
                            (game-score g))]
        [(key=? ke "left") (make-game 
                            (make-ship (ship-x (game-ship g))
                                       (ship-y (game-ship g))
                                       "left")
                            (game-loc g)
                            (game-score g))]
        [(key=? ke "right") (make-game 
                             (make-ship (ship-x (game-ship g))
                                        (ship-y (game-ship g))
                                        "right")
                             (game-loc g)
                             (game-score g))]
        [else g]))


;; Game KeyEvent -> Game
;; Changes ship direction to "none" when keys released
(check-expect (handle-release (make-game
                               (make-ship 300 500 "left")
                               (list
                                (make-comet 100 100 5)
                                (make-comet 400 200 5))
                               0)
                              "up")
              (make-game (make-ship 300 500 "left")
                         (list 
                          (make-comet 100 100 5)
                          (make-comet 400 200 5))
                         0))

(define (handle-release g ke)
  (cond [(key=? ke "up") (if
                          (string=? "up" (ship-dir (game-ship g)))
                          (make-game 
                           (make-ship (ship-x (game-ship g))
                                      (ship-y (game-ship g))
                                      "none")
                           (game-loc g)
                           (game-score g))
                          (make-game
                           (make-ship (ship-x (game-ship g))
                                      (ship-y (game-ship g))
                                      (ship-dir (game-ship g)))
                           (game-loc g)
                           (game-score g)))]
        [(key=? ke "down") (if
                            (string=? "down" (ship-dir (game-ship g)))
                            (make-game 
                             (make-ship (ship-x (game-ship g))
                                        (ship-y (game-ship g))
                                        "none")
                             (game-loc g)
                             (game-score g))
                            (make-game
                             (make-ship (ship-x (game-ship g))
                                        (ship-y (game-ship g))
                                        (ship-dir (game-ship g)))
                             (game-loc g)
                             (game-score g)))]
        [(key=? ke "left") (if
                            (string=? "left" (ship-dir (game-ship g)))
                            (make-game 
                             (make-ship (ship-x (game-ship g))
                                        (ship-y (game-ship g))
                                        "none")
                             (game-loc g)
                             (game-score g))
                            (make-game 
                             (make-ship (ship-x (game-ship g))
                                        (ship-y (game-ship g))
                                        (ship-dir (game-ship g)))
                             (game-loc g)
                             (game-score g)))]
        [(key=? ke "right") (if
                             (string=? "right" (ship-dir (game-ship g)))  
                             (make-game 
                              (make-ship (ship-x (game-ship g))
                                         (ship-y (game-ship g))
                                         "none")
                              (game-loc g)
                              (game-score g))
                             (make-game 
                              (make-ship (ship-x (game-ship g))
                                         (ship-y (game-ship g))
                                         (ship-dir (game-ship g)))
                              (game-loc g)
                              (game-score g)))]
        [else g]))


;; Game -> Boolean
;; Produces true if ship hits left or right screen edge or collides with a comet
(check-expect (game-over? (make-game
                           (make-ship 100 100 "none")
                           (list (make-comet 200 200 10))
                           100))
              false)
(check-expect (game-over? (make-game
                           (make-ship 100 100 "none")
                           (list (make-comet 101 101 10))
                           100))
              true)
(check-expect (game-over? (make-game
                           (make-ship -1 100 "none")
                           (list (make-comet 101 101 10))
                           100))
              true)
(check-expect (game-over? (make-game
                           (make-ship (+ 1 WIDTH) 100 "none")
                           (list (make-comet 101 101 10))
                           100))
              true)

;(define (game-over? g) true) ;stub
(define (game-over? g)
  (local [(define (collision? c)
            (and
             (< 
              (abs (- (ship-x (game-ship g)) (comet-x c))) 
              (/ (image-width COMET-IMG) 2))
             (< 
              (abs (- (ship-y (game-ship g)) (comet-y c))) 
              (/ (image-width COMET-IMG) 2))))]  
    (or
     (< 
      (- (ship-x (game-ship g)) (/ (image-width SHIP-IMG) 2)) 0) 
     (> 
      (+ (ship-x (game-ship g)) (/ (image-width SHIP-IMG) 2)) WIDTH)
     (ormap collision? (game-loc g)))))

(define (game-over-display g)
  (overlay
   (above 
    (text "GAME OVER" 20 "white")
    (text (string-append "Score: " (number->string (game-score g))) 20 "white")) 
   (render-game g))) 


(main (make-game (make-ship 300 550 "none") empty 0))   ; initializes game

