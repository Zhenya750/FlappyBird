#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require racket/trace)

;;----------------------------------------------------
;;              Structures and images
;;----------------------------------------------------

(define WIDTH 700)
(define HEIGHT 480)

;Point for any object
(struct point
  (x y vx vy)
  #:transparent)

;IMAGES
;resizes image img to w-width, h-height
(define (image-resize w h img)
  (let ((scale-w (/ w (image-width img)))
        (scale-h (/ h (image-height img))))
    (scale/xy scale-w scale-h img)))


;hero image
(define bird-img
  (image-resize (/ WIDTH 13) (/ HEIGHT 9)
                (bitmap "images/hero_christmas.png")))


;Background image
(define background-img
  (image-resize WIDTH HEIGHT
                (bitmap "images/forest_vector_2.JPG")))


;Wood block image
(define wood-block-img
  (image-resize 100 HEIGHT
                (bitmap "images/mytree.png")))


(struct world
  (hero     ;player
   woods    ;obstacles for the player
   speed    ;speed along OX for each wood
   counter) ;counts overcomed obstacles
   #:transparent)


;;----------------------------------------------------
;;                   Woods list
;;----------------------------------------------------

(define wood-img-width (image-width wood-block-img))
;GENERATES list of points(woods) like: '((pt1 pt2 pt3) (pb1 pb2 pb3))
;n - number of woods
;width-between - distance between woods
;height-between - distance between wood blocks
;startX - x coordinate to start from
(define (gen-woods n width-between height-between startX)
  (define (iter i top bottom)
    (let* ((ybottom (random height-between HEIGHT))
           (ytop (- ybottom height-between))
           (x (+ startX (* width-between i 2))))
      (if (= i n)
          (list top bottom)
          (iter (+ i 1)
                (cons (point x ytop 0 0) top)
                (cons (point x ybottom 0 0) bottom)))))
  (iter 0 '() '()))


;DRAWS all the woods from '((pt1 pt2 pt3) (pb1 pb2 pb3))
;on the scene
;returns updated scene
(define (draw-woods woods scene)
  (define (draw-wood i woods scene)
    (if (= i 2)
        scene
        (draw-wood (+ i 1)
                   (cdr woods)
                   (foldl (λ (p1 p2)
                            (place-image/align
                             (overlay/align "middle" (if (= i 0) "bottom" "top")
                                            wood-block-img
                                            (rectangle wood-img-width 500 "solid" (color 255 255 255 0)))
                             (point-x p1) (point-y p1)
                             "left" (if (= i 0) "bottom" "top")
                             p2))
                          scene
                          (car woods)))))
  (draw-wood 0 woods scene))


;REMOVES a wood if it is outside
;Pushs a new wood at the right side
;returns updated list of woods
(define (remove+add woods)
  ;Checks individual point if it is outside
  ;returns boolean
  (define (is-shown? pnt wood-width)
    (> (+ (point-x pnt) wood-width) 0))
  (let ((filtered-woods (map (λ l
                               (filter (λ c
                                         (is-shown? (car c) wood-img-width))
                                       (car l)))
                             woods)))
    (map (λ (a b) (append a b))
         filtered-woods
         (if (< (length (car filtered-woods))
                (length (car woods)))
             (gen-woods 1 wood-img-width 200 WIDTH)
             (list '() '())))))


;MOVES woods across the horizontal
;changes x coordinate adding vx
(define (move woods vx)
  (map (λ l
         (for/list ((i (car l)))
           (struct-copy point i (x (+ (point-x i) vx)))))
       woods))

;;----------------------------------------------------
;;                     Game
;;----------------------------------------------------

;DEFAULT world
(define world0
  (world (point (/ WIDTH 6) (/ HEIGHT 2) 0 0)
         (gen-woods 4 100 200 (/ WIDTH 2))
         -2
         0))


;DRAWS COUNTER
(define (draw-counter counter scene)
  (place-image/align (text (number->string counter) 32 "black")
                     10 10
                     "left"
                     "top"
                     scene))


;UPDATE COUNTER
(define (update-counter w)
  (let ((hero (world-hero w))
        (counter (world-counter w))
        (woods (world-woods  w))
        (speed (world-speed w)))
    (if (for*/or ((lst woods)
                  (wood lst))
          (= (floor (+ (point-x wood) wood-img-width))
             (floor (+ (point-x hero)))))
        (struct-copy world w (counter (+ counter 1)))
        w)))


;DRAWS THE WORLD
(define (draw w)
  (let* ((hero (world-hero w))
         (woods (world-woods w))
         (x (/ WIDTH 6))
         (y (+ (point-y hero) (point-vy hero)))
         (vx (point-vx hero))
         (angle (radians->degrees (atan (/ (- 200 (+ vx vx)) 150))))
         (counter (world-counter w)))
    (place-image (rotate angle bird-img)
                 x y
                 (draw-counter counter
                               (draw-woods woods
                                           background-img)))))

;KEY-PRESSED HANDLER
(define (ke-pressed w ke)
  (let* ((hero (world-hero w))
         (x (point-x hero))
         (y (+ (point-y hero) (point-vy hero)))
         (vx 0)
         (vy 0))
    (cond
      ((key=? ke "up")
       (struct-copy world w (hero (point x y vx vy)))) 
      (else w))))


;TICK CONTROL
(define (tick-ctrl w)
  (let* ((hero (world-hero w))
         (speed (world-speed w))
         (woods (remove+add (move (world-woods w) speed)))
         (counter (world-counter w))
         (vx (+ (point-vx hero) 4))
         (vy (/ (* vx (- vx 200)) 150)))
    (update-counter
     (struct-copy world w
                  (hero (struct-copy point hero (vx vx) (vy vy)))
                  (woods woods)))))



;GAMEOVER when hero is under the screen
(define (out-of-bounds? w)
  (let ((hero (world-hero w)))
    (> (+ (point-vy hero) (point-y hero)) HEIGHT)))



;COLLISION detection
;comparison via circle from hero's center 
(define (collision? w)
  (let* ((hero (world-hero w))
         (x0 (point-x hero))
         (y0 (+ (point-y hero) (point-vy hero)))
         (r 5)
         (n 6)
         (angle (/ (+ pi pi) n)))
    ;wt - top wood
    ;wb - bottom wood
    (define (check i wt wb)
      (let ((x (+ (* r (cos (* i angle))) x0))
            (y (+ (* r (sin (* i angle))) y0)))
        (if (> i n)
            false
            (if (or
                 (and (and (>= x (point-x wt))
                           (<= x (+ (point-x wt) wood-img-width)))
                      (<= y (point-y wt)))
                 (and (and (>= x (point-x wb))
                           (<= x (+ (point-x wb) wood-img-width)))
                      (>= y (point-y wb))))
                true
                (check (+ i 1) wt wb)))))
    (for/or ((wt (car (world-woods w)))
             (wb (cadr (world-woods w))))
      (check 0 wt wb))))
  


;GAMEOVER DEFAULT world
(define (last-world w)
  (let ((counter (world-counter w)))
    (place-image(above (text "Game over" 32 "black")
                       (text (string-append "score: "
                                            (number->string counter))
                             32 "black"))
                (/ WIDTH 2) (/ HEIGHT 2)
                background-img)))


;LAUNCH the game
(big-bang world0
  (to-draw draw)
  (on-key ke-pressed)
  (on-tick tick-ctrl 0.015)
  (stop-when (λ w (or (collision? (car w))
                      (out-of-bounds? (car w))))
             last-world))