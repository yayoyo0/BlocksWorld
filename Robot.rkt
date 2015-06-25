#lang slideshow

#|
First project
Luis Eduardo Rodríguez Ramírez
A01221156
Programming languages
|#

;Window
(require htdp/draw)
(require lang/posn)
(require racket/gui/base)

;Constants
(define WIDTH 180)
(define HEIGHT 200)
(define BLOCK-SIZE 30)
(define RADIUS 15)
(define BLOCK-DISTANCE 5)

;Columns of the world
(define C1 BLOCK-DISTANCE)
(define C2 (+ BLOCK-DISTANCE BLOCK-SIZE BLOCK-DISTANCE))
(define C3 (+ BLOCK-DISTANCE BLOCK-SIZE BLOCK-DISTANCE BLOCK-SIZE BLOCK-DISTANCE))
(define C4 (+ BLOCK-DISTANCE BLOCK-SIZE BLOCK-DISTANCE BLOCK-SIZE BLOCK-DISTANCE BLOCK-SIZE BLOCK-DISTANCE))
(define C5 (+ BLOCK-DISTANCE BLOCK-SIZE BLOCK-DISTANCE BLOCK-SIZE BLOCK-DISTANCE BLOCK-SIZE BLOCK-DISTANCE BLOCK-SIZE BLOCK-DISTANCE))
;Rows of the world
(define F1 (- HEIGHT BLOCK-SIZE 10))
(define F2 (- HEIGHT BLOCK-SIZE BLOCK-SIZE 10))
(define F3 (- HEIGHT BLOCK-SIZE BLOCK-SIZE BLOCK-SIZE 10))
(define F4 (- HEIGHT BLOCK-SIZE BLOCK-SIZE BLOCK-SIZE BLOCK-SIZE 10))
(define F5 (- HEIGHT BLOCK-SIZE BLOCK-SIZE BLOCK-SIZE BLOCK-SIZE BLOCK-SIZE 10))

;Define column block length
(define C1L 0)
(define C2L 0)
(define C3L 0)
(define C4L 0)
(define C5L 0)

;Positions of static objects
(define origin (make-posn 15 0))
(define centerArm (make-posn 1 30))
(define floorPosition (make-posn 0 (- HEIGHT 10)))

;Block structure
(define-struct block (name x y shape color))

;Blocks
(define A (list ))
(define B (list ))
(define C (list ))
(define D (list ))
(define E (list ))

(define tempPos C1)

;Start the window
(begin (cond ((start WIDTH HEIGHT))))


;Draw robot arm in GUI
(define robot-arm
  (begin
    (draw-solid-rect origin 5 30 'black)
    (draw-solid-rect centerArm 30 5 'black)))

;Draw floor on GUI
(define floor
  (draw-solid-rect floorPosition WIDTH 10 'black))

;Draw cube in GUI
(define drawCube (lambda (item)
                   (cond ((equal? (block-shape item) 'square) (define temp (make-posn (block-x item) (block-y item)))
                                                              (draw-solid-rect temp BLOCK-SIZE BLOCK-SIZE (block-color item))
                                                              (define temp2 (make-posn (+ (block-x item) 10) (+ (block-y item) 20)))
                                                              (draw-solid-string temp2 (parseItem (block-name item))))
                         
                         ((equal? (block-shape item) 'circle) (define temp (make-posn (+ (block-x item) 15) (+ (block-y item) 15)))
                                                              (draw-solid-disk temp RADIUS (block-color item))
                                                              (define temp2 (make-posn (+ (block-x item) 10) (+ (block-y item) 20)))
                                                              (draw-solid-string temp2 (parseItem (block-name item)))))))
;Erase cube from GUI
(define clearCube (lambda (item)
                   (cond ((equal? (block-shape item) 'square) (define temp (make-posn (block-x item) (block-y item)))
                                                              (clear-solid-rect temp BLOCK-SIZE BLOCK-SIZE (block-color item))
                                                              (define temp2 (make-posn (+ (block-x item) 10) (+ (block-y item) 20)))
                                                              (clear-solid-string temp2 (parseItem (block-name item))))
                         ((equal? (block-shape item) 'circle) (define temp (make-posn (+ (block-x item) 15) (+ (block-y item) 15)))
                                                              (clear-solid-disk temp RADIUS (block-color item))
                                                              (define temp2 (make-posn (+ (block-x item) 10) (+ (block-y item) 20)))
                                                              (clear-solid-string temp2 (parseItem (block-name item)))))))

;Parse the name of the block to print it in the canvas
(define parseItem (lambda (item)
                    (cond ((equal? item 'A) "A")
                          ((equal? item 'B) "B")
                          ((equal? item 'C) "C")
                          ((equal? item 'D) "D")
                          ((equal? item 'E) "E"))))

;Parse user input to defined columns
(define parseInput (lambda (input)
                     (cond ((equal? input 'C1) C1)
                           ((equal? input 'C2) C2)
                           ((equal? input 'C3) C3)
                           ((equal? input 'C4) C4)
                           ((equal? input 'C5) C5)
                           (else C1))))

;Checks the last row available
(define checkRow (lambda (column)
                   (cond ((equal? C1 column) (define temp (- HEIGHT (+ (* C1L BLOCK-SIZE) BLOCK-SIZE 10))) (set! C1L (+ C1L 1)) temp)
                         ((equal? C2 column) (define temp (- HEIGHT (+ (* C2L BLOCK-SIZE) BLOCK-SIZE 10))) (set! C2L (+ C2L 1)) temp)
                         ((equal? C3 column) (define temp (- HEIGHT (+ (* C3L BLOCK-SIZE) BLOCK-SIZE 10))) (set! C3L (+ C3L 1)) temp)
                         ((equal? C4 column) (define temp (- HEIGHT (+ (* C4L BLOCK-SIZE) BLOCK-SIZE 10))) (set! C4L (+ C4L 1)) temp)
                         ((equal? C5 column) (define temp (- HEIGHT (+ (* C5L BLOCK-SIZE) BLOCK-SIZE 10))) (set! C5L (+ C5L 1)) temp))))

;Finds an empty column
(define findSpace (lambda () 
                    (cond 
                      ((equal? C1L 0) C1)
                      ((equal? C2L 0) C2)
                      ((equal? C3L 0) C3)
                      ((equal? C4L 0) C4)
                      ((equal? C5L 0) C5))))

;Finds if there is something above the blocks trying to be moved
(define onTop(lambda (block)
               (define column (block-x block))
               (define row (block-y block))
               (define rowAbove (- row BLOCK-SIZE))
               (cond ((and (equal? column (block-x A)) (equal? rowAbove (block-y A))) A)
                     ((and (equal? column (block-x B)) (equal? rowAbove (block-y B))) B)
                     ((and (equal? column (block-x C)) (equal? rowAbove (block-y C))) C)
                     ((and (equal? column (block-x D)) (equal? rowAbove (block-y D))) D)
                     ((and (equal? column (block-x E)) (equal? rowAbove (block-y E))) E)
                     (else '()))))

;Parse the onTop blocks into a list to be processed
(define onTopList (lambda (block)
                    (define temp '())
                    (cond ((null? block) temp)
                          (else (reverse (cons block (onTopList (onTop block))))))))

;Move block in GUI
(define moveBlock (lambda (block column row)
                    (clearCube block)
                    (cond ((equal? (block-x block) C1)(set! C1L (- C1L 1)))
                          ((equal? (block-x block) C2)(set! C2L (- C2L 1)))
                          ((equal? (block-x block) C3)(set! C3L (- C3L 1)))
                          ((equal? (block-x block) C4)(set! C4L (- C4L 1)))
                          ((equal? (block-x block) C5)(set! C5L (- C5L 1))))
                    
                    (define temp (make-block (block-name block) column row (block-shape block) (block-color block)))
                    (cond ((equal? block A) (set! A temp))
                          ((equal? block B) (set! B temp))
                          ((equal? block C) (set! C temp))
                          ((equal? block D) (set! D temp))
                          ((equal? block E) (set! E temp)))
                    (drawCube temp)))

;Helper function to move blocks
(define moveHelper (lambda (list)
                     (cond ((null? list) true) 
                     (else (let ([column (findSpace)]) (moveBlock (car list) column (checkRow column))) 
                           (sleep-for-a-while 2)
                           (moveHelper (cdr list))))))

;Move block to another position with the input of the user
(define putOn(lambda (from to)
               (cond ((equal? (block-shape to) 'circle) (display "Unable to perform the movement"))
                     (else (cond ((and (null? (onTop to)) (null? (onTop from)))
                                  (moveBlock from (block-x to) (checkRow (block-x to))))
                                 ((not (null? (onTop from))) (moveHelper (onTopList (onTop from))) (putOn from to))
                                 ((not (null? (onTop to))) (moveHelper (onTopList (onTop to))) (putOn from to))
                                 (((and 
                                   (not (null? (onTop from)))
                                   (not (null? (onTop to)))))
                                  (moveBlock from (block-x to) (checkRow (block-x to))))
                                 (else (putOn from to)))))))

;Process the user input to specify the position of the blocks
(define blockInit (lambda () 
                    (for ([i 5])
                      (display "In which column do you want to place the figure? (C1-C5)")
                      (set! tempPos (read))
                      (cond ((= i 0)
                             (let ([column (parseInput tempPos)]) (set! A (make-block 'A column (checkRow column) 'square 'green)))
                             (drawCube A))
                            
                            ((= i 1)
                             (let ([column (parseInput tempPos)]) (set! B (make-block 'B column (checkRow column) 'square 'red)))
                             (drawCube B))
                            
                            ((= i 2)
                             (let ([column (parseInput tempPos)]) (set! C (make-block 'C column (checkRow column) 'square 'blue)))
                             (drawCube C))
                            
                            ((= i 3)
                             (let ([column (parseInput tempPos)]) (set! D (make-block 'D column (checkRow column) 'square 'orange)))
                             (drawCube D))
                            ((= i 4)
                             (let ([column (parseInput tempPos)]) (set! E (make-block 'E column (checkRow column) 'circle 'yellow)))
                             (drawCube E))))))

;Initialization function
(define init (lambda ()
               (begin
                  robot-arm
                  floor
                  (blockInit))))

(init)