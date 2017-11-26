#lang racket

(require racket/draw racket/gui)
(define frame (new frame% [label "Sierpinski Triangle"]))
;(define canvas
;  (new canvas% [parent frame]
;       [paint-callback
;        (lambda (canvas dc)
;                (render-triangle dc))]))
(define canvas(new canvas% [parent frame]))
(define dc (send canvas get-dc))
(define blue-pen (make-object pen% "BLUE" 0.02 'solid))

(define (generate-triangle-vertex-sequence x-size
                                           y-size
                                           current-x
                                           current-y
                                           discard-threshold
                                           amount-to-generate
                                           generated-vertex-count
                                           vertex-list)
  (let*-values ([(x-size y-size) (send frame get-client-size)]
                [(lower-left-x)  (* x-size 0.05)]
                [(lower-left-y)  (- y-size (* y-size 0.05))]
                [(lower-right-x) (- x-size (* x-size 0.05))]
                [(lower-right-y) (- y-size (* y-size 0.05))]
                [(apex-x)        (/ x-size 2)]
                [(apex-y)        (- y-size (* (/ (sqrt 3) 2)
                                              (- x-size
                                                 (* x-size 0.1))))])
        (if (<= generated-vertex-count amount-to-generate)
        (if (>  generated-vertex-count discard-threshold)
             (let* ([vertex          (random 1 4)]
                    [new-vertex-list (cond
                                       [(eq? vertex 1) (cons
                                                        (cons (/ (+ lower-left-x  current-x) 2)
                                                              (/ (+ lower-left-y  current-y) 2))
                                                        vertex-list)]
                                       [(eq? vertex 2) (cons
                                                        (cons (/ (+ lower-right-x current-x) 2)
                                                              (/ (+ lower-right-y current-y) 2))
                                                        vertex-list)]
                                       [(eq? vertex 3) (cons 
                                                        (cons (/ (+ apex-x        current-x) 2)
                                                              (/ (+ apex-y        current-y) 2))
                                                        vertex-list)])])
               (generate-triangle-vertex-sequence
                x-size
                y-size
                (caar new-vertex-list)
                (cdar new-vertex-list)
                discard-threshold
                amount-to-generate
                (+ 1 generated-vertex-count)
                new-vertex-list))
             (generate-triangle-vertex-sequence
              x-size
              y-size
              0 
              0
              discard-threshold
              amount-to-generate
              (+ 1 generated-vertex-count)
              vertex-list))
        vertex-list)))

(define (render-triangle dc
                         vertex-list)
  (send dc set-pen blue-pen)
  (for ([vertex vertex-list])
    (send dc draw-point (car vertex) (cdr vertex))
    ;(sleep/yield 0.001)
    ))
            

(define (run)
  (let*-values ([(x-size y-size) (send frame get-client-size)])
    (send frame show #t)
    (sleep/yield 1)
    (render-triangle
     dc
     (generate-triangle-vertex-sequence
      x-size
      y-size
      0
      0
      1000
      30000
      0
      '()))))


