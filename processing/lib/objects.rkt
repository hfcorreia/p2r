#lang racket/base

(provide (all-defined-out))

(require racket/class
         "constants.rkt")

(define PVector
  (class object%
         (init-field [x 0] 
                     [y 0]
                     [z 0])

         (define/public (set x [y null] [z null])
                        (cond 
                          [(vector? x) 
                           (begin 
                             (set-field! x this (vector-ref x 0))
                             (set-field! y this (vector-ref x 1))
                             (set-field! z this (vector-ref x 2)))]
                          [(is-a? x PVector)
                           (begin 
                             (set-field! x this (get-field x x))
                             (set-field! y this (get-field y x))
                             (set-field! z this (get-field z x)))]
                          [else 
                            (begin 
                              (set-field! x this x)
                              (and (not (null? y)) (set-field! y this y))
                              (and (not (null? z)) (set-field! z this z)))]))

         ; (define/public (random2D [v null])
         ;                (fromAngle (* (random) TWO_PI) v))

         ; (define/public (random3D [v null])
         ;                (let* ([ang (* (random) TWO_PI)]
         ;                       [vz  (- (* (random) 2) 1)]
         ;                       [mult (sqrt (- 1 (* vz vz)))]
         ;                       [vx (* mult (cos ang))]
         ;                       [vy (* mult (sin ang))])
         ;                  (if (null? v)
         ;                    (make-object PVector vx vy vz)
         ;                    (begin 
         ;                      (set-field! x v vx)
         ;                      (set-field! y v vy)
         ;                      (set-field! z v vz)
         ;                      v))))


         ; (define/public (fromAngle ang [v null])
         ;                (let ([x-val (cos ang)] 
         ;                      [y-val (sin ang)])
         ;                  (if (null? v)
         ;                    (make-object PVector x-val y-val)
         ;                    (begin 
         ;                      (set-field! x v x-val)
         ;                      (set-field! y v y-val)
         ;                      v))))

         (define/public (get)
                        (make-object PVector x y z))

         (define/public (mag)
                        (sqrt (+ (* x x)
                                 (* y y)
                                 (* z z))))
         (define/public (magSq)
                        (+ (* x x)
                           (* y y)
                           (* z z)))



         (define/public (add x [y null] [z null])
                        (cond
                          [(and (is-a? x PVector) (null? y))
                           (begin
                             (set-field! x this (+ (get-field x this) 
                                                   (get-field x x)))
                             (set-field! y this (+ (get-field y this) 
                                                   (get-field y x)))
                             (set-field! z this (+ (get-field z this)
                                                   (get-field z x))))]
                          [else 
                            (begin 
                              (set-field! x this (+ x (get-field x this)))
                              (set-field! y this (+ y (get-field y this)))
                              (set-field! z this (+ z (get-field z this))))]))

         (define/public (sub x [y null] [z null])
                        (cond
                          [(and (is-a? x PVector) (null? y))
                           (begin
                             (set-field! x this (- (get-field x this)
                                                   (get-field x x)))
                             (set-field! y this (- (get-field y this)
                                                   (get-field y x)))
                             (set-field! z this (- (get-field z this)
                                                   (get-field z x))))]
                          [(and (not (null? y)) (not (null? z)))
                           (begin 
                             (set-field! x this (- (get-field x this) x))
                             (set-field! y this (- (get-field y this) y))
                             (set-field! z this (- (get-field z this) z)))]))

         (define/public (mult n)
                        (set-field! x this (* x n))
                        (set-field! y this (* y n))
                        (set-field! z this (* z n)))

         (define/public (div n)
                        (set-field! x this (/ x n))
                        (set-field! y this (/ y n))
                        (set-field! z this (/ z n)))

         (define/public (rotate ang)
                        (let ([c (cos ang)]
                              [s (sin ang)])
                          (set-field! y this (+ (* s x)
                                                (* c y)))
                          (set-field! x this (- (* c x)
                                                (* s y)))))

         (define/public (dist v)
                        (let ([dx (- x (get-field x v))]
                              [dy (- y (get-field y v))]
                              [dz (- z (get-field z v))])
                          (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))

         (define/public (dot x [y null] [z null])
                        (if (is-a? x PVector)
                          (+ (* (get-field x x) (get-field x this))
                             (* (get-field y x) (get-field y this))
                             (* (get-field z x) (get-field z this)))
                          (+ (* x (get-field x this))
                             (* y (get-field y this))
                             (* z (get-field z this)))))

         (define/public (cross v [res null])
                        (let ([v-x (get-field x v)]
                              [v-y (get-field y v)]
                              [v-z (get-field z v)])
                          (cond 
                            [(null? res)
                             (make-object PVector
                                          (- (* y v-z) (* v-y z))
                                          (- (* z v-x) (* v-z x))
                                          (- (* x v-y) (* v-x y)))]
                            [(is-a? res PVector)
                             (begin
                               (send res 
                                     set 
                                     (- (* y v-z) (* v-y z))
                                     (- (* z v-x) (* v-z x))
                                     (- (* x v-y) (* v-x y)))
                               res)])))

         (define/public (normalize [v null])
                        (let ([m (if (null? v) (mag) (send v mag))])
                          (cond
                            [(and (null? v) (not (eq? 0 m)) (not (eq? 1 m)))
                             (div m)]
                            [(and (not (null? v)) (> m 0))
                             (begin (send v div m) v)]
                            [(and (not (null? v)) (< m 0))
                             v])))

         (define/public (limit high)
                        (if (> (magSq) (* high high))
                          (normalize)
                          (mult high)))

         (define/public (setMag len [target null])
                        (if (null? target)
                          (begin 
                            (normalize)
                            (mult len))
                          (begin 
                            (send len normalize)
                            (send len mult target)
                            len)))

         (define/public (heading)
                        (- (atan (- y) x)))

         (define/public (heading2D)
                        (heading))

         (define/public (lerp x y [z null] [amt null])
                        (define (lerp-aux x y z amt)
                          (define (lerp-val start stop amt)
                            (+ start (* amt (- stop start))))
                          (set-field! x this (lerp-val (get-field x this) x amt))
                          (set-field! y this (lerp-val (get-field y this) y amt))
                          (set-field! z this (lerp-val (get-field z this) z amt)))

                        (if (and (null? z) (null? amt))
                          (let ([amt y]
                                [x (get-field x x)]
                                [y (get-field y x)]
                                [z (get-field z x)])
                            (lerp-aux x y z amt))
                          (lerp-aux x y z amt)))


         (define/public (toString)
                        (format "[~a, ~a, ~a]" x y z))

         (define/public (array)
                        (vector x y z))

         (super-instantiate ())))

