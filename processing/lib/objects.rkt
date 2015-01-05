(module processing/objects racket

  (provide (all-defined-out))

  (require "constants.rkt")

  (define PVector
    (class object%
           (init-field [x 0] 
                  [y 0]
                  [z 0])

            (define/public (set x [y 0] [z 0])
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
                                 (set-field! y this y)
                                 (set-field! z this z))]))

            (define/public (random2D [v null])
                           (fromAngle (* (random) TWO_PI) v))

            (define/public (random3D [v null])
                           (let* ([ang (* (random) TWO_PI)]
                                  [vz  (- (* (random) 2) 1)]
                                  [mult (sqrt (- 1 (* vz vz)))]
                                  [vx (* mult (cos ang))]
                                  [vy (* mult (sin ang))])
                             (if (null? v)
                               (make-object PVector vx vy vz)
                               (begin 
                                 (set-field! x v vx)
                                 (set-field! y v vy)
                                 (set-field! z v vz)
                                 v))))


           (define/public (fromAngle ang [v null])
                          (let ([x-val (cos ang)] 
                                [y-val (sin ang)])
                            (if (null? v)
                              (make-object PVector x-val y-val)
                              (begin 
                                (set-field! x v x-val)
                                (set-field! y v y-val)
                                v))))
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

          ; (define/public (setMag v [len null])
          ;                (if (null? len)
          ;                  ))
                             

            (define/public (add x [y null] [z null])
                           (cond
                             [(and (is-a? x PVector) (null? y))
                              (begin
                                (set-field! x this (+ x (get-field x x)))
                                (set-field! y this (+ y (get-field y x)))
                                (set-field! z this (+ z (get-field z x))))]
                             [else 
                               (begin 
                                 (set-field! x this (+ x (get-field x this)))
                                 (set-field! y this (+ y (get-field y this)))
                                 (set-field! z this (+ z (get-field z this))))]))

            (define/public (sub x [y null] [z null])
                           (cond
                             [(and (is-a? x PVector) (null? y))
                              (begin
                                (set-field! x this (- x (get-field x x)))
                                (set-field! y this (- y (get-field y x)))
                                (set-field! z this (- z (get-field z x))))]
                             [else 
                               (begin 
                                 (set-field! x this (- x (get-field x this)))
                                 (set-field! y this (- y (get-field y this)))
                                 (set-field! z this (- z (get-field z this))))]))

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

            (define/public (cross v)
                           (let ([v-x (get-field v x)]
                                 [v-y (get-field v y)]
                                 [v-z (get-field v y)])
                           (make-object PVector
                                        (- (* y v-z) (* v-y z))
                                        (- (* z v-x) (* v-z x))
                                        (- (* x v-y) (* v-x y)))))

            (define/public (normalize)
                           (let ([m (mag)])
                             (if (> m 0)
                               (div m)
                               (void))))

            (define/public (limit high)
                           (if (> (mag) high)
                             (normalize)
                             (mult high)))

            (define/public (heading)
                           (- (atan (- y) x)))

            (define/public (heading2D)
                           (heading))

            (define/public (toString)
                           (format "[~a, ~a, ~a]" x y z))

            (define/public (array)
                           (vector x y z))

            (super-instantiate ())))

  )
