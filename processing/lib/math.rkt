(module processsing/math racket

  (provide (all-defined-out))


  (require racket/math
           (rename-in racket/base 
                      [max orig-max]
                      [min orig-min]
                      [random orig-rand]
                      [map orig-map]))

  (define (ceil x)
    (ceiling x))

  (define (constrain val min max)
    (cond 
      [(< val min) min]
      [(> val max) max]
      [else val]))

  (define-syntax dist
    (syntax-rules ()
      [(_ x1 y1 x2 y2) 
       (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))]
      [(_ x1 y1 z1 x2 y2 z2)
       (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2) (expt (- z2 z1) 2)))]))

  (define-syntax max
    (syntax-rules ()
      [(_ array)
       (apply orig-max (vector->list array))]
      [(_ elem elem2 ...)
       (orig-max elem elem2 ...)]))

  (define-syntax min 
    (syntax-rules ()
      [(_ array)
       (apply orig-min (vector->list array))]
      [(_ elem elem2 ...)
       (orig-min elem elem2 ...)]))

  (define (pow b e)
    (expt b e))

  (define (sq n)
    (* n n))

  (define (radian ang)
    (degrees->radians ang))

  (define (degrees ang)
    (radians->degrees ang))

  (define-syntax random
    (syntax-rules ()
      [(_ num) 
       (+ (orig-rand (- (inexact->exact (truncate num)) 1))
          (orig-rand (make-pseudo-random-generator)))]
      [(_ min max)
       (if (> min max)
         min
         (+ min (+ (orig-rand (inexact->exact (truncate (- max min))))
                   (orig-rand (make-pseudo-random-generator)))))]))

  (define (randomSeed seed)
    (random-seed seed))

  (define (lerp start stop amt)
    (+ (* (- stop start) amt) start))

  (define-syntax mag
    (syntax-rules ()
      [(_ a b)
       (sqrt (+ (sq a) (sq b)))]
      [(_ a b c)
       (sqrt (+ (sq a) (sq b) (sq c)))]))

  (define-syntax map 
    (syntax-rules ()
      [(_ value start1 stop1 start2 stop2)
       (+ start2 
          (/ (* (- stop2 start2) 
                (- value start1)) 
             (- stop1 start1)))]
      [(_ func lst ...)
       (orig-map func lst ...)]))

  (define (norm value start stop)
    (/ (- value stop) (- start stop)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  ;;; randomGaussian
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

  (define next-gaussian null)

  (define (randomGaussian)
    (define (calculate-random n)
      (sqrt (* -2.0 (/ (log n) n))))
    (let ([x1 null]
          [x2 null]
          [w null])
      (if (not (null? next-gaussian))
        (begin
          (set! w next-gaussian)
          (set! next-gaussian null)
          w)
        (let loop ()
          (set! x1 (- (* 2.0 (orig-rand)) 1))
          (set! x2 (- (* 2.0 (orig-rand (make-pseudo-random-generator))) 1))
          (set! w  (+ (sq x1) (sq x2)))
          (if (>= w 1.0)
            (loop)
            (let ([val (calculate-random w)])
              (set! next-gaussian (* x2 w))
              (* x1 w)))))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  ;;; Noise
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

  (define octaves 4)
  (define fallout 0.5)
  (define seed (orig-rand 200000))

  (define (generate-permutations)
    (define p (make-vector 512))
    (random-seed (orig-rand 200000))
    (for ([i 256])
      (vector-set! p i i))
    (for ([i 256])
      (let* ([j  (orig-rand 256)]
            [t (vector-ref p j)])
      (vector-set! p j (vector-ref p i))
      (vector-set! p i t)))
    (for ([i 256])
      (vector-set! p (+ i 256) (vector-ref p i)))
    p)
  
      
  (define (f elem)
    (* elem elem (- 3 (* 2 elem))))

  (define (noise1D x)
    (define perm (generate-permutations))
    (define (grad1D i x)
      (if (zero? (bitwise-and i 1)) (- x) x))
    (let* ([X  (bitwise-and (exact-floor x) 255)]
           [x  (- x (floor x))])
      (lerp 
            (grad1D (vector-ref perm X) x)
            (grad1D (vector-ref perm (+ X 1)) (- x 1))
            (f x))))

  (define (noise2D x y)
    (define perm (generate-permutations))
    (define (grad2D i x y)
      (let ([v (if (zero? (bitwise-and i 1)) x y)])
        (if (zero? (bitwise-and i 2)) (- v) v)))
    (let* ([X  (bitwise-and (exact-floor x) 255)]
           [Y  (bitwise-and (exact-floor y) 255)]
           [x  (- x (floor x))]
           [y  (- y (floor y))]
           [p0 (+ (vector-ref perm X) Y)]
           [p1 (+ (vector-ref perm (+ X 1)) Y)])
      (lerp 
        (lerp
          (grad2D (vector-ref perm p0) x y)
          (grad2D (vector-ref perm p1) (- x 1) y)
          (f x))
        (lerp 
          (grad2D (vector-ref perm (+ p0 1)) x (- y 1))
          (grad2D (vector-ref perm (+ p1 1)) (- x 1) (- y 1))
          (f x))
        (f y))))

  (define (noise3D x y z)
    (define perm (generate-permutations))
    (define (grad3D i x y z)
      (let* ([h (bitwise-and i 15)]
            [u (if (< h 8) x y)]
            [v (cond [(< h 4)y]
                    [(or (eq? h 12) (eq? h 14)) x]
                    [else z])])
        (+ (if (zero? (bitwise-and h 1)) u (- u))
           (if (zero? (bitwise-and h 2)) v (- v)))))
    (let* ([X  (bitwise-and (exact-floor x) 255)]
           [Y  (bitwise-and (exact-floor y) 255)]
           [Z  (bitwise-and (exact-floor z) 255)]
           [x  (- x (floor x))]
           [y  (- y (floor y))]
           [z  (- z (floor z))]
           [p0 (+ (vector-ref perm X) Y)]
           [p00 (+ (vector-ref perm p0) Z)]
           [p01 (+ (vector-ref perm (+ 1 p0)) Z)]
           [p1  (+ (vector-ref perm (+ X 1)) Y)]
           [p10 (+ (vector-ref perm p1) Z)]
           [p11 (+ (vector-ref perm (+ p1 1)) Z)])
      (lerp 
        (lerp 
          (lerp
            (grad3D (vector-ref perm p00) x y z)
            (grad3D (vector-ref perm p10) (- x 1) y z)
            (f x))
          (lerp 
            (grad3D (vector-ref perm p01) x (- y 1) z)
            (grad3D (vector-ref perm p11) (- x 1) (- y 1) z)
            (f x))
          (f y))
        (lerp 
          (lerp 
            (grad3D (vector-ref perm (+ 1 p00)) x y (- z 1))
            (grad3D (vector-ref perm (+ 1 p10)) (- x 1) y (- z 1))
            (f x))
          (lerp 
            (grad3D (vector-ref perm (+ 1 p01)) x (- y 1) (- z 1))
            (grad3D (vector-ref perm (+ 1 p11)) (- x 1) (- y 1)(- z 1))
            (f x))
          (f y))
        (f z))))

  (define (1D x)
    (let ([k 0.5]
          [effect 1])
      (for/fold ([sum 0])
                ([i octaves])
                (set! effect (* effect fallout))
                (set! k (* k 2))
        (displayln (* effect (/ (+ 1 (noise1D (* k x)))2)))
                (+ sum (* effect (/ (+ 1 (noise1D (* k x)) 2)))))))

  (define (2D x y)
    (let ([k 0.5]
          [effect 1])
      (for/fold ([sum 0])
                ([i octaves])
                (set! effect (* effect fallout))
                (set! k (* k 2))
                (+ sum (/ (+ 1 (noise2D (* k x) (* k y)) 2))))))

  (define (3D x y z)
    (let ([k 0.5]
          [effect 1])
      (for/fold ([sum 0])
                ([i octaves])
                (set! effect (* effect fallout))
                (set! k (* k 2))
        (displayln (/ (+ 1 (noise3D (* k x) (* k y) (* k z)))2))
                (+ sum 1))))


  (define (noiseDetail oct fall)
    (set! octaves oct)
    (if (not (null? fall))
      (set! fallout fall)
      (void)))

  ;(define (noiseSeed s)
   ; (set! seed s)
   ; (set! generator null))

  (define-syntax noise
    (syntax-rules ()
      [(_ x) (1D x)]
      [(_ x y) (2D x y)]
      [(_ x y z) (3D x y z)]))


  )
