#lang racket/base

(provide (except-out (all-defined-out)
                     current-random-generator
                     int-generator
                     double-generator
                     noise1D
                     noise2D
                     noise3D
                     fade
                     generate-permutations
                     seed
                     fallout
                     octaves
                     next-gaussian))

(require racket/math
         "../bindings.rkt"
         "../ast/types.rkt"
         (rename-in racket/base
                    [abs orig-abs]
                    [max orig-max]
                    [min orig-min]
                    [random orig-rand]
                    [map orig-map]))


(define-types (abs null (create-type 'float) null [(create-type 'float) n])
  (orig-abs n))

(define-types (ceil null (create-type 'int) null [(create-type 'float) x])
  (inexact->exact (ceiling x)))

(define-types (constrain null (create-type 'float) null
                         [(create-type 'float) val]
                         [(create-type 'float) min]
                         [(create-type 'float) max])
              (cond
                [(< val min) min]
                [(> val max) max]
                [else val]))

(define-types (dist null (create-type 'float) null
                    [(create-type 'float) x1]
                    [(create-type 'float) y1]
                    [(create-type 'float) x2]
                    [(create-type 'float) y2])
                (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))

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


(define (lerp a b t)
  (+  a (* t (- b a))))

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

(define (norm value low high)
  (/ (- value low) (- high low)))

(define (atan2 x y)
  (atan x y))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; random
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define current-random-generator
  (lambda () (orig-rand (make-pseudo-random-generator))))

(define (int-generator [i1 null] [i2 null])
  (let ([a (if (null? i1) 362436069 i1)]
        [b (if (null? i2) 521288629 i2)])
    (define (shift left right)
      (+ (arithmetic-shift left (- right))
         (arithmetic-shift 2 (bitwise-not right))))
    (let ([z (bitwise-and  (+ (*  36969 (bitwise-and a 65535) (shift a 16)))
                           #xFFFFFFFF)]
          [w (bitwise-and  (+ (*  18000 (bitwise-and b 65535) (shift b 16)))
                           #xFFFFFFFF)])
      (bitwise-and (bitwise-ior
                     (arithmetic-shift (bitwise-and z  #xFFFF) 16)
                     (bitwise-and w #xFFFF))
                   #xFFFFFFFF))))

(define (double-generator [i1 null] [i2 null])
  (lambda ()
    (let ([i (/ (int-generator i1 i2) 4294967296)])
      (if (< i 0) (+ i 1) i))))

(define (random [i1 null] [i2 null])
  (cond
    [(null? i1) (current-random-generator)]
    [(null? i2) (* i1 (current-random-generator))]
    [else (+ (* (current-random-generator) (- i2 i1)) i1)]))

(define (randomSeed seed)
  (set! current-random-generator (double-generator seed)))
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
;;; Perlin Aux procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define octaves 4)
(define fallout 0.5)
(define seed (orig-rand 200000))

(define (generate-permutations)
  (define p (make-vector 512))
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


(define (fade t)
  (* t t t (+ (* t (- (* t 6.0) 15.0)) 10.0)))

(define (noise1D x)
  (define perm (generate-permutations))
  (define (grad1D i x)
    (if (bitwise-bit-set? (bitwise-and i #x01) 0) (- x) x))
  (let* ([X  (bitwise-and (exact-floor x) 255)]
         [x  (- x (floor x))])
    (lerp
      (grad1D (vector-ref perm X) x)
      (grad1D (vector-ref perm (+ X 1)) (- x 1))
      (fade x))))

(define (noise2D x y)
  (define perm (generate-permutations))
  (define (grad2D i x y)
    (let ([v (if (bitwise-bit-set? (bitwise-and i 1) 0) x y)])
      (if (bitwise-bit-set? (bitwise-and i 2) 0) (- v) v)))
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
        (fade x))
      (lerp
        (grad2D (vector-ref perm (+ p0 1)) x (- y 1))
        (grad2D (vector-ref perm (+ p1 1)) (- x 1) (- y 1))
        (fade x))
      (fade y))))

(define (noise3D x y z)
  (define perm (generate-permutations))
  (define (grad3D i x y z)
    (let* ([h (bitwise-and i 15)]
           [u (if (< h 8) x y)]
           [v (cond [(< h 4)y]
                    [(or (eq? h 12) (eq? h 14)) x]
                    [else z])])
      (+ (if (bitwise-bit-set? (bitwise-and h 1) 0) u (- u))
         (if (bitwise-bit-set? (bitwise-and h 2) 0) v (- v)))))
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
          (fade x))
        (lerp
          (grad3D (vector-ref perm p01) x (- y 1) z)
          (grad3D (vector-ref perm p11) (- x 1) (- y 1) z)
          (fade x))
        (fade y))
      (lerp
        (lerp
          (grad3D (vector-ref perm (+ 1 p00)) x y (- z 1))
          (grad3D (vector-ref perm (+ 1 p10)) (- x 1) y (- z 1))
          (fade x))
        (lerp
          (grad3D (vector-ref perm (+ 1 p01)) x (- y 1) (- z 1))
          (grad3D (vector-ref perm (+ 1 p11)) (- x 1) (- y 1)(- z 1))
          (fade x))
        (fade y))
      (fade z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Perlin Noise
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (noiseDetail oct [fall null])
  (set! octaves oct)
  (void (and (not (null? fall)) (set! fallout fall))))

(define (noiseSeed s)
  (set! seed s))

(define-syntax noise
  (syntax-rules ()
    [(_ x)
     (let ([k 0.5]
           [effect 1])
       (for/fold ([sum 0])
                 ([i octaves])
                 (set! effect (* effect fallout))
                 (set! k (* k 2))
                 (+ sum (* effect (/ (+ 1 (noise1D (* k x)) 2))))))]
    [(_ x y)
     (let ([k 0.5]
           [effect 1])
       (for/fold ([sum 0])
                 ([i octaves])
                 (set! effect (* effect fallout))
                 (set! k (* k 2))
                 (+ sum (/ (+ 1 (noise2D (* k x) (* k y)) 2)))))]
    [(_ x y z)
     (let ([k 0.5]
           [effect 1])
       (for/fold ([sum 0])
                 ([i octaves])
                 (set! effect (* effect fallout))
                 (set! k (* k 2))
                 (+ sum (/ (+ 1 (noise3D (* k x) (* k y) (* k z)))2))))]))


