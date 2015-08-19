#lang racket

(require (planet aml/rosetta))

(provide elliptic-torus-render)

(backend rhino5)

;(erase-2d-top)

(define (iterate-quads f ptss)
  (for/list ((pts0 ptss)
             (pts1 (cdr ptss)))
    (for/list ((p0 pts0)
               (p1 pts1)
               (p2 (cdr pts1))
               (p3 (cdr pts0)))
      (f p0 p1 p2 p3))))

(define (pts-average p0 p1)
  (/c (+c p0 p1) 2))

(define (quad-center p0 p1 p2 p3)
  (pts-average
   (pts-average p0 p2)
   (pts-average p1 p3)))

(define (quad-normal p0 p1 p2 p3)
  (poligon-normal (list p0 p1 p2 p3)))

(define (poligon-normal pts)
  (norm-c
   (crossed-products
    (append pts (list (car pts))))))

(define (crossed-products pts)
  (if (null? (cdr pts))
      (xyz 0 0 0)
      (+c (cross-c (car pts) (cadr pts))
          (crossed-products (cdr pts)))))

(define (transpose-matrix matrix)
  (if (null? (car matrix))
      (list)
      (cons (map car matrix)
            (transpose-matrix (map cdr matrix)))))


(define (elliptic-torus p c u0 u1 n v0 v1 m)
  (map-division
   (lambda (u v)
     (+xyz p
           (* (+ c (cos v)) (cos u))
           (* (+ c (cos v)) (sin u))
           (+ (sin v) (cos v))))
   u0 u1 n
   v0 v1 m))

;(surface-grid (elliptic-torus (xyz 0 0 0) 1.5 pi/4 (+ (/ 3pi 2) pi/4) 40 0 2pi 60))

(define (xy-plane p u0 u1 n v0 v1 m)
  (map-division
   (lambda (u v) (+xyz p u v 0))
   u0 u1 n v0 v1 m))


(define (positive-z? p)
  (if (>= (cz p) 0)
      #t
      #f))

(define (elliptic-torus-positive p c u0 u1 n v0 v1 m)
  (for/list ((x (elliptic-torus p c u0 u1 n v0 v1 m)))
    (filter positive-z? x)))

;(map spline (elliptic-torus-positive (xyz 0 0 -1) 0.5 pi/4 (+ (/ 3pi 2) pi/4) 40 0 2pi 60))
;(map spline (transpose-matrix (elliptic-torus-positive (xyz 0 0 -1) 0.5 pi/4 (+ (/ 3pi 2) pi/4) 40 0 2pi 60)))
;(surface-grid (elliptic-torus-positive (xyz 0 0 -1) 0.5 pi/4 (+ (/ 3pi 2) pi/4) 40 0 2pi 60))


(define (panel p0 p1 p2 p3)
  (let ((c (quad-center p0 p1 p2 p3))
        (n (quad-normal p0 p1 p2 p3))
        (h (random-range -0.01 0.03)))
    (if (<= h 0)
        #t
        (extrusion
         (surface-polygon p0 p1 p2 p3)
         (*c n (random-range 0 0.03))))))

(define (openings p0 p1 p2 p3 e h f)
  (let ((c (quad-center p0 p1 p2 p3))
        (r (random-range 0.0 (min (distance p0 p1) (distance p0 p3))))
        (n (quad-normal p0 p1 p2 p3)))
    (thicken
     (loft (list
           (polygon p0 p1 p2 p3)
           (move
            (scale (polygon p0 p1 p2 p3) f c)
            (*c n h)))) e)))

;(iterate-quads
; (lambda (p0 p1 p2 p3) (openings p0 p1 p2 p3 0.005))
; (elliptic-torus-positive (xyz 0 0 -1) 0.5 pi/4 (+ (/ 3pi 2) pi/4) 60 0 2pi 60))


(define (person p max-h)
  (let* ((h (random-range (* 0.9 max-h) max-h))
         (h1 (* 1/8 h))
         (h2 (* 5/8 h))
         (h3 (* 1/16 h))
         (r (random-range (* 1/8 h) (* 2/8 h)))
         (r2 (random-range (* 1/3 r) (* 2/3 r)))
         (r3 (* 2/3 r)))
    (cone-frustum p r (+z p h1) r2)
    (cone-frustum (+z p h1) r2 (+z p (+ h1 h2)) r)
    (cone-frustum (+z p (+ h1 h2)) r (+z p (+ h1 h2 h3)) r3)
    (sphere (+z p (+ h1 h2 h3 r3)) r3)))


(define (elliptic-torus-render p e h f u0 u1 v0 v1)
  (with-current-layer "White"
                      (iterate-quads (lambda (p0 p1 p2 p3) (openings p0 p1 p2 p3 e h f))
                                     (elliptic-torus-positive p 0.5 u0 u1 60 v0 v1 60))
                      (surface-rectangle (xy -1.7 -1.7) (xy 1.7 1.7))
                      #;(for ((i (in-range 16)))
                        (person (+xy (xyz 0 0 0) (random-range -1.6 1.6) (random-range -1.6 1.6)) 0.1))
                      (view (xyz 5.03354 1.90776 1.15746) (xyz -1.91274 -1.01359 -0.447255) 50)))


(elliptic-torus-render (xyz 0 0 -1) 0.005 0.03 0.5 pi/4 (+ (/ 3pi 2) pi/4) 0 2pi)


(define (color-renders)
  (with-current-layer "Black"
                      (iterate-quads (lambda (p0 p1 p2 p3) (panel p0 p1 p2 p3))
                                     (elliptic-torus-positive (xyz 0 0 -1) 0.5 pi/4 (+ (/ 3pi 2) pi/4) 60 0 2pi 60)))
  (with-current-layer "White"
                      (thicken (surface-grid (elliptic-torus-positive (xyz 0 0 -1) 0.5 pi/4 (+ (/ 3pi 2) pi/4) 60 0 2pi 60)) -0.02))
  (with-current-layer "Grey"
                      (surface-rectangle (xy -1.7 -1.7) (xy 1.7 1.7)))
  (with-current-layer "Red"
                      (person (+xy (u0) (random-range -1.6 1.6) (random-range -1.6 1.6)) 0.1)
                      (person (+xy (u0) (random-range -1.6 1.6) (random-range -1.6 1.6)) 0.1)
                      (person (+xy (u0) (random-range -1.6 1.6) (random-range -1.6 1.6)) 0.1)
                      (person (+xy (u0) (random-range -1.6 1.6) (random-range -1.6 1.6)) 0.1)
                      (person (+xy (u0) (random-range -1.6 1.6) (random-range -1.6 1.6)) 0.1)
                      (person (+xy (u0) (random-range -1.6 1.6) (random-range -1.6 1.6)) 0.1)
                      (person (+xy (u0) (random-range -1.6 1.6) (random-range -1.6 1.6)) 0.1)
                      (person (+xy (u0) (random-range -1.6 1.6) (random-range -1.6 1.6)) 0.1)
                      (person (+xy (u0) (random-range -1.6 1.6) (random-range -1.6 1.6)) 0.1)
                      (person (+xy (u0) (random-range -1.6 1.6) (random-range -1.6 1.6)) 0.1)
                      (person (+xy (u0) (random-range -1.6 1.6) (random-range -1.6 1.6)) 0.1)
                      (person (+xy (u0) (random-range -1.6 1.6) (random-range -1.6 1.6)) 0.1)
                      (person (+xy (u0) (random-range -1.6 1.6) (random-range -1.6 1.6)) 0.1)
                      (person (+xy (u0) (random-range -1.6 1.6) (random-range -1.6 1.6)) 0.1)
                      (person (+xy (u0) (random-range -1.6 1.6) (random-range -1.6 1.6)) 0.1)
                      (person (+xy (u0) (random-range -1.6 1.6) (random-range -1.6 1.6)) 0.1))
  (view (xyz 5.03354 1.90776 1.15746) (xyz -1.91274 -1.01359 -0.447255) 50))