#lang racket/base

(provide (all-defined-out))

(require (prefix-in ros- (planet aml/rosetta))
         "runtime-bindings.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2D Shapes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/types (arc [Object point] [float width] [float height] [float start] [float stop] -> void)
  (ros-arc point width start (- stop start)))

(define/types (ellipse [Object point] [float c] [float d] -> void)
  (if (= c d)
      (ros-circle point c)
      (ros-ellipse point c d)))

(define/types (line [Object p1] [Object p2] -> void)
  (ros-line p1 p2))

(define/types (point [Object p] -> void)
  (ros-point p))

(define/types (quad [Object p1] [Object p2] [Object p3] [Object p4] -> void)
  (ros-polygon p1 p2 p3 p4))

(define/types (triangle [Object p1] [Object p2] [Object p3] -> void)
  (ros-polygon p1 p2 p3))

(define/types (rect [Object p1] [float c] [float d] -> void)
  (ros-rectangle p1 c d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Curves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
 |(define-syntax bezier
 |  (syntax-rules ()
 |    [(_ x1 y1 x2 y2 x3 y3 x4 y4)
 |     (error "bezier: Not implemented yet!")]
 |    [(_ x1 y1 z1 x2 y2 z2 x3 y3 z3 x4 y4 z4)
 |     (error "bezier: Not implemented yet!")]))
 |
 |(define (bezierDetail detail)
 |  (error "bezierDetail: Not implemented yet!"))
 |
 |(define (bezierPoint a b c d t)
 |  (error "bezierPoint: Not implemented yet!"))
 |
 |(define (bezierTangent a b c d t)
 |  (error "bezierTangent: Not implemented yet!"))
 |
 |(define-syntax curve
 |  (syntax-rules ()
 |    [(_ x1 y1 x2 y2 x3 y3 x4 y4)
 |     (error "curve: Not implemented yet!")]
 |    [(_ x1 y1 z1 x2 y2 z2 x3 y3 z3 x4 y4 z4)
 |     (error "curve: Not implemented yet!")]))
 |
 |(define (curveDetail detail)
 |  (error "curveDetail: Not implemented yet!"))
 |
 |(define (curvePoint a b c d t)
 |  (error "curvePoint: Not implemented yet!"))
 |
 |(define (curveTangent a b c d t)
 |  (error "Not implemented yet!"))
 |
 |(define (curveTightness tightness)
 |  (error "curveTightness: Not implemented yet!"))
 |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3D Primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; xyz : float float float -> xyz
;;; coordinate abstraction
(define/types (xyz [float x] [float y] [float z] -> Object)
  (ros-xyz x y z))

;;; xyz : float float float -> xyz
;;; coordinate abstraction
(define/types (pol [float r] [float ang] -> Object)
  (ros-pol r ang))

;;; xyz : float float float -> xyz
;;; coordinate abstraction
(define/types (cyl [float r] [float th] [float fi] -> Object)
  (ros-cyl r th fi))

;;; box : -> void
;;; unary rectangular cuboid from the bottom-left corner
(define/types (box -> Object)
  (ros-box))

;;; box : xyz xyz -> void
;;; rectagular cuboid from the bottom-left corner to opposite top corner
(define/types (box [Object p1] [Object p2] -> Object)
  (ros-box p1 p2))

;;; box : xyz float float float -> void
;;; rectagular cuboid from the bottom-left with length, witdh and height
(define/types (box [Object p1] [float dx] [float dy] [float dz] -> Object)
  (ros-box p1 dx dy dz))

;;; right-cuboid : xyz float float float -> void
;;; cuboid base centered on a point
(define/types (right-cuboid [Object p1] [float dx] [float dy] [float dz] -> Object)
  (ros-right-cuboid p1 dx dy dz))

;;; cuboid : xyz ... xyz -> void
;;; cuboid based on 4 top vertices and 4 bot vertices
(define/types (cuboid [Object b1] [Object b2] [Object b3] [Object b4] [Object t1] [Object t2] [Object t3] [Object t4] -> Object)
  (ros-cuboid b1 b2 b3 b4 t1 t2 t3 t4))

;;; sphere : xyz float -> void
;;; sphere given the centroid point and the radius
(define/types (sphere [Object p] [float r] -> Object)
  (ros-sphere p r))

;;; cylinder : xyz float float -> void
;;; cylinder given base center, radius and height
(define/types (cylinder [Object p] [float r] [float h] -> Object)
  (ros-cylinder p r h))

;;; cylinder : xyz float float -> void
;;; cylinder given base center, radius and height
(define/types (cylinder [Object b] [float r] [Object t] -> Object)
  (ros-cylinder b r t))

;;; cone : xyz float float -> void
;;; cone given base center, radius and height
(define/types (cone [Object b] [float r] [float h] -> Object)
  (ros-cone b r h))

;;; cone : xyz float xyz -> void
;;; cone given the base center, radius and top center
(define/types (cone [Object b] [float r] [Object t] -> Object)
  (ros-cone b r t))

;;; cone-frustum : xyz float float float -> void
;;; cone-frustum given the base center, bottom radius , height and top radius
(define/types (cone-frustum [Object b] [float r1] [float h] [float r2] -> Object)
  (ros-cone-frustum b r1 h r2))

;;; cone-frustum : xyz float xyz float -> void
;;; given the base center, bottom radius , top center and top radius
(define/types (cone-frustum [Object b] [float r1] [Object t] [float r2] -> Object)
  (ros-cone-frustum b r1 t r2))

;;; pyramid : int xyz float float float -> void
;;; takes the number of sides, base point, base radius, initial angle and height
(define/types (pyramid [float n] [Object p] [float r] [float ang] [float h] -> Object)
  (ros-regular-pyramid n p r ang h))

;;; pyramid : int xyz float xyz float -> void
;;; takes the number of sides, base point, base radius, initial angle and top point
(define/types (pyramid [float n] [Object p] [float r] [float ang] [Object t] -> Object)
  (ros-regular-pyramid n p r ang t))

;;; pyramid-frustum : int xyz float float float float -> void
;;; takes the number of sides, base point, base radius, initial angle, height, and top radius
(define/types (pyramid-frustum [float n] [Object p] [float r] [float ang] [float h] [float r2] -> Object)
  (ros-regular-pyramid-frustum n p r ang h r2))

;;; pyramid-frustum : int xyz float xyz float float -> void
;;; takes the number of sides, base point, base radius, initial angle, top point, and top radius
(define/types (pyramid-frustum [float n] [Object p] [float r] [float ang] [Object t] [float r2] -> Object)
  (ros-regular-pyramid-frustum n p r ang t r2))

;;; irregular-pyramid : (vector/of xyz) xyz -> void
;;; takes a list of points that define the base and the location of the apex
(define/types (irregular-pyramid [Object lst] [Object apex] -> Object)
  (ros-irregular-pyramid  (vector->list lst) apex #t))

;;; prism : int xyz float float float -> void
;;; takes the number of sides, base point, base radius, initial angle, height.
(define/types (prism [float n] [Object p] [float r] [float ang] [float h] -> Object)
  (ros-regular-prism n p r ang h))

;;; prism : int xyz float float xyz -> void
;;; takes the number of sides, base point, base radius, initial angle, top point.
(define/types (prism [float n] [Object p] [float r] [float ang] [Object t] -> Object)
  (ros-regular-prism n p r ang t))

;;; irregular-prism : (vector/of xyz) xyz -> void
;;; takes a list of points that define the base and the location of the apex
(define/types (irregular-prism [Object lst] [Object apex] -> Object)
  (ros-irregular-prism (vector->list lst) apex))

;;; irregular-prism : (vector/of xyz) xyz -> void
;;; takes the center point of the torus, the torus radius and the section radius
(define/types (torus  [Object p] [float r1] [float r2] -> Object)
  (ros-torus p r1 r2))

