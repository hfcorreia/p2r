#lang racket/base

(provide (all-defined-out))

(require "runtime-bindings.rkt")


(require (prefix-in ros- (planet aml/rosetta)))
 #|
 |;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 |;;; Shapes
 |;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 |(define-syntax  createShape
 |  (syntax-rules ()
 |    [(_)
 |     (error "createShape: Not implemented!")]
 |    [(_ src)
 |     (error "createShape: Not implemented!")]
 |    [(_ kind p)
 |     (error "createShape: Not implemented!")]))
 |
 |(define (loadShape filename)
 |  (error "loadShape: Not implemented!"))
 |
 |;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 |;;; 2D Shapes
 |;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 |
 |(define (arc a b c d start stop [mode 'OPEN])
 |  (case mode
 |    ['OPEN  (ros-arc (ros-xy a b) c start (- stop start))]
 |    ['PIE   (error "Not implemented yet!")]
 |    ['CHORD (error "Not implemented yet!")]))
 |
 |(define (ellipse a b c d)
 |  (if (= c d)
 |    (ros-circle (ros-xy a b) c)
 |    (ros-ellipse (ros-xy a b) c d)))
 |
 |(define-syntax line
 |  (syntax-rules ()
 |    [(_ x1 y1 z1 x2 y2 z2)
 |     (error "No support for 3D")]
 |    [(_ x1 y1 x2 y2)
 |     (ros-line (ros-xy x1 y1) (ros-xy x2 y2))]))
 |
 |(define-syntax point
 |  (syntax-rules ()
 |    [(_ x y)
 |     (ros-point (ros-xy x y))]
 |    [(_ x y z)
 |     (ros-point (ros-xy x y z))]))
 |
 |(define (quad x1 y1 x2 y2 x3 y3 x4 y4)
 |  (ros-polygon
 |    (ros-xy x1 y1)
 |    (ros-xy x2 y2)
 |    (ros-xy x3 y3)
 |    (ros-xy x4 y4)))
 |
 |(define (rect a b c d)
 |  (ros-rectangle (ros-xy a b) c d))
 |
 |(define (triangle x1 y1 x2 y2 x3 y3)
 |  (ros-polygon
 |    (ros-xy x1 y1)
 |    (ros-xy x2 y2)
 |    (ros-xy x3 y3)))
 |
 |;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 |;;; Curves
 |;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 |
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

#|(define-syntax sphereDetail
 |  (syntax-rules ()
 |    [(_ res)
 |     (error "sphereDetail: Not implemented yet!")]
 |    [(_ ures vres)
 |     (error "sphereDetail: Not implemented yet!")]))
 |
 |;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 |;;; Attributes
 |;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 |
 |(define (ellipseMode mode)
 |  (error "ellipseMode: Not implemented yet!"))
 |
 |(define (noSmooth)
 |  (error "noSmooth: Not implemented yet!"))
 |
 |(define (rectMode mode)
 |  (case mode
 |    ['CORNER  (error "rectMode: Not implemented yet!")]
 |    ['CENTER  (error "rectMode: Not implemented yet!")]
 |    ['RADIUS  (error "rectMode: Not implemented yet!")]
 |    ['CORNERS  (error "rectMode: Not implemented yet!")]))
 |
 |(define-syntax smooth
 |  (syntax-rules ()
 |    [(_) (error "smooth: Not implemented yet!")]
 |    [(_ level) (error "smooth: Not implemented yet!")]))
 |
 |(define (strokeCap cap)
 |  (case cap
 |    ['SQUARE  (error "strokeCap: Not implemented yet!")]
 |    ['PROJECT (error "strokeCap: Not implemented yet!")]
 |    ['ROUND   (error "strokeCap: Not implemented yet!")]))
 |
 |(define (strokeJoin join)
 |  (case join
 |    ['MITER (error "strokeJoin: Not implemented yet!")]
 |    ['BEVEL (error "strokeJoin: Not implemented yet!")]
 |    ['ROUND (error "strokeJoin: Not implemented yet!")]))
 |
 |(define (strokeWeight weight)
 |  (error "strokeWeight: Not implemented yet!"))
 |
 |
 |;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 |;;; Vertex
 |;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 |
 |(define (beginContour)
 |  (error "beginContour: Not implemented yet!"))
 |
 |(define (endContour)
 |  (error "endContour: Not implemented yet!"))
 |
 |(define-syntax beginShape
 |  (syntax-rules ()
 |    [(_)
 |     (error "beginShape: Not implemented yet!")]
 |    [(_ kind)
 |     (error "beginShape: Not implemented yet!")]))
 |
 |(define (endShape)
 |  (syntax-rules ()
 |    [(_ )
 |     (error "endShape: Not implemented yet!")]
 |    [(_ mode)
 |     (error "endShape: Not implemented yet!")]))
 |
 |(define-syntax bezierVertex
 |  (syntax-rules ()
 |    [(_ x2 y2 x3 y3 x4 y4)
 |     (error "bezierVertex: Not implemented yet!")]
 |    [(_ x2 y2 z2 x3 y3 z3 x4 y4 z4)
 |     (error "bezierVertex: Not implemented yet!")]))
 |
 |(define-syntax curveVertex
 |  (syntax-rules ()
 |    [(_ x y)
 |     (error "curveVertex: Not implemented yet!")]
 |    [(_ x y z)
 |     (error "curveVertex: Not implemented yet!")]))
 |
 |(define-syntax quadraticVertex
 |  (syntax-rules ()
 |    [(_ cx cy x y)
 |     (error "quadraticVertex: Not implemented yet!")]
 |    [(_ cx cy cz x y z)
 |     (error "quadraticVertex: Not implemented yet!")]))
 |
 |(define-syntax vertex
 |  (syntax-rules ()
 |    [(_ v)
 |     (error "vertex: Not implemented yet!")]
 |    [(_ x y)
 |     (error "vertex: Not implemented yet!")]
 |    [(_ x y z)
 |     (error "vertex: Not implemented yet!")]
 |    [(_ x y u v)
 |     (error "vertex: Not implemented yet!")]
 |    [(_ x y z u v)
 |     (error "vertex: Not implemented yet!")]))
 |
 |;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 |;;; Load and display
 |;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 |(define-syntax shape
 |  (syntax-rules ()
 |    [(_ shape)
 |     (error "shape: Not implemented yet!")]
 |    [(_ shape x y)
 |     (error "shape: Not implemented yet!")]
 |    [(_ shape a b c d)
 |     (error "shape: Not implemented yet!")]))
 |
 |(define (shapeMode mode)
 |  (case mode
 |    ['CORNER  (error "shapeMode: Not implemented yet!")]
 |    ['CORNERS (error "shapeMode: Not implemented yet!")]
 |    ['CENTER  (error "shapeMode: Not implemented yet!")]))
|#
