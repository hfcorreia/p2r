(module processing/shapes racket

  (provide (all-defined-out))
  
  (require  (prefix-in ros- (planet aml/rosetta:1:=50)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Shapes
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-syntax  createShape
    (syntax-rules ()
      [(_)
       (error "createShape: Not implemented!")]
      [(_ src)
       (error "createShape: Not implemented!")]
      [(_ kind p)
       (error "createShape: Not implemented!")]))

  (define (loadShape filename)
       (error "loadShape: Not implemented!"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; 2D Shapes
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (arc a b c d start stop [mode 'OPEN])
    (case mode
      ['OPEN  (ros-arc (ros-xy a b) c start (- stop start))]
      ['PIE   (error "Not implemented yet!")]
      ['CHORD (error "Not implemented yet!")]))

  (define (ellipse a b c d)
    (if (= c d)
      (ros-circle (ros-xy a b) c)
      (ros-ellipse (ros-xy a b) c d)))

  (define-syntax line 
    (syntax-rules ()
      [(_ x1 y1 z1 x2 y2 z2)
       (error "No support for 3D")]
      [(_ x1 y1 x2 y2)
       (ros-line (ros-xy x1 y1) (ros-xy x2 y2))]))

  (define-syntax point
    (syntax-rules ()
      [(_ x y)
       (ros-point (ros-xy x y))]
      [(_ x y z)
       (ros-point (ros-xy x y z))]))

  (define (quad x1 y1 x2 y2 x3 y3 x4 y4)
    (ros-polygon 
      (ros-xy x1 y1)
      (ros-xy x2 y2)
      (ros-xy x3 y3)
      (ros-xy x4 y4)))

  (define (rect a b c d)
    (ros-rectangle (ros-xy a b) c d))

  (define (triangle x1 y1 x2 y2 x3 y3)
    (ros-polygon 
      (ros-xy x1 y1)
      (ros-xy x2 y2)
      (ros-xy x3 y3)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Curves
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax bezier
    (syntax-rules ()
      [(_ x1 y1 x2 y2 x3 y3 x4 y4)
       (error "bezier: Not implemented yet!")]
      [(_ x1 y1 z1 x2 y2 z2 x3 y3 z3 x4 y4 z4)
       (error "bezier: Not implemented yet!")]))

  (define (bezierDetail detail)
    (error "bezierDetail: Not implemented yet!"))

  (define (bezierPoint a b c d t)
    (error "bezierPoint: Not implemented yet!"))

  (define (bezierTangent a b c d t)
    (error "bezierTangent: Not implemented yet!"))

  (define-syntax curve
    (syntax-rules ()
      [(_ x1 y1 x2 y2 x3 y3 x4 y4)
       (error "curve: Not implemented yet!")]
      [(_ x1 y1 z1 x2 y2 z2 x3 y3 z3 x4 y4 z4)
       (error "curve: Not implemented yet!")]))

  (define (curveDetail detail)
    (error "curveDetail: Not implemented yet!"))

  (define (curvePoint a b c d t)
    (error "curvePoint: Not implemented yet!"))

  (define (curveTangent a b c d t)
    (error "Not implemented yet!"))

  (define (curveTightness tightness)
    (error "curveTightness: Not implemented yet!"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; 3D Primitives
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax box
    (syntax-rules ()
      [(_ size)
       (error "box: Not implemented yet!")]
      [(_ w d h)
       (error "box: Not implemented yet!")]))

  (define (sphere r)
    (error "sphere: Not implemented yet!"))

  (define-syntax sphereDetail
    (syntax-rules ()
      [(_ res)
       (error "sphereDetail: Not implemented yet!")]
      [(_ ures vres)
       (error "sphereDetail: Not implemented yet!")]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Attributes
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (ellipseMode mode)
    (error "ellipseMode: Not implemented yet!"))

  (define (noSmooth)
    (error "noSmooth: Not implemented yet!"))

  (define (rectMode mode)
    (case mode
      ['CORNER  (error "rectMode: Not implemented yet!")]
      ['CENTER  (error "rectMode: Not implemented yet!")]
      ['RADIUS  (error "rectMode: Not implemented yet!")]
      ['CORNERS  (error "rectMode: Not implemented yet!")]))

  (define-syntax smooth
    (syntax-rules ()
    [(_) (error "smooth: Not implemented yet!")]
    [(_ level) (error "smooth: Not implemented yet!")]))

  (define (strokeCap cap)
    (case cap
      ['SQUARE  (error "strokeCap: Not implemented yet!")]
      ['PROJECT (error "strokeCap: Not implemented yet!")]
      ['ROUND   (error "strokeCap: Not implemented yet!")]))

  (define (strokeJoin join)
    (case join
      ['MITER (error "strokeJoin: Not implemented yet!")]
      ['BEVEL (error "strokeJoin: Not implemented yet!")]
      ['ROUND (error "strokeJoin: Not implemented yet!")]))

  (define (strokeWeight weight)
    (error "strokeWeight: Not implemented yet!"))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Vertex
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (beginContour) 
    (error "beginContour: Not implemented yet!"))

  (define (endContour)
    (error "endContour: Not implemented yet!"))

  (define-syntax beginShape
    (syntax-rules ()
      [(_)
       (error "beginShape: Not implemented yet!")]
      [(_ kind)
       (error "beginShape: Not implemented yet!")]))

  (define (endShape)
    (syntax-rules ()
      [(_ )
       (error "endShape: Not implemented yet!")]
      [(_ mode)
       (error "endShape: Not implemented yet!")]))

  (define-syntax bezierVertex
    (syntax-rules ()
      [(_ x2 y2 x3 y3 x4 y4)
       (error "bezierVertex: Not implemented yet!")]
      [(_ x2 y2 z2 x3 y3 z3 x4 y4 z4)
       (error "bezierVertex: Not implemented yet!")]))

  (define-syntax curveVertex
    (syntax-rules ()
      [(_ x y)
       (error "curveVertex: Not implemented yet!")]
      [(_ x y z)
       (error "curveVertex: Not implemented yet!")]))

  (define-syntax quadraticVertex
    (syntax-rules ()
      [(_ cx cy x y)
       (error "quadraticVertex: Not implemented yet!")]
      [(_ cx cy cz x y z)
       (error "quadraticVertex: Not implemented yet!")]))

  (define-syntax vertex
    (syntax-rules ()
      [(_ v)
       (error "vertex: Not implemented yet!")]
      [(_ x y)
       (error "vertex: Not implemented yet!")]
      [(_ x y z)
       (error "vertex: Not implemented yet!")]
      [(_ x y u v)
       (error "vertex: Not implemented yet!")]
      [(_ x y z u v)
       (error "vertex: Not implemented yet!")]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Load and display
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-syntax shape
    (syntax-rules ()
      [(_ shape)
       (error "shape: Not implemented yet!")]
      [(_ shape x y)
       (error "shape: Not implemented yet!")]
      [(_ shape a b c d)
       (error "shape: Not implemented yet!")]))

  (define (shapeMode mode)
    (case mode
      ['CORNER  (error "shapeMode: Not implemented yet!")]
      ['CORNERS (error "shapeMode: Not implemented yet!")]
      ['CENTER  (error "shapeMode: Not implemented yet!")]))
  )
