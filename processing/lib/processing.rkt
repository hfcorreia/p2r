(module runtime/processing racket
  (provide (all-defined-out))

  (require (prefix-in ros- (planet aml/rosetta:1:=50))
           racket/system)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Rosetta Configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define tikz ros-tikz)
  (define (backend backend-id)
    (ros-backend backend-id))

  (define (generateTikz file-name [scale 1] [pdf-viewer "evince"])
    (define (tikz->tex str out)
      (let ((scale (* scale 0.028)))
        (fprintf out
               "\\documentclass[]{article}\n\\usepackage{tikz}\n\\begin{document}\n\\begin{tikzpicture}[yscale=-~a,xscale=~a]\n~a\n\\end{tikzpicture}\n\\end{document}"
               scale scale str)))
    (define (display-tikz-to-string)
      (let ([output-port (open-output-string)])
        (parameterize ([current-output-port output-port])
          (ros-display-tikz))
        (get-output-string output-port)))

    (define out 
      (open-output-file (string-append file-name ".tex") #:exists 'replace))

    (tikz->tex (display-tikz-to-string) out)
    (close-output-port out)
    (system (string-append "pdflatex " file-name ".tex"))
    (system (string-append pdf-viewer " " file-name ".pdf")))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; 2D Shapes
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; arc() - Draws an arc to the screen. Arcs are drawn along the outer edge of an
  ;;; ellipse defined by the a, b, c, and d parameters. The origin of the arc's
  ;;; ellipse may be changed with the ellipseMode() function. Use the start and stop
  ;;; parameters to specify the angles (in radians) at which to draw the arc.

  ;;; There are three ways to draw an arc; the rendering technique used is defined
  ;;; by the optional seventh paramter. The three examples, are PIE, OPEN, and CHORD. 
  ;;; The default mode is the OPEN stroke with a PIE fill.

  ;;; Parameters:
  ;;;   a       float: x-coordinate of the arc's ellipse
  ;;;   b       float: y-coordinate of the arc's ellipse
  ;;;   c       float: width of the arc's ellipse by default
  ;;;   d       float: height of the arc's ellipse by default
  ;;;   start   float: angle to start the arc, specified in radians
  ;;;   stop    float: angle to stop the arc, specified in radians
  (define (arc a b c d start stop [mode 'OPEN])
    (case mode
      ['OPEN  (ros-arc (ros-xy a b) c start stop mode)]
      ['PIE   (error "Not implemented yet!")]
      ['CHORD (error "Not implemented yet!")]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; ellipse() - Draws an ellipse (oval) to the screen. An ellipse with equal
  ;;; width and height is a circle. By default, the first two parameters set the
  ;;; location, and the third and fourth parameters set the shape's width and
  ;;; height. The origin may be changed with the ellipseMode() function. 

  ;;; Parameters:
  ;;;   a       float: x-coordinate of the ellipse
  ;;;   b       float: y-coordinate of the ellipse
  ;;;   c       float: width of the ellipse by default
  ;;;   d       float: height of the ellipse by default
  (define (ellipse a b c d)
    (ros-circle (ros-xy a b) c))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  line() - Draws a line (a direct path between two points) to the screen. 
  ;;;  The version of line() with four parameters draws the line in 2D. To color a 
  ;;;  line, use the stroke() function. A line cannot be filled, therefore the fill()
  ;;;  function will not affect the color of a line. 2D lines are drawn with a
  ;;;  width of one pixel by default, but this can be changed with the
  ;;;  strokeWeight() function. The version with six parameters allows the line to
  ;;;  be placed anywhere within XYZ space. Drawing this shape in 3D with the z
  ;;;  parameter requires the P3D parameter in combination with size() as shown in
  ;;;  the above example. 

  ;;; Parameters: 
  ;;;   x1     float: x-coordinate of the first point
  ;;;   y1     float: y-coordinate of the first point
  ;;;   x2     float: x-coordinate of the second point
  ;;;   y2     float: y-coordinate of the second point
  ;;;   z1     float: z-coordinate of the first point
  ;;;   z2     float: z-coordinate of the second point
  (define-syntax line 
    (syntax-rules ()
      [(_ x1 y1 z1 x2 y2 z2)
       (error "No support for 3D")]
      [(_ x1 y1 x2 y2)
         (ros-line (ros-xy x1 y1) (ros-xy x2 y2))]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  point() - Draws a point, a coordinate in space at the dimension of one pixel. The
  ;;;  first parameter is the horizontal value for the point, the second value is
  ;;;  the vertical value for the point, and the optional third value is the depth
  ;;;  value. Drawing this shape in 3D with the z parameter requires the P3D
  ;;;  parameter in combination with size() as shown in the above example. 

  ;;; Parameters: 
  ;;;   x       float: x-coordinate of the point
  ;;;   y       float: y-coordinate of the point
  ;;;   z       float: z-coordinate of the point
  (define (point x y [z #f])
    (ros-xy x y))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; quad() - A quad is a quadrilateral, a four sided polygon. It is similar to a
  ;;; rectangle, but the angles between its edges are not constrained to
  ;;; ninety degrees. The first pair of parameters (x1,y1) sets the first
  ;;; vertex and the subsequent pairs should proceed clockwise or
  ;;; counter-clockwise around the defined shape.  

  ;;; Parameters: 
  ;;;   x1      float: x-coordinate of the first corner
  ;;;   y1      float: y-coordinate of the first corner
  ;;;   x2      float: x-coordinate of the second corner
  ;;;   y2      float: y-coordinate of the second corner
  ;;;   x3      float: x-coordinate of the third corner
  ;;;   y3      float: y-coordinate of the third corner
  ;;;   x4      float: x-coordinate of the fourth corner
  ;;;   y4      float: y-coordinate of the fourth corner
  (define (quad x1 y1 x2 y2 x3 y3 x4 y4)
    (ros-polygon 
      (ros-xy x1 y1)
      (ros-xy x2 y2)
      (ros-xy x3 y3)
      (ros-xy x4 y4)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; rect() - Draws a rectangle to the screen. A rectangle is a four-sided shape with every
  ;;; angle at ninety degrees. By default, the first two parameters set the location
  ;;; of the upper-left corner, the third sets the width, and the fourth sets the
  ;;; height. The way these parameters are interpreted, however, may be changed with
  ;;; the rectMode() function.

  ;;; To draw a rounded rectangle, add a fifth parameter, which is used as the
  ;;; radius value for all four corners.

  ;;; To use a different radius value for each corner, include eight parameters.
  ;;; When using eight parameters, the latter four set the radius of the arc at each
  ;;; corner separately, starting with the toleft corner and moving clockwise
  ;;; around the rectangle. 

  ;;; Parameters:
  ;;;   a     float: x-coordinate of the rectangle by default
  ;;;   b     float: y-coordinate of the rectangle by default
  ;;;   c     float: width of the rectangle by default
  ;;;   d     float: height of the rectangle by default
  ;;;   r     float: radii for all four corners
  ;;;   tl    float: radius for toleft corner
  ;;;   tr    float: radius for toright corner
  ;;;   br    float: radius for bottom-right corner
  ;;;   bl    float: radius for bottom-left corner
  (define (rect a b c d)
    (ros-rectangle (ros-xy a b) c d))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  trigangle() - A triangle is a plane created by connecting three points. 
  ;;;  The first two arguments specify the first point, the middle two arguments 
  ;;;  specify the second point, and the last two arguments specify the third point. 

  ;;; Parameters 
  ;;;   x1     float: x-coordinate of the first point
  ;;;   y1     float: y-coordinate of the first point
  ;;;   x2     float: x-coordinate of the second point
  ;;;   y2     float: y-coordinate of the second point
  ;;;   x3     float: x-coordinate of the third point
  ;;;   y3     float: y-coordinate of the third point
  (define (triangle x1 y1 x2 y2 x3 y3)
    (ros-polygon 
      (ros-xy x1 y2)
      (ros-xy x2 y2)
      (ros-xy x3 y3)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Curves
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; bezier() - Draws a Bezier curve on the screen. These curves are defined by
  ;;; a series of anchor and control points. The first two parameters specify the
  ;;; first anchor point and the last two parameters specify the other anchor point.
  ;;; The middle parameters specify the control points which define the shape of the
  ;;; curve. 

  ;;;  Parameters    
  ;;;   x1    float: coordinates for the first anchor point
  ;;;   y1    float: coordinates for the first anchor point
  ;;;   z1    float: coordinates for the first anchor point
  ;;;   x2    float: coordinates for the first control point
  ;;;   y2    float: coordinates for the first control point
  ;;;   z2    float: coordinates for the first control point
  ;;;   x3    float: coordinates for the second control point
  ;;;   y3    float: coordinates for the second control point
  ;;;   z3    float: coordinates for the second control point
  ;;;   x4    float: coordinates for the second anchor point
  ;;;   y4    float: coordinates for the second anchor point
  ;;;   z4    float: coordinates for the second anchor point
  (define-syntax bezier
    (syntax-rules ()
      [(_ x1 y1 x2 y2 x3 y3 x4 y4)
       (error "Not implemented yet!")]
      [(_ x1 y1 z1 x2 y2 z2 x3 y3 z3 x4 y4 z4)
       (error "Not implemented yet!")]))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; bezierDetail() - Sets the resolution at which Beziers display. 
  ;;; The default value is 20. This function is only useful when using the 
  ;;; P3D renderer; the default P2D renderer does not use this information.

  ;;; Parameters  
  ;;;   detail  int: resolution of the curves
  (define (bezierDetail detail)
    (error "Not implemented yet!"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; bezierPoint() - Evaluates the Bezier at point t for points a, b, c, d. The
  ;;; parameter t varies between 0 and 1, a and d are points on the curve, and b and c
  ;;; are the control points. This can be done once with the x coordinates and a
  ;;; second time with the y coordinates to get the location of a bezier curve at t.

  ;;; Parameters  
  ;;;   a   float: coordinate of first point on the curve
  ;;;   b   float: coordinate of first control point
  ;;;   c   float: coordinate of second control point
  ;;;   d   float: coordinate of second point on the curve
  ;;;   t   float: value between 0 and 1
  (define (bezierPoint a b c d t)
    (error "Not implemented yet!"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; bezierTangent() - Calculates the tangent of a point on a Bezier curve. 

  ;;; Parameters  
  ;;;   a   float: coordinate of first point on the curve
  ;;;   b   float: coordinate of first control point
  ;;;   c   float: coordinate of second control point
  ;;;   d   float: coordinate of second point on the curve
  ;;;   t   float: value between 0 and 1
  (define (bezierTangent a b c d t)
    (error "Not implemented yet!"))

  ;;; curve() - Draws a curved line on the screen. The first and second parameters specify
  ;;; the beginning control point and the last two parameters specify the ending
  ;;; control point. The middle parameters specify the start and stop of the
  ;;; curve. Longer curves can be created by putting a series of curve() functions
  ;;; together or using curveVertex(). An additional function called
  ;;; curveTightness() provides control for the visual quality of the curve. The
  ;;; curve() function is an implementation of Catmull-Rom splines.

  ;;; Parameters  
  ;;;   x1  float: coordinates for the beginning control point
  ;;;   y1  float: coordinates for the beginning control point
  ;;;   x2  float: coordinates for the first point
  ;;;   y2  float: coordinates for the first point
  ;;;   x3  float: coordinates for the second point
  ;;;   y3  float: coordinates for the second point
  ;;;   x4  float: coordinates for the ending control point
  ;;;   y4  float: coordinates for the ending control point
  ;;;   z1  float: coordinates for the beginning control point
  ;;;   z2  float: coordinates for the first point
  ;;;   z3  float: coordinates for the second point
  ;;;   z4  float: coordinates for the ending control point
  (define-syntax curve
    (syntax-rules ()
      [(_ x1 y1 x2 y2 x3 y3 x4 y4)
       (error "Not implemented yet!")]
      [(_ x1 y1 z1 x2 y2 z2 x3 y3 z3 x4 y4 z4)
       (error "Not implemented yet!")]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; curveDetail() - Sets the resolution at which curves display. 
  ;;; The default value is 20. This function is only useful when using the 
  ;;; P3D renderer; the default P2D renderer does not use this information.

  ;;; Parameters  
  ;;;   detail  int: resolution of the curves
  (define (curveDetail detail)
    (error "Not implemented yet!"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; curvePoint() - Evaluates the curve at point t for points a, b, c, d. The parameter t may
  ;;; range from 0 (the start of the curve) and 1 (the end of the curve). a and d
  ;;; are points on the curve, and b and c are the control points. This can be
  ;;; used once with the x coordinates and a second time with the y coordinates to
  ;;; get the location of a curve at t. 

  ;;; Parameters  
  ;;;   a   float: coordinate of first point on the curve
  ;;;   b   float: coordinate of first control point
  ;;;   c   float: coordinate of second control point
  ;;;   d   float: coordinate of second point on the curve
  ;;;   t   float: value between 0 and 1
  (define (curvePoint a b c d t)
    (error "Not implemented yet!"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; curveTangent() - Calculates the tangent of a point on a curve. 

  ;;; Parameters  
  ;;;   a   float: coordinate of first point on the curve
  ;;;   b   float: coordinate of first control point
  ;;;   c   float: coordinate of second control point
  ;;;   d   float: coordinate of second point on the curve
  ;;;   t   float: value between 0 and 1
  (define (curveTangent a b c d t)
    (error "Not implemented yet!"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; curveTightness() - Modifies the quality of forms created with curve() and curveVertex(). The
  ;;; parameter tightness determines how the curve fits to the vertex points. The
  ;;; value 0.0 is the default value for tightness (this value defines the curves to
  ;;; be Catmull-Rom splines) and the value 1.0 connects all the points with straight 
  ;;; lines. Values within the range -5.0 and 5.0 will deform the curves but will leave 
  ;;; them recognizable and as values increase in magnitude, they will continue to deform. 

  ;;;  Parameters    
  ;;;   tightness     float: amount of deformation from the original vertices
  (define (curveTightness tightness)
    (error "Not implemented yet!"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; 3D Primitives
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; box() - A box is an extruded rectangle. A box with equal dimensions on
  ;;; all sides is a cube.

  ;;; box(size)
  ;;; box(w, h, d)

  ;;; Parameters    
  ;;; size  float: dimension of the box in all dimensions (creates a cube)
  ;;; w     float: dimension of the box in the x-dimension
  ;;; h     float: dimension of the box in the y-dimension
  ;;; d     float: dimension of the box in the z-dimension
  (define-syntax box
    (syntax-rules ()
      [(_ size)
       (error "Not implemented yet!")]
      [(_ w d h)
       (error "Not implemented yet!")]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; sphere() - A sphere is a hollow ball made from tessellated triangles.

  ;;; Parameters  
  ;;;     float: the radius of the sphere
  (define (sphere r)
    (error "Not implemented yet!"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; sphereDetail() - Controls the detail used to render a sphere by adjusting the
  ;;; number of vertices of the sphere mesh. The default resolution is 30, which
  ;;; creates a fairly detailed sphere definition with vertices every 360/30 = 12
  ;;; degrees. If you're going to render a great number of spheres per frame, it is
  ;;; advised to reduce the level of detail using this function. The setting stays
  ;;; active until sphereDetail() is called again with a new parameter and so should
  ;;; not be called prior to every sphere() statement, unless you wish to render
  ;;; spheres with different settings, e.g. using less detail for smaller spheres or
  ;;; ones further away from the camera. To control the detail of the horizontal and
  ;;; vertical resolution independently, use the version of the functions with two
  ;;; parameters.

  ;;; Parameters 
  ;;; res    int: number of segments (minimum 3) used per full circle revolution
  ;;; ures   int: number of segments used longitudinally per full circle revolutoin
  ;;; vres   int: number of segments used latitudinally from top to bottom
  (define-syntax sphereDetail
    (syntax-rules ()
      [(_ res)
       (error "Not implemented yet!")]
      [(_ ures vres)
       (error "Not implemented yet!")]))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Attributes
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; ellipseMode() - Modifies the location from which ellipses are drawn by 
  ;;; changing the way in which parameters given to ellipse() are intepreted.

  ;;; The default mode is ellipseMode(CENTER), which interprets the first two
  ;;; parameters of ellipse() as the shape's center point, while the third and
  ;;; fourth parameters are its width and height.

  ;;; ellipseMode(RADIUS) also uses the first two parameters of ellipse() as the
  ;;; shape's center point, but uses the third and fourth parameters to specify half
  ;;; of the shapes's width and height.

  ;;; ellipseMode(CORNER) interprets the first two parameters of ellipse() as the
  ;;; upper-left corner of the shape, while the third and fourth parameters are its
  ;;; width and height.

  ;;; ellipseMode(CORNERS) interprets the first two parameters of ellipse() as the
  ;;; location of one corner of the ellipse's bounding box, and the third and fourth
  ;;; parameters as the location of the opposite corner.

  ;;; Parameters:
  ;;;   mode    int: either CENTER, RADIUS, CORNER, or CORNERS
  (define (ellipseMode mode)
    (error "Not implemented yet!"))
  )
