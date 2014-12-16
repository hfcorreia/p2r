(module processingAPI/objects racket

  (provide (all-defined-out))

  (define PVector
    (class object%
           (field [x 0] 
                  [y 0]
                  [z 0])

           (define/public (fromAngle ang [v null])
                          (if (null? v)
                            (make-object PVector)
                            (begin 
                              (set-field! x v (cos ang))
                              (set-field! y v (sin ang))
                              v)))




           (super-instantiate ())))

  )
