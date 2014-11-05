(module name-mangling racket
  
  (provide racket->java)
  
  ;;; Converts a racket name to a java name
  (define (racket->java name)
    (let ((name (regexp-replace #rx"^([0-9]+)" name "_\\1"))) 
      (list->string
       (let iter ((i 0))
         (if (= i (string-length name))
             (list)
             (let ((c (string-ref name i)))
               (let ((special
                      (case c
                        [(#\-) ""]
                        [(#\!) "bang"]
                        [(#\#) "sharp"]
                        [(#\%) "percent"]
                        [(#\&) "and"]
                        [(#\*) "star"]
                        [(#\+) "plus"]
                        [(#\.) "dot"]
                        [(#\/) "slash"]
                        [(#\{) "lbracket"]
                        [(#\}) "rbracket"]
                        [(#\<) "less"]
                        [(#\=) "equal"]
                        [(#\>) "greater"]
                        [(#\^) "up"]
                        [(#\~) "tilde"]
                        [(#\@) "at"]
                        [(#\?) "p"]
                        [(#\:) "colon"]
                        [else #f])))
                 (if special
                     (append
                      (string->list
                       (if (= i 0)
                           special
                           (string-titlecase special)))
                      (let ((others (iter  (+ 1 i))))
                        (if (null? others)
                            (list)
                            (cons (char-upcase (first others))
                                  (rest others)))))
                     (cons c (iter (+ 1 i)))))))))))
  
  
  )
