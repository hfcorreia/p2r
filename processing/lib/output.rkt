(module processing-api/output racket

  (provide print
           println)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Print procedures
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (print . args)
    (display (build-seperated-string (reverse (map arg->string args))
                                     #\space)))

  (define (println . args)
    (displayln (build-seperated-string (reverse (map arg->string args))
                                       #\space)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Aux procedures
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; Converts print arguments to be correctly printed
  (define (arg->string arg)
    (format "~a"
            (cond
              [(and (boolean? arg) arg) "true"]
              [(and (boolean? arg) (not arg)) "false"]
              [(void? arg)  ""] 
              [(not (null? arg)) arg]
              [else ""])))

  ;;; Given a list of strings generates a string seprated by char
  (define (build-seperated-string lst char)
    (cond 
      [(null? lst) ""]
      [(eq? (length lst) 1) (car lst)]
      [else (format "~a~a~a" 
                    (car lst) 
                    char
                    (build-seperated-string (cdr lst) char))]))
  )
