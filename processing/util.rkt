#lang racket

(provide (all-defined-out))

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

;;; mangle-function-id symbol (list/of symbols) -> symbol
;;; Mangles a function identifier using argument type information
(define (mangle-function-id id args)
  (string->symbol
    (string-append*
      `(,(symbol->string id)
         "-"
         ,(if (not (null? args))
            (string-append* (append
                              (map (lambda (x)
                                   (string (char-upcase (string-ref
                                                          (symbol->string x)
                                                          0))))
                                 args)
                            (list "-fn")))
            "fn")))))

;;; Given a list of strings generates a string seprated by char
(define (build-seperated-string lst char)
  (cond
    [(null? lst) ""]
    [(eq? (length lst) 1) (car lst)]
    [else (format "~a~a~a"
                  (car lst)
                  char
                  (build-seperated-string (cdr lst) char))]))
