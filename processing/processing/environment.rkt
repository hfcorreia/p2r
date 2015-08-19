#lang racket/base

(provide (all-defined-out))

(require (rename-in (planet aml/rosetta:1:54)
                    [autocad ros-autocad]
                    [rhino5 ros-rhino5])
         (for-syntax "runtime-bindings.rkt")
         "runtime-bindings.rkt"
         racket/system)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rosetta Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define/types Object : autocad ros-autocad)
(define/types Object : rhino5 ros-rhino5)

(define/types (backend [Object id] -> Object)
  (backend  id))

(define/types (render [Object f] [Object p1] [Object p2] [Object t]  -> void)
   (view-with-background p1 p2 t)
   (render-view f))

(define/types (view-expression -> void)
  (view-expression))

(define/types (view [Object p1] [Object p2] [Object t] -> void)
  (view p1 p2 t))

(define/types (delete -> void)
  (delete-all-shapes))

(define/types (render-size [int w] [int h] -> void)
  (render-size w h))

(define/types (render-dir [String dir] -> void)
  (render-dir dir))

#|;;; Generates a pdf using the tikz backend
 |(define (generateTikz [file-name "tmp"] [scale 1] [pdf-viewer "evince"])
 |  (define (tikz->tex str out)
 |    (let ((scale (* scale 0.024)))
 |      (fprintf out
 |               "\\documentclass[]{article}\n\\usepackage{tikz}\n\\begin{document}\n\\begin{tikzpicture}[yscale=-~a,xscale=~a]\n~a\n\\end{tikzpicture}\n\\end{document}"
 |               scale scale str)))
 |  (define (display-tikz-to-string)
 |    (let ([output-port (open-output-string)])
 |      (parameterize ([current-output-port output-port])
 |        (display-tikz))
 |      (get-output-string output-port)))
 |
 |  (define out
 |    (open-output-file (string-append file-name ".tex") #:exists 'replace))
 |
 |  (tikz->tex (display-tikz-to-string) out)
 |  (close-output-port out)
 |  (system (string-append "pdflatex " file-name ".tex"))
 |  (system (string-append pdf-viewer " " file-name ".pdf"))
 |  (system "rm *.tex *.log *.aux -f"))
 |#
