#lang racket

(require (planet aml/rosetta)
         racket/system)

(provide generateTikz)

(define (tikz->tex str out)
  (fprintf out
    "\\documentclass[]{standalone}
    \\usepackage{tikz}
    \\begin{document}
    \\begin{tikzpicture}
    ~a
    \\end{tikzpicture}
    \\end{document}" str))

(define (generateTikz file-name)
  (define out 
    (open-output-file (string-append file-name ".tex") #:exists 'replace))
  (tikz->tex (display-tikz-to-string) out)
  (close-output-port out)
  (system (string-append "pdflatex " file-name ".tex"))
  (system (string-append "evince " file-name ".pdf")))


(define (display-tikz-to-string)
    (let ([output-port (open-output-string)])
          (parameterize ([current-output-port output-port])
                  (display-tikz))
              (get-output-string output-port)))
