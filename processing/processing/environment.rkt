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
(define/types Object : rhinocerous ros-rhino5)

(define/types (foo [int i] [int j] -> Object) (displayln j))


(define/types (backend [Object id] -> Object)
  (backend  id)
  (white-renders))


(define/types (render [Object f] [Object p1] [Object p2] [Object t]  -> void)
   (render-dir "C:\\Users\\hugo\\render")
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
 |
 |;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 |;;; Environment
 |;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 |
 |;;; System variable that stores the width of the entire screen display.
 |;;; This is used to run a full-screen program on any display size.
 |(define displayWidth 0)
 |
 |
 |;;; System variable that stores the height of the entire screen display.
 |;;; This is used to run a full-screen program on any display size.
 |(define displayHeight 0)
 |
 |;;; System variable that stores the width of the display window.
 |(define width 100)
 |
 |;;; System variable that stores the height of the display window.
 |(define height 100)
 |
 |;;; Confirms if a Processing program is "focused," meaning that it is active and
 |;;; will accept mouse or keyboard input.
 |;;; This variable is "true" if it is focused and "false" if not.
 |(define focused #f)
 |
 |;;; The system variable frameCount contains the number of frames that have
 |;;; been displayed since the program started. Inside setup() the value is 0, after
 |;;; the first iteration of draw it is 1, etc.
 |(define frameCount 0)
 |
 |;;; The system variable frameRate contains the approximate frame rate of a
 |;;; running sketch. The initial value is 10 fps and is updated with each frame.
 |;;; The value is averaged over several frames, and so will only be accurate after
 |;;; the draw function has run 5-10 times.
 |(define frameRateVar 10)
 |
 |(define-syntax frameRate
 |  (syntax-rules ()
 |    [(_) frameRateVar]
 |    [(_ fps) (error "frameRate: Not implemented!")]))
 |
 |(define (size w h)
 |  (set! width w)
 |  (set! height h))
 |
 |(define-syntax cursor
 |  (syntax-rules ()
 |    [(_)         (error "cursor: Not implemented")]
 |    [(_ img)     (error "cursor: Not implemented")]
 |    [(_ img x y) (error "cursor: Not implemented")]))
 |
 |(define (noCursor)
 |  (error "noCursor: Not implemented!"))
 |#
