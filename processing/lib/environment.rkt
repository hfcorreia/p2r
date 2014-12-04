(module processing-api/environment racket

  (provide (all-defined-out))

  (require  (planet aml/rosetta:1:=50)
            racket/system)

  (define-syntax-rule 
    (todo msg)
    (raise-syntax-error 'todo msg))
                      
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Rosetta Configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; Generates a pdf using the tikz backend
  (define (generateTikz [file-name "tmp"] [scale 1] [pdf-viewer "evince"])
    (define (tikz->tex str out)
      (let ((scale (* scale 0.024)))
        (fprintf out
                 "\\documentclass[]{article}\n\\usepackage{tikz}\n\\begin{document}\n\\begin{tikzpicture}[yscale=-~a,xscale=~a]\n~a\n\\end{tikzpicture}\n\\end{document}"
                 scale scale str)))
    (define (display-tikz-to-string)
      (let ([output-port (open-output-string)])
        (parameterize ([current-output-port output-port])
          (display-tikz))
        (get-output-string output-port)))

    (define out 
      (open-output-file (string-append file-name ".tex") #:exists 'replace))

    (tikz->tex (display-tikz-to-string) out)
    (close-output-port out)
    (system (string-append "pdflatex " file-name ".tex"))
    (system (string-append pdf-viewer " " file-name ".pdf"))
    (system "rm *.tex *.log *.aux -f"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Environment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; System variable that stores the width of the entire screen display. 
  ;;; This is used to run a full-screen program on any display size. 
  (define displayWidth 0)
  

  ;;; System variable that stores the height of the entire screen display. 
  ;;; This is used to run a full-screen program on any display size. 
  (define displayHeight 0)

  ;;; System variable that stores the width of the display window.
  (define width 100)

  ;;; System variable that stores the height of the display window.
  (define height 100)

  ;;; Confirms if a Processing program is "focused," meaning that it is active and
  ;;; will accept mouse or keyboard input. 
  ;;; This variable is "true" if it is focused and "false" if not.
  (define focused #f)

  ;;; The system variable frameCount contains the number of frames that have
  ;;; been displayed since the program started. Inside setup() the value is 0, after
  ;;; the first iteration of draw it is 1, etc. 
  (define frameCount 0)

  ;;; The system variable frameRate contains the approximate frame rate of a
  ;;; running sketch. The initial value is 10 fps and is updated with each frame.
  ;;; The value is averaged over several frames, and so will only be accurate after
  ;;; the draw function has run 5-10 times. 
  (define frameRateVar 10)

  (define-syntax frameRate
    (syntax-rules ()
      [(_) frameRateVar]
      [(_ fps) (todo "frameRate: Not implemented!")]))

  (define-syntax size 
    (syntax-rules ()
      [(_ w h)
       (begin 
         (set! width w)
         (set! height h))]
      [(_ w h render)
       (todo "size: Not implemented")]))

  (define-syntax cursor 
    (syntax-rules ()
      [(_)         (todo "cursor: Not implemented")]
      [(_ img)     (todo "cursor: Not implemented")]
      [(_ img x y) (todo "cursor: Not implemented")]))

  (define (noCursor)
    (todo "noCursor: Not implemented!"))

  )
