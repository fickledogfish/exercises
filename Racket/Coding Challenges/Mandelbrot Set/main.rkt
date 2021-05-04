#lang racket

(require (only-in 2htdp/image
                  color
                  color-list->bitmap
                  scale
                  save-image))

(define mandelbrot%
  (class object%
    (init-field width height)

    (define img (for/list ([i (* width height)])
                  (color 0 0 0 255)))

    (define/public (get-bitmap)
      (color-list->bitmap img width height))

    (super-new)))

(define (save mandelbrot-image filename)
  (let ([mandelbrot-bitmap (send mandelbrot-image get-bitmap)]
        [width (get-field width mandelbrot-image)]
        [height (get-field height mandelbrot-image)])
    (save-image
     mandelbrot-bitmap
     (string-append filename ".png"))))

(save (instantiate mandelbrot% (100 100)) "image")
