#!/usr/bin/env -S guile -s
!#
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(use-modules (ice-9 binary-ports)
             (ice-9 textual-ports)
             (rnrs bytevectors)
             (srfi srfi-1)
             (srfi srfi-27)
             (raylib))

(define WIDTH 100)
(define HEIGHT 100)

(define (write-ppm data name)
  (call-with-output-file name
    (lambda (port)
      (put-string port (format #f "P6~%~a ~a 255~%" WIDTH HEIGHT))
      (put-bytevector port data)))
    #:binary #t)

(define (img->pixel-bytevector img)
  (define bv (make-bytevector (* WIDTH HEIGHT 3)))
  (do ((y 0 (1+ y))) ((= y HEIGHT))
    (do ((x 0 (1+ x))) ((= x WIDTH))
      (let* ((p (GetImageColor img x y))
             (i (* 3 (+ (* y WIDTH) x))))
        (bytevector-u8-set! bv i       (Color-r p))
        (bytevector-u8-set! bv (+ i 1) (Color-g p))
        (bytevector-u8-set! bv (+ i 2) (Color-b p)))))
  bv)

(define (image-write-ppm img name)
  (write-ppm (img->pixel-bytevector img) name))

(define (draw-random-rectangle img)
  (define px (random-integer WIDTH))
  (define py (random-integer HEIGHT))
  (define width (random-integer (- WIDTH px)))
  (define height (random-integer (- HEIGHT py)))
  (ImageDrawRectangle img px py width height WHITE))

(define (draw-random-circle img)
  (define px (random-integer WIDTH))
  (define py (random-integer HEIGHT))
  (define rad (* (random-real) (if (< WIDTH HEIGHT) (- WIDTH px) (- HEIGHT py))))
  (ImageDrawCircle img px py (inexact->exact (truncate rad)) WHITE))

(define (generage-training-data)
  (define img (GenImageColor WIDTH HEIGHT BLACK))
  (do ((i 0 (1+ i))) ((= i 10))
    (set! img (GenImageColor WIDTH HEIGHT BLACK))
    (draw-random-rectangle img)
    (image-write-ppm img (format #f "shapes/rect-~a.ppm" i))

    (set! img (GenImageColor WIDTH HEIGHT BLACK))
    (draw-random-circle img)
    (image-write-ppm img (format #f "shapes/circle-~a.ppm" i))))

(define (main argv)
  (format #t "~a~%" argv)
  (InitWindow WIDTH HEIGHT "Perceptron")
  (define img (GenImageColor WIDTH HEIGHT BLACK))
  (CloseWindow)
  )

(main (command-line))
