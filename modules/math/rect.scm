;;; Copyright (C) 2024 David Thompson <dave@spritely.institute>
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Commentary:
;;;
;;; Rectangle data type.
;;;
;;; Code:

(define-module (math rect)
  #:pure
  #:use-module (scheme base)
  #:use-module ((hoot bytevectors)
                #:select
                (bytevector-ieee-double-native-ref
                 bytevector-ieee-double-native-set!))
  #:export (make-rect
            rect?
            rect-x
            rect-y
            rect-width
            rect-height
            set-rect-x!
            set-rect-y!
            set-rect-width!
            set-rect-height!
            rect-intersects?
            rect-clip))

;; For speed, a rect is a wrapper around a bytevector so that we can
;; use unboxed floats.
(define-record-type <rect>
  (%make-rect bv)
  rect?
  (bv rect-bv))

(define f64-ref  bytevector-ieee-double-native-ref)
(define f64-set! bytevector-ieee-double-native-set!)

(define (make-rect x y w h)
  (let ((bv (make-bytevector (* 8 4))))
    (f64-set! bv 0 x)
    (f64-set! bv 8 y)
    (f64-set! bv 16 w)
    (f64-set! bv 24 h)
    (%make-rect bv)))

(define (rect-x r)
  (f64-ref (rect-bv r) 0))

(define (rect-y r)
  (f64-ref (rect-bv r) 8))

(define (rect-width r)
  (f64-ref (rect-bv r) 16))

(define (rect-height r)
  (f64-ref (rect-bv r) 24))

(define (set-rect-x! r x)
  (f64-set! (rect-bv r) 0 x))

(define (set-rect-y! r y)
  (f64-set! (rect-bv r) 8 y))

(define (set-rect-width! r width)
  (f64-set! (rect-bv r) 16 width))

(define (set-rect-height! r height)
  (f64-set! (rect-bv r) 24 height))

(define (rect-intersects? a b)
  (and (< (rect-x a) (+ (rect-x b) (rect-width b)))
       (< (rect-y a) (+ (rect-y b) (rect-height b)))
       (> (+ (rect-x a) (rect-width a)) (rect-x b))
       (> (+ (rect-y a) (rect-height a)) (rect-y b))))

(define (rect-clip a b)
  (let* ((x1 (max (rect-x a) (rect-x b)))
         (x2 (min (+ (rect-x a) (rect-width a))
                  (+ (rect-x b) (rect-width b))))
         (y1 (max (rect-y a) (rect-y b)))
         (y2 (min (+ (rect-y a) (rect-height a))
                  (+ (rect-y b) (rect-height b)))))
    (make-rect x1 y1 (- x2 x1) (- y2 y1))))
