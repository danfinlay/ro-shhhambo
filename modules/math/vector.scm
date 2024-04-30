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
;;; Vectors, in the linear algebra sense.
;;;
;;; Code:

(define-module (math vector)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme inexact)
  #:use-module ((hoot bytevectors)
                #:select
                (bytevector-ieee-double-native-ref
                 bytevector-ieee-double-native-set!))
  #:use-module (math)
  #:export (vec2
            vec2?
            vec2-x
            vec2-y
            set-vec2-x!
            set-vec2-y!
            vec2-add!
            vec2-sub!
            vec2-mul-scalar!
            vec2-magnitude
            vec2-normalize!
            vec2-clamp!))

;; For speed, a vec2 is a wrapper around a bytevector so that we can
;; use unboxed floats.
(define-record-type <vec2>
  (make-vec2 bv)
  vec2?
  (bv vec2-bv))

(define f64-ref  bytevector-ieee-double-native-ref)
(define f64-set! bytevector-ieee-double-native-set!)

(define (vec2 x y)
  (let ((v (make-vec2 (make-bytevector 16))))
    (set-vec2-x! v x)
    (set-vec2-y! v y)
    v))

(define (vec2-x v)
  (f64-ref (vec2-bv v) 0))

(define (vec2-y v)
  (f64-ref (vec2-bv v) 8))

(define (set-vec2-x! v x)
  (f64-set! (vec2-bv v) 0 x))

(define (set-vec2-y! v y)
  (f64-set! (vec2-bv v) 8 y))

(define (vec2-add! v w)
  (set-vec2-x! v (+ (vec2-x v) (vec2-x w)))
  (set-vec2-y! v (+ (vec2-y v) (vec2-y w))))

(define (vec2-sub! v w)
  (set-vec2-x! v (- (vec2-x v) (vec2-x w)))
  (set-vec2-y! v (- (vec2-y v) (vec2-y w))))

(define (vec2-mul-scalar! v x)
  (set-vec2-x! v (* (vec2-x v) x))
  (set-vec2-y! v (* (vec2-y v) x)))

(define (vec2-magnitude v)
  (sqrt (+ (* (vec2-x v) (vec2-x v)) (* (vec2-y v) (vec2-y v)))))

(define (vec2-normalize! v)
  (unless (and (= (vec2-x v) 0.0) (= (vec2-y v) 0.0))
    (let ((m (vec2-magnitude v)))
      (set-vec2-x! v (/ (vec2-x v) m))
      (set-vec2-y! v (/ (vec2-y v) m)))))

(define (vec2-clamp! v xmin ymin xmax ymax)
  (set-vec2-x! v (clamp (vec2-x v) xmin xmax))
  (set-vec2-y! v (clamp (vec2-y v) ymin ymax)))
