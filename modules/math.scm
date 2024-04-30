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
;;; Helpful math things.
;;;
;;; Code:

;; (library (math)
;;   (export random
;;           clamp)
;;   (import (scheme base)
;;           (hoot ffi))

;;   (define-foreign random
;;     "math" "random"
;;     -> f64)

;;   (define (clamp x min max)
;;     (cond ((< x min) min)
;;           ((> x max) max)
;;           (else x))))

(define-module (math)
  #:pure
  #:use-module (scheme base)
  #:use-module (hoot ffi)
  #:export (random clamp))

(define-foreign random
  "math" "random"
  -> f64)

(define (clamp x min max)
  (cond ((< x min) min)
        ((> x max) max)
        (else x)))
