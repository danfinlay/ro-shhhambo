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
;;; Element bindings.
;;;
;;; Code:

(define-module (dom element)
  #:pure
  #:use-module (scheme base)
  #:use-module (hoot ffi)
  #:export (element-value
            set-element-value!
            element-width set-element-width!
            element-height set-element-height!
            append-child!
            remove!
            replace-with!
            set-attribute!
            remove-attribute!
            clone-element))

(define-foreign element-value
  "element" "value"
  (ref extern) -> (ref string))
(define-foreign set-element-value!
  "element" "setValue"
  (ref extern) (ref string) -> none)
(define-foreign element-width
  "element" "width"
  (ref extern) -> i32)
(define-foreign element-height
  "element" "height"
  (ref extern) -> i32)
(define-foreign set-element-width!
  "element" "setWidth"
  (ref extern) i32 -> none)
(define-foreign set-element-height!
  "element" "setHeight"
  (ref extern) i32 -> none)
(define-foreign append-child!
  "element" "appendChild"
  (ref extern) (ref extern) -> (ref extern))
(define-foreign remove!
  "element" "remove"
  (ref extern) -> none)
(define-foreign replace-with!
  "element" "replaceWith"
  (ref extern) (ref extern) -> none)
(define-foreign set-attribute!
  "element" "setAttribute"
  (ref extern) (ref string) (ref string) -> none)
(define-foreign remove-attribute!
  "element" "removeAttribute"
  (ref extern) (ref string) -> none)
(define-foreign clone-element
  "element" "clone"
  (ref extern) -> (ref extern))
