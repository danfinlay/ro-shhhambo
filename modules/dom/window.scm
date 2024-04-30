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
;;; Window bindings.
;;;
;;; Code:

(define-module (dom window)
  #:pure
  #:use-module (scheme base)
  #:use-module (hoot ffi)
  #:export (current-window
            window-inner-width
            window-inner-height
            request-animation-frame
            timeout))

(define-foreign current-window
  "window" "get"
  -> (ref extern))
(define-foreign window-inner-width
  "window" "innerWidth"
  (ref extern) -> i32)
(define-foreign window-inner-height
  "window" "innerHeight"
  (ref extern) -> i32)
(define-foreign request-animation-frame
  "window" "requestAnimationFrame"
  (ref extern) -> none)
(define-foreign timeout
  "window" "setTimeout"
  (ref extern) f64 -> i32)
