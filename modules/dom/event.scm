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
;;; EventTarget and Event bindings.
;;;
;;; Code:

(define-module (dom event)
  #:pure
  #:use-module (scheme base)
  #:use-module (hoot ffi)
  #:export (add-event-listener!
            remove-event-listener!
            prevent-default!
            keyboard-event-code))

;; EventTarget
(define-foreign add-event-listener!
  "event" "addEventListener"
  (ref extern) (ref string) (ref extern) -> none)
(define-foreign remove-event-listener!
  "event" "removeEventListener"
  (ref extern) (ref string) (ref extern) -> none)

;; Event
(define-foreign prevent-default!
  "event" "preventDefault"
  (ref extern) -> none)

;; KeyboardEvent
(define-foreign keyboard-event-code
  "event" "keyboardCode"
  (ref extern) -> (ref string))
