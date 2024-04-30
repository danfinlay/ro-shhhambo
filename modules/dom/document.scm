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
;;; Document bindings.
;;;
;;; Code:

(define-module (dom document)
  #:pure
  #:use-module (scheme base)
  #:use-module (hoot ffi)
  #:export (current-document
            document-body
            get-element-by-id
            make-text-node
            make-element))

(define-foreign current-document
  "document" "get"
  -> (ref extern))
(define-foreign document-body
  "document" "body"
  -> (ref null extern))
(define-foreign get-element-by-id
  "document" "getElementById"
  (ref string) -> (ref null extern))
(define-foreign make-text-node
  "document" "createTextNode"
  (ref string) -> (ref extern))
(define-foreign make-element
  "document" "createElement"
  (ref string) -> (ref extern))
