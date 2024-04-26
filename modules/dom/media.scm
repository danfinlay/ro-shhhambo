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
;;; HTMLMediaElement bindings.
;;;
;;; Code:

(library (dom media)
  (export make-audio
          media-play
          media-pause
          media-volume
          set-media-volume!
          set-media-loop!
          media-seek)
  (import (scheme base)
          (hoot ffi)
          (hoot match)
          (only (hoot syntax) define*))

  (define-foreign make-audio
    "media" "newAudio"
    (ref string) -> (ref extern))
  (define-foreign media-play
    "media" "play"
    (ref extern) -> none)
  (define-foreign media-pause
    "media" "pause"
    (ref extern) -> none)
  (define-foreign media-volume
    "media" "volume"
    (ref extern) -> f64)
  (define-foreign set-media-volume!
    "media" "setVolume"
    (ref extern) f64 -> none)
  (define-foreign set-media-loop!
    "media" "setLoop"
    (ref extern) i32 -> none)
  (define-foreign media-seek
    "media" "seek"
    (ref extern) f64 -> none))
