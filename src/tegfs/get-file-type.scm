;;;; Copyright (C) 2022  Otto Jung
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Affero General Public License as published
;;;; by the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Affero General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Affero General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(cond-expand
 (guile
  (define-module (tegfs get-file-type)
    :export (get-file-type)
    :use-module ((tegfs a-weblink-q) :select (a-weblink?))
    :use-module ((tegfs file-is-audio-q) :select (file-is-audio?))
    :use-module ((tegfs file-is-image-q) :select (file-is-image?))
    :use-module ((tegfs file-is-text-q) :select (file-is-text?))
    :use-module ((tegfs file-is-video-q) :select (file-is-video?))
    )))



(define (get-file-type filepath)
  (cond
   ((a-weblink? filepath) 'weblink)
   ((file-is-image? filepath) 'image)
   ((file-is-video? filepath) 'video)
   ((file-is-audio? filepath) 'audio)
   ((file-is-text? filepath) 'text)
   (else #f)))
