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

%run guile

%var get-file-type

%use (a-weblink?) "./a-weblink-q.scm"
%use (file-is-audio?) "./file-is-audio-q.scm"
%use (file-is-image?) "./file-is-image-q.scm"
%use (file-is-text?) "./file-is-text-q.scm"
%use (file-is-video?) "./file-is-video-q.scm"

(define (get-file-type filepath)
  (cond
   ((a-weblink? filepath) 'weblink)
   ((file-is-image? filepath) 'image)
   ((file-is-video? filepath) 'video)
   ((file-is-audio? filepath) 'audio)
   ((file-is-text? filepath) 'text)
   (else #f)))
