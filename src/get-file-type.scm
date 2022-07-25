;;;; Copyright (C) 2022  Otto Jung
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; version 3 of the License.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

%run guile

%var get-file-type

%use (file-is-image?) "./file-is-image-q.scm"
%use (file-is-video?) "./file-is-video-q.scm"
%use (file-is-text?) "./file-is-text-q.scm"
%use (file-is-audio?) "./file-is-audio-q.scm"
%use (a-weblink?) "./a-weblink-q.scm"

(define (get-file-type filepath)
  (cond
   ((a-weblink? filepath) 'weblink)
   ((file-is-image? filepath) 'image)
   ((file-is-video? filepath) 'video)
   ((file-is-audio? filepath) 'audio)
   ((file-is-text? filepath) 'text)
   (else #f)))
