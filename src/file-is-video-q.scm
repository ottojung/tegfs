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

%var file-is-video?

(define (file-is-video? target-fullpath)
  (or (string-suffix? ".mp4" target-fullpath)
      (string-suffix? ".avi" target-fullpath)
      (string-suffix? ".mkv" target-fullpath)
      (string-suffix? ".mpg" target-fullpath)
      (string-suffix? ".webm" target-fullpath)))
